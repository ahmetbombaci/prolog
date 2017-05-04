:- module(ttr, [c/2, connected/6, ticket/4, readCardSlot/2, cityCoord/3, getPlayerList/1]).

:- include(map).
:- include(tickets).
:- include(longestroad).
:- include(deck).
:- include(playergraph).
:- include(players).
:- include(gui).

%:- initialization(runGame([beatthemall,beatthemallv10])).
:- initialization(runGame([beatthemall,p2])).

:- dynamic turn/1.
turn(0).

c(0, 0).
c(1, 1).
c(2, 2).
c(3, 4).
c(4, 7).
c(5, 10).
c(6, 15).

getPlayerList(PlayerList) :-
	setof(Player, A^B^player(Player, A, B), PlayerList).
	
% --------------------------------
% Game end methods
% --------------------------------	

:- dynamic endGameTrigger/1.
endGameTrigger(false).

endGameCheck(Player) :- 
	readPlayerWagonCount(Player, PlayerWagonCount),
	PlayerWagonCount < 3,
	retract(endGameTrigger(_)),
	assert(endGameTrigger(true))
	;
	true.
	
finalizeTicketScores(Player) :-
	readPlayerTicketCards(Player, PlayerTicketCards),
	playerGraph(Player, Graph, dist),
	findall(Score, 
				(
					member(Ticket, PlayerTicketCards),
					ticket(Ticket, From, To, Score0),
					dijkstra(Graph, From, To, Distance, _),
					(Distance == 0 -> Score is Score0; Score is -Score0)
				),
			TicketScores
	),
	sumlist(TicketScores, TicketScoresSum),
	updatePlayerScore(Player, TicketScoresSum).
	
% --------------------------------
% Run game methods
% --------------------------------	
	
runGame(PlayerModules) :- 
	initializeTrainCarDeck,
	initializeTicketDeck,
	initializePlayers(PlayerModules),
	fillAllCardSlots,
	getPlayerList(PlayerList),
	forall( member(Player, PlayerList),
		(
			forall( between(1, 4, _),
				(
					assignCardAndAnnounce(Player, 0, _, _)
				)
			),
			actionSelectTicket(Player, 2)
		)
	),
	initializeBoard,
	mainLoop(PlayerList),nl,
	!.
	
displayScores(Leader, PlayerScores) :-
	getPlayerList(PlayerList),	
	findall(Score-Player,
		(
			member(Player, PlayerList),
			player(Player, score, Score)
		),
		PlayerScores
	),
	write('Scores: '),write(PlayerScores),nl,
	keysort(PlayerScores, PlayerScoresSorted),
	reverse(PlayerScoresSorted, PlayerScoresSortedReversed),
	[_-Leader|_] = PlayerScoresSortedReversed.
	
finalizeScores :-
	getPlayerList(PlayerList),
	forall( member(Player, PlayerList),
		finalizeTicketScores(Player)
	),
	longestroadPlayers(LongestRoadPlayers),
	forall( member(LRPlayer, LongestRoadPlayers), 
		updatePlayerScore(LRPlayer, 10)
	).
	
mainLoop([]) :- 
	finalizeScores,!,
	display('*******************************'), nl,
	display('Game ended'), nl,
	displayScores(Winner, _),
	write('Winner: '),write(Winner),nl,
	turn(T0),
	write('Number of turns: '),write(T0),nl.
	
mainLoop([Player|RemainingPlayerList]) :- 
	turn(T0),
	T1 is T0 + 1,
	retractall(turn(_)),
	assert(turn(T1)),
	write('*** '),write(Player),write(' ***'),nl,
	doAction(Player),
	(
		endGameTrigger(IsEndGameTriggered),
		\+IsEndGameTriggered,
		append(RemainingPlayerList, [Player], NewPlayerList),
		endGameCheck(Player)
		;
		NewPlayerList = RemainingPlayerList
	),
	refreshGUI,
	!,
	mainLoop(NewPlayerList).
	
% --------------------------------
% Action selection methods
% --------------------------------	
	
availableAction(build, InputActionList, ActionList) :-	
	findall(C, connected(C,_,_,_,_,empty), Cs),
	length(Cs, 0),
	ActionList = InputActionList
	;
	append(InputActionList, [build], ActionList).
	
availableAction(ticket, InputActionList, ActionList) :-	
	ticketDeck(TicketDeck),
	length(TicketDeck, 0),
	ActionList = InputActionList
	;
	append(InputActionList, [ticket], ActionList).
	
availableAction(card, InputActionList, ActionList) :-	
	findall(C, cardSlot(C,empty), Cs),
	length(Cs, 5),
	trainCarDeck(TrainCarDeck), 
	length(TrainCarDeck, 0),
	trainCarDeckDiscardPile(TrainCarDeckDiscardPile),
	length(TrainCarDeckDiscardPile, 0),
	ActionList = InputActionList
	;
	append(InputActionList, [card], ActionList).
	
availableActions(ActionList) :-
	availableAction(build, [], L),
	availableAction(ticket, L, L2),
	availableAction(card, L2, ActionList).

doAction(Player) :-
	getPlayerModule(Player, Module),
	availableActions(ActionList),
	catch( call_with_time_limit(100,(	
										Module:decideAction(ActionList, Action),
										write('Action: '),write(Action),nl,
										doAction(Player, Action)
										;
										true
									)
			),
			time_limit_exceeded,
			announceTimeOut(Player)
	).
	
doAction(Player, card) :- actionDrawCard(Player).
doAction(Player, ticket) :- actionSelectTicket(Player).
doAction(Player, build) :- actionClaimRoute(Player).

announceTimeOut(TimeOutPlayer) :-
	forall( (getPlayerModule(_, Module)),
		(
			Module:announceAction(timeout, TimeOutPlayer)
			;
			true
		)
	).		
	
% --------------------------------
% Claim route methods
% --------------------------------	

actionClaimRoute(Player) :-
	getPlayerModule(Player, Module), 
	Module:buildRoad(City1, City2, CardList),
	buildRoad(City1, City2, Player, CardList, Coordinates),
	playerColor(Player, PlayerColor),
	buildRoadGUI(PlayerColor, Coordinates),
	announceBuildRouteResult(Player,true,City1, City2, CardList)
	;
	announceBuildRouteResult(Player,false,City1, City2, CardList).
	
buildRoad(City1, City2, Player, CardList, Coordinates) :- 
	readPlayerTrainCarCards(Player, PlayerTrainCarCards),
	subset(CardList, PlayerTrainCarCards),
	connected(City1, City2, Length, PathColor, Coordinates, empty), 
	length(CardList, L), L=:=Length,
	readPlayerWagonCount(Player, PlayerWagonCount),
	PlayerWagonCount >= Length,
	sort(CardList,CardSet), length(CardSet, L2),
	(
		L2 == 1,
		member(CardColor, CardSet),
		(
			PathColor == gray
			;
			PathColor == CardColor
			;
			CardColor == joker
		)
		;
		L2 == 2,
		member(joker, CardSet),
		member(CardColor, CardSet),
		CardColor \= joker,
		(
			PathColor == gray
			;
			PathColor == CardColor
		)
	),
	useTrainCarCards(Player, CardList),
	usePlayerWagons(Player, Length),
	discardTrainCarCards(CardList),
	(
		(
			retract(path(City1,City2,Length,PathColor,Coordinates,empty)),
			assert(path(City1,City2,Length,PathColor,Coordinates,Player))
		)
		;
		(
			retract(path(City2,City1,Length,PathColor,Coordinates,empty)),
			assert(path(City2,City1,Length,PathColor,Coordinates,Player))
		)
	),
	c(Length,RoadScore),
	updatePlayerScore(Player, RoadScore).	
	
announceBuildRouteResult(Player, Success, City1, City2, CardList) :-
	getPlayerModule(Player, Module),
	(
		Module:buildRoadResult(Success)
		;
		true
	),
	Success,
	forall( (getPlayerModule(OtherPlayer, OtherPlayerModule), OtherPlayer \= Player),
		(
			OtherPlayerModule:announceAction(buildroad, Player, City1, City2, CardList)
			;
			true
		)
	)
	;
	true.
	
% --------------------------------
% Ticket select methods
% --------------------------------	

actionSelectTicket(Player) :- 
	actionSelectTicket(Player, 1).
	
actionSelectTicket(Player, MinTicketCount) :-
	drawTicketCard(Ticket1),
	drawTicketCard(Ticket2),
	drawTicketCard(Ticket3),
	append([Ticket1,Ticket2,Ticket3], Tickets),
	length(Tickets, SentTicketCount),
	getPlayerModule(Player, Module), 
	(
		SentTicketCount \= 0,
		(
			Module:selectTickets(Tickets, ST),
			sort(ST, SelectedTickets),
			length(SelectedTickets, L), L >= MinTicketCount,
			%subset(SelectedTickets, Tickets),
			subtractlist(Tickets, SelectedTickets, NonSelectedTickets),
			returnTicketCards(NonSelectedTickets),
			assignTicketCards(Player, SelectedTickets),
			announceSelectTicketResult(Player, SelectedTickets)
		)
		;
		assignTicketCards(Player, Tickets),
		announceSelectTicketResult(Player, Tickets)
	).

announceSelectTicketResult(Player, Tickets) :-
	getPlayerModule(Player, Module),
	(
		Module:selectTicketResult(Tickets)
		;
		true
	),
	length(Tickets, TicketCount),
	forall( (getPlayerModule(OtherPlayer, OtherPlayerModule), OtherPlayer \= Player),
		(
			OtherPlayerModule:announceAction(selectticket, Player, TicketCount)
			;
			true
		)
	)
	;
	true.
	
% --------------------------------
% Train car card draw methods
% --------------------------------	

actionDrawCard(Player) :- 
	getPlayerModule(Player, Module), 
	(Module:drawCard(SlotNo), assignCardAndAnnounce(Player, SlotNo, Card, 1)),
	refreshGUI,
	(
		(SlotNo \= 0, Card==joker)
		;
		actionDrawCard(Player,2)
	)
	;
	true.
	
actionDrawCard(Player,TryCount) :-  
	getPlayerModule(Player, Module),	
	(
		TryCount<3,
		Module:drawCard(SlotNo),
		(
			assignCardAndAnnounce(Player, SlotNo, _, 2)
			;
			actionDrawCard(Player,TryCount+1)
		)
		;
		announceDrawCardResult(Player, none, -1)
	),
	refreshGUI.
	

assignCardAndAnnounce(Player, 0, Card, _) :- 
	drawTrainCarCard(Card), 
	assignTrainCarCard(Player, Card),
	announceDrawCardResult(Player, Card, 0).

assignCardAndAnnounce(Player, SlotNo, Card, 1) :- 
	SlotNo > 0, SlotNo =<5,
	readCardSlot(SlotNo, Card), 
	assignTrainCarCard(Player, Card),
	announceDrawCardResult(Player, Card, SlotNo),
	fillCardSlot(SlotNo).

assignCardAndAnnounce(Player, SlotNo, Card, 2) :- 
	SlotNo > 0, SlotNo =<5,
	readCardSlot(SlotNo, Card), 
	Card \= joker,
	assignTrainCarCard(Player, Card),
	announceDrawCardResult(Player, Card, SlotNo),
	fillCardSlot(SlotNo).	

announceDrawCardResult(Player, Card, SlotNo) :-
	write(SlotNo),write(': '),write(Card),nl,
	getPlayerModule(Player, Module),
	(
		Module:drawCardResult(Card)
		;
		true
	),
	(SlotNo == 0, AnnouncedCard = closed ; AnnouncedCard = Card),
	forall( (getPlayerModule(OtherPlayer, OtherPlayerModule), OtherPlayer \= Player),
		(
			OtherPlayerModule:announceAction(drawcard, Player, AnnouncedCard)
			;
			true
		)
	)
	;
	true.
