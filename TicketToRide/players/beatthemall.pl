:- module(beatthemall, []).


:- dynamic difficultyValues/2.
:- dynamic myPlayer/1.
:- dynamic myDestinationTicket/5.
:- dynamic myWagonCard/2.
:- dynamic isGameJustStarted/1.
:- dynamic drawCardCardType/1.
:- dynamic buildRoadCardList/3.
:- dynamic remainingWagonCount/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


initialize(PlayerName, PlayerCount) :- 
	assert(myPlayer(PlayerName)), assert(isGameJustStarted(yes)),
	assert(myWagonCard(yellow,0)),assert(myWagonCard(green,0)),assert(myWagonCard(purple,0)),assert(myWagonCard(blue,0)),
	assert(myWagonCard(orange,0)),assert(myWagonCard(white,0)),assert(myWagonCard(red,0)),assert(myWagonCard(black,0)),assert(myWagonCard(joker,0)), 
	assert(remainingWagonCount(45)),
	debug_log('--------------------------------------GAME STARTED---------------------------------------').

	

%get ticket which has the highest score
%check whether it is possible to complete route or not
%if route is not possible, skip ticket, if there is no remaining ticket, request another ticket
%if route is possible, get the component which has the highest risk and do required action for that component (built road or draw wagon card)
decideAction(ActionList, Action) :- 
	(
		findall((Color,ColorCount),myWagonCard(Color,ColorCount),L),debug_log('Wagon Cards are: '), debug_log(L),
		findHighestTicket(TicketNo), 
		debug_log('Ticket No with highest value is '), debug_log(TicketNo),
		ticket(TicketNo, From, To, Score), 
		(	
			(
				find_all_claim_route_list(From, To, RouteList),  
				debug_log(RouteList),
				(
					(				
						find_max_risk_route(RouteList, TargetFrom, TargetTo, _),
						debug_log(TargetFrom),debug_log(TargetTo),
						perform_action(TargetFrom,TargetTo,Action),
						!
					);
					(
						% ticket is completed so mark it and check next one
						debug_log('Destination ticket is completed'),
						retract(myDestinationTicket(TicketNo,From,To,Score,0)),
						assert(myDestinationTicket(TicketNo,From,To,Score,1)),				
						decideAction(ActionList, Action)
					)				
				),
				!
			);
			(
				% it is not possible to complete ticket so mark it and check next one
				debug_log('It is not possible to complete destination ticket'),
				retract(myDestinationTicket(TicketNo,From,To,Score,0)),
				assert(myDestinationTicket(TicketNo,From,To,Score,2)),				
				decideAction(ActionList, Action)
			)
		),
		!
	);
	(
		remainingWagonCount(W),
		(
			(W<6,connected(TargetFromNew,TargetToNew,2,gray,_,empty),perform_action(TargetFromNew,TargetToNew,Action));	
			(not(W<6),Action=ticket)
		)
	).

    %Action=ticket; Action=build; Action=card.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%drawCardCardType give the card color which we need. it is calculated in perform_action which is called in each turn
%we are checking the card in open deck first, if it is available, it is our choice, otherwise we will pick card from closed deck
drawCard(SlotNo) :- 
	drawCardCardType(Color), 
	(
		(
			mySlot(X),
			readCardSlot(X,Color),
			SlotNo = X,
			!
		)
		;
		(
			SlotNo=0
		)
	).

%update myWagonCard after card result
drawCardResult(Card) :- retract(myWagonCard(Card,Count)), NewCount is Count + 1, assert(myWagonCard(Card,NewCount)),
	debug_log('The following card is drawn:'),debug_log(Card).

%%%%%%%%
mySlot(1).
mySlot(2).
mySlot(3).
mySlot(4).
mySlot(5).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%selectTickets(TicketList, SelectedTickets) :- [_|SelectedTickets]=TicketList.
selectTickets(TicketList, SelectedTickets) :- 
	(
		isGameJustStarted(yes),
		SelectedTickets = TicketList,		
		retract(isGameJustStarted(yes)),
		!
	);
	(
		[H1,H2,H3] = TicketList,
		ticket(H1,F1,T1,S1), ticket(H2,F2,T2,S2), ticket(H3,F3,T3,S3), 
		find_mindiff_with_wagon(F1,T1,MinDiff_1,PathLen_1,Possible1),
		find_mindiff_with_wagon(F2,T2,MinDiff_2,PathLen_2,Possible2),
		find_mindiff_with_wagon(F3,T3,MinDiff_3,PathLen_3,Possible3),
		debug_log('Destination Ticket Select Algorithm log'),
		debug_log([F1,T1,MinDiff_1,PathLen_1,Possible1]),
		debug_log([F2,T2,MinDiff_2,PathLen_2,Possible2]),
		debug_log([F3,T3,MinDiff_3,PathLen_3,Possible3]),
		((Possible1==1,MinDiff_1>0,SC1 is S1*S1/MinDiff_1);((not(Possible1==1);not(MinDiff_1>0)),SC1 is 0)),
		((Possible2==1,MinDiff_2>0,SC2 is S2*S2/MinDiff_2);((not(Possible2==1);not(MinDiff_2>0)),SC2 is 0)),
		((Possible3==1,MinDiff_3>0,SC3 is S3*S3/MinDiff_3);((not(Possible3==1);not(MinDiff_3>0)),SC3 is 0)),
		remainingWagonCount(WagonCount),
		(
			(
				Possible1 == 1, not(PathLen_1>WagonCount), SC1 > SC2, SC1 > SC3, 
				Extra = [],
				((Possible2==0,MinDiff_2==0,Extra_2=[H2|Extra]);  ((not(Possible2==0);not(MinDiff_2==0)),Extra_2=Extra)),
				((Possible3==0,MinDiff_3==0,Extra_3=[H3|Extra_2]);((not(Possible3==0);not(MinDiff_3==0)),Extra_3=Extra_2)),
				SelectedTickets = [H1|Extra_3],!
			);
			(
				Possible2 == 1, not(PathLen_2>WagonCount), SC2 > SC3, 
				Extra = [],
				((Possible1==0,MinDiff_1==0,Extra_1=[H1|Extra]);  ((not(Possible1==0);not(MinDiff_1==0)),Extra_1=Extra)),
				((Possible3==0,MinDiff_3==0,Extra_3=[H3|Extra_1]);((not(Possible3==0);not(MinDiff_3==0)),Extra_3=Extra_1)),
				SelectedTickets = [H2|Extra_3],!
			);
			(
				Extra = [],
				((Possible1==0,MinDiff_1==0,Extra_1=[H1|Extra]);  ((not(Possible1==0);not(MinDiff_1==0)),Extra_1=Extra)),
				((Possible2==0,MinDiff_2==0,Extra_2=[H2|Extra_1]);((not(Possible2==0);not(MinDiff_2==0)),Extra_2=Extra_1)),
				SelectedTickets = [H3|Extra_2] % take ticket because it is mandatory to take at least one ticket and this is the lowest whether it is possible to complete or not
			)
		)   
	).   
	
selectTicketResult(SelectedTickets) :- 
	keepSelectedTickets(SelectedTickets),
	debug_log('I picked following destination tickets'), debug_log(SelectedTickets).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%buildRoadCardList gives wagon card which will be used to build road from City1 to City2
% it is calculated in perform_action which is called in each turn

buildRoad(City1, City2, CardList) :- buildRoadCardList(City1, City2, CardList).
buildRoadResult(Success) :-
	Success,
	buildRoadCardList(City1, City2, CardList),
	update_wagon_cards(CardList),
	remainingWagonCount(WagonCount),
	debug_log('Remaining # wagon cards'),debug_log(WagonCount),
	write('Road has been built.'),
	nl
	;
	write('Road could not be built.'),
	nl.

%%%%%%%%%%%%%%%%%%%%
update_wagon_cards([H]) :- 
	retract(remainingWagonCount(WagonCount)),
	retract(myWagonCard(H, Count)),
	NewCount is Count - 1,
	NewWagonCount is WagonCount - 1,
	assert(myWagonCard(H, NewCount)),
	assert(remainingWagonCount(NewWagonCount)),
	!.
update_wagon_cards([H|T]) :- 
	update_wagon_cards(T),
	retract(remainingWagonCount(WagonCount)),
	retract(myWagonCard(H, Count)),
	NewCount is Count - 1,
	NewWagonCount is WagonCount - 1,
	assert(myWagonCard(H, NewCount)),
	assert(remainingWagonCount(NewWagonCount)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

announceAction(drawcard, Player, CardList) :- write(Player),write(' received card(s): '),write(CardList),nl.
announceAction(selectticket, Player, TicketCount) :- write(Player),write(' received '),write(TicketCount),write(' tickets.'),nl.
announceAction(buildroad, Player, City1, City2, CardList) :- write(Player),write(' built route: '),write(City1),write('-'),write(City2),write(' with '),write(CardList),nl.
announceAction(clearfaceup, DiscardedCardList) :- write('Face-up cards are discarded: '),write(DiscardedCardList),nl.
announceAction(timeout, Player) :- write(Player),write(' run out of time!'),nl.
announceAction(deckshuffled) :- write('deck shuffled!'),nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%This function will decide whether to "build" road or draw "card"
%If it is possible to build road with cards, it will return "build". Joker has to be used to build road, if necessary
%If road is gray, any color can be used which builds road 
%If there are two roads between cities, the road will be chosen which is easier to build for us 
%decide action will be stored in buildRoadCardList(List) & drawCardCardType(Color)
%Example to build road: assert(buildRoadCardList(chicago, duluth,[red,red,joker])) 
%Example to draw card: assert(drawCardCardType(red))
perform_action(TargetFrom,TargetTo,Action) :- 
	retractall(drawCardCardType(_)),
	retractall(buildRoadCardList(_,_,_)),
	find_target_color(TargetFrom, TargetTo, TargetColor, TargetCount), % find the color and the number of wagon cards we need to have
	check_wagon_cards(TargetColor, TargetCount, BuildPossible, ResultCards),
	(
		(
			BuildPossible = 1,
			Action = build,
			assert(buildRoadCardList(TargetFrom, TargetTo, ResultCards)),
			debug_log('I want to build road from '),debug_log(TargetFrom),debug_log('to'),debug_log(TargetTo),debug_log('with cards'),debug_log(ResultCards),
			!
		);
		(
			BuildPossible = 0,
			assert(drawCardCardType(TargetColor)),
			debug_log('I need the following card '),debug_log(TargetColor),
			Action = card
		)
	).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_wagon_cards(Color, RoadLength, BuildPossible, ResultCards) :-
	myWagonCard(Color, Count),
	(
		(
			not(Count<RoadLength),
			BuildPossible = 1,
			convertTC(Color,RoadLength,ResultCards),
			!
		);
		(
			myWagonCard(joker, JokerCount),
			not((Count+JokerCount)<RoadLength),			
			BuildPossible = 1,
			convertTC(Color,Count,ResultCardsRegular),
			RequiredJokerCount is RoadLength-Count,
			convertTC(joker,RequiredJokerCount,ResultCardsJoker),
			append(ResultCardsRegular,ResultCardsJoker,ResultCards),
			!
		);
		(
			BuildPossible = 0,
			ResultCards = []
		)
	).

	
	

%convert (yellow,3) to [yellow,yellow,yellow]:
convertTC(Color, 0, []) :- !.						
convertTC(Color, 1, [Color]) :- !.
convertTC(Color, L, [Color|List]) :-
	L1 is L - 1,
	convertTC(Color, L1, List).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if gray, find the color which can be used to build road
%else; find the color which we have already have to build road
%      if it is not possible, return the color which we have more
find_target_color(TargetFrom, TargetTo, ResultColor, ResultLen) :- 
	(
		connected(TargetFrom, TargetTo, Length, gray, _, empty),
		ResultLen=Length,
		(
			(
				myWagonCard(Color,Count),
				not(Count<Length),
				ResultColor=Color,
				!
			);
			(
				findall((Color,Count),(myWagonCard(Color,Count),Color \==joker),L),
				find_max_couple(L,ResultColor,ResultColorCount)
			)
		),
		!
	);
	(
		connected(TargetFrom, TargetTo, Length, Color, _, empty),
		myWagonCard(Color,Count),
		not(Count<Length),
		ResultColor=Color,
		ResultLen=Length,
		!
	);
	(
		findall((Color,Count),(connected(TargetFrom, TargetTo, Length, Color, _, empty),myWagonCard(Color,Count)),L),
		find_max_couple(L,ResultColor,ResultColorCount),
		connected(TargetFrom, TargetTo, RoadLength, ResultColor, _, empty),
		ResultLen = RoadLength,
		!
	).

find_max_couple([(Element,Count)],Element,Count) :- !.	
find_max_couple([(Element,Count)|T],ElementResult,CountResult) :- 
	find_max_couple(T,T_Result,T_Count),
	((T_Count>Count,ElementResult=T_Result,CountResult=T_Count);
	 (not(T_Count>Count),ElementResult=Element,CountResult=Count)).		


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% finds components of route in order to complete ticket
find_all_claim_route_list(Start,Goal,Result) :- 
	findMinDifficulty(Start,Goal,_,MinPath),
	convert_list_to_couples(MinPath,Result,PathLen).



find_mindiff_with_wagon(Start,Goal,MinDiff,PathLen,ActionPossible) :- 
	findMinDifficulty(Start,Goal,MinDiff,MinPath),
	convert_list_to_couples(MinPath,Result,PathLen),
	ActionPossible is 1,!;
	ActionPossible is 0.	
	
findMinDifficulty(Start,Start,MinVal,[Start]) :- MinVal is 0, !.

%selects minimum difficulty from the list
findMinDifficulty(Start,Goal,MinVal,MinPath) :- 
	findall((MinDifficulty,Path),
		calculateAllDifficultiesFirst(Start,Goal,MinDifficulty,Path),
		List),
	min_in_list(List,MinVal,MinPath),
	clearDifficultyValues.

%calculates all difficulties for the path
calculateAllDifficultiesFirst(Start,Goal,MinDifficulty,Path) :- 
	assert(difficultyValues(Start,0)),
	continuous_path_list(Start,Goal,Path,_),
	difficultyValues(Goal,MinDifficulty).
	
	
%clean up all difficultyValues in the system at the end of calculation	
clearDifficultyValues :- 
	findall(X,
		(cityCoord(X,_,_),retract(difficultyValues(X,_))),
		Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
% continuos path list calculates all possible paths from A to B
% during this operation we fill difficultyValues for each city (difficultyValues is dynamic fact)
% for A its value is zero and for the neighbours of A its value is difficulty of edge
% for each city it is calculate in the following format
%    let's say that X is neighbour of Y
%        if X does not have difficulty value, difficultyValue(X,DiffValX) equals to difficulty value of Y plus difficulty of edge X-Y
%        if X has difficulty value, we calculate difficultyValue again and we compare the values. If new value is lower than we updated the value by using retract and assert functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
continuous_path_list(A,B,Path_AB,DifficultyAB) :-
						route(A,B,[A],PathAB,DifficultyAB), 
						reverse(PathAB,Path_AB).		
		
		

route(A,B,P,[B|P],D) :- 
    difficulty(A,B,D),
	(
		(
			difficultyValues(B,DifficultyB),
			difficultyValues(A,DifficultyA),
			DifficultyB>D+DifficultyA,
			retract(difficultyValues(B,_)),
			NewDifficultyNode is D+DifficultyA,
			assert(difficultyValues(B,NewDifficultyNode))
		);
		(
			not(difficultyValues(B,_)),
			difficultyValues(A,DifficultyA),
			NewDifficultyNode is D+DifficultyA,
			assert(difficultyValues(B,NewDifficultyNode))
		)
	).
	
route(A,B,Visited,Path,DifficultyAB) :-
       difficulty(A,C,DifficultyAC),           
       C \== B,
       \+member(C,Visited),	   
	(
		(
			difficultyValues(C,DifficultyC),
			difficultyValues(A,DifficultyA),
			DifficultyC>DifficultyAC+DifficultyA,
			retract(difficultyValues(C,_)),
			NewDifficultyNode is DifficultyAC+DifficultyA,
			assert(difficultyValues(C,NewDifficultyNode))
		);
		(
			not(difficultyValues(C,_)),
			difficultyValues(A,DifficultyA),
			NewDifficultyNode is DifficultyAC+DifficultyA,
			assert(difficultyValues(C,NewDifficultyNode))
		)
	),
       route(C,B,[C|Visited],Path,DifficultyCB),
	   DifficultyAB is DifficultyAC + DifficultyCB,
	   A \== B.
	   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calculates risk factor. Higher risk means that compete route asap
risk(FromCity, ToCity, Risk ) :- 
	connected(FromCity, ToCity, Length, Color, _, Status),
	((Color==gray, Diff is 2) ; (Diff is 1)),
	emptyPathCount(FromCity, ToCity, EmptyPathCount), 
	Risk is (60/Length) * Diff * (2/EmptyPathCount),
	!  %%penny2: add another parameter here which calculates alternative path cost
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

find_max_risk_route([[H1,H2]], H1, H2, R) :- risk(H1,H2,R), !.


find_max_risk_route([[H1,H2]|T], TargetFrom, TargetTo, TargetRisk) :-
	find_max_risk_route(T, TF, TT, TR),
	risk(H1,H2,R),
	(
		(TR>=R,TargetFrom=TF,TargetTo=TT,TargetRisk=TR);
		(TR<R,TargetFrom=H1,TargetTo=H2,TargetRisk=R)
	).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% difficulties of paths
edge(X,Y,PlayerX,DistanceXY) :- connected(X,Y,DistanceXY,Color,Coordinates,PlayerX).

difficulty(X,Y,0) :- myPlayer(PlayerX),edge(X,Y,PlayerX,_).   %there is no difficulty since we are the owner of path

difficulty(X,Y,1) :- edge(X,Y,empty,1).
difficulty(X,Y,2) :- edge(X,Y,empty,2).
difficulty(X,Y,4) :- edge(X,Y,empty,3).
difficulty(X,Y,7) :- edge(X,Y,empty,4).
difficulty(X,Y,10) :- edge(X,Y,empty,5).
difficulty(X,Y,15) :- edge(X,Y,empty,6).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calculates minimum member of list
min(X,Y,X,PH,PT,PH) :- X =< Y, !.
min(X,Y,Y,PH,PT,PT) :- X > Y.
min_in_list([(M,PM)],M,PM) :- !.
min_in_list([(H,PH)|T],M,PFinal) :- min_in_list(T,N,PT), min(H,N,M,PH,PT,PFinal).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calculates size of list	   
size([],0).
size([H|T],L) :- size(T,L1), L is L1 + 1. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% converts [atlanta, miami, new_orleans] to [[atlanta, miami], [miami, new_orleans]]
convert_list_to_couples([H1,H2],R,DistanceXY) :- 
	(difficulty(H1,H2,0),R=[],DistanceXY is 0,!);
	(R=[[H1,H2]],singleEdge(H1,H2,_,DistanceXY),!).
	
convert_list_to_couples([H1,H2|T],RR,DistanceXY) :- 
	(difficulty(H1,H2,0),RR=R,convert_list_to_couples([H2|T],R,D),DistanceXY is D,!);
	(RR=[[H1,H2]|R],convert_list_to_couples([H2|T],R,D),singleEdge(H1,H2,_,Distance12),DistanceXY is Distance12 + D).


singleEdge(X,Y,PlayerX,DistanceXY) :- edge(X,Y,PlayerX,DistanceXY),!.	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% we want to display some messages during development due to debug our algorithm
debug_status(ok).
debug_log(Message) :- (
						   debug_status(ok), 
						   write('--PENNY--'), write(Message), nl, 						   
						   open('GameLog.txt', append, Stream),write(Stream, Message),nl(Stream),close(Stream)
                      ); 
					  debug_status(nok).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the number of empty paths between cities	
emptyPathCount(FromCity, ToCity, Count) :-
	findall((FromCity,ToCity),connected(FromCity,ToCity,_,_,_,empty) , X_All),
	size(X_All, Count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% keep selected tickets in memory
keepSelectedTickets([H]) :- ticket(H,From,To,Score), assert(myDestinationTicket(H,From,To,Score,0)),!.
keepSelectedTickets([H|T]) :- ticket(H,From,To,Score), assert(myDestinationTicket(H,From,To,Score,0)), keepSelectedTickets(T).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% find the ticket with highest value in hand which is not completed
searchHighestTicket([(H,HS)],H,HS) :- !.
searchHighestTicket([(H,HS)|T],RT,RS) :- searchHighestTicket(T,TT,TS), ((HS>=TS,RT=H,RS=HS);(HS<TS,RT=TT,RS=TS)).

findHighestTicket(TicketNo) :- 
	findall((T,S),myDestinationTicket(T,_,_,S,0),L),
	searchHighestTicket(L,TicketNo,_).
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

