:- module(human, []).
:- dynamic cards/1.
:- dynamic tickets/1.

cards([]).
tickets([]).

initialize(PlayerName, PlayerCount) :-
	write('I am '),write(PlayerName),write(' of a '),write(PlayerCount),write(' player game.'),nl.
	
decideAction(ActionList, Action) :- 
	write('*** Select your action. Available actions: '), write(ActionList), nl,
	cards(C), write('Your cards: '), write(C), nl,
	tickets(T), write('Your tickets: '), write(T), nl,
	(
		read(Action0),
		member(Action0, ActionList),
		Action = Action0
	).

drawCard(SlotNo) :- 
	write('*** Select slot to draw card: '), nl,
	read(SlotNo0),
	SlotNo0 >= 0, SlotNo0 =< 5,
	SlotNo is SlotNo0.
	
drawCardResult(Card) :-
	Card \= none, Card \= empty,
	write('Received card: '),
	write(Card),
	cards(CardList),
	retract(cards(_)),
	assert(cards([Card|CardList])),
	nl
	;
	write('Card not received'),
	nl.

selectTickets(TicketList, SelectedTickets) :- 
	write('*** Select tickets. Ticket list: '),write(TicketList), nl,
	read(Tickets0),
	SelectedTickets = Tickets0.
	%sort(Tickets0, Tickets1),
	%length(Tickets1, L), L >= 1,
	%subset(Tickets0, TicketList),
	%SelectedTickets = Tickets1.
	
selectTicketResult(SelectedTickets) :-
	write('Received tickets: '),
	write(SelectedTickets),
	tickets(TicketList),
	retract(tickets(_)),
	append(SelectedTickets,TicketList,NewTicketList),
	assert(tickets(NewTicketList)),
	nl.

buildRoad(City1, City2, CardList) :-
	write('Your cards: '), nl,
	cards(C), 
	write(C), nl,
	write('Enter first city:'),
	read(City1),
	write('Enter second city:'),
	read(City2),
	write('Enter cards: '),
	read(CardList),
	nl.

buildRoadResult(Success) :-
	Success,
	write('Road has been built.'),
	nl
	;
	write('Road could not be built.'),
	nl.

announceAction(drawcard, Player, CardList) :- write(Player),write(' received card(s): '),write(CardList),nl.
announceAction(selectticket, Player, TicketCount) :- write(Player),write(' received '),write(TicketCount),write(' tickets.'),nl.
announceAction(buildroad, Player, City1, City2, CardList) :- write(Player),write(' built route: '),write(City1),write('-'),write(City2),write(' with '),write(CardList),nl.
announceAction(clearfaceup, DiscardedCardList) :- write('Face-up cards are discarded: '),write(DiscardedCardList),nl.
announceAction(timeout, Player) :- write(Player),write(' run out of time!'),nl.
announceAction(deckshuffled) :- write('deck shuffled!'),nl.