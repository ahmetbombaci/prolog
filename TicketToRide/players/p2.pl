:- module(p2, []).

initialize(PlayerName, PlayerCount) :-
	write('I am '),write(PlayerName),write(' of a '),write(PlayerCount),write(' player game.'),nl.
	
decideAction(+ActionList, -Action) :- Action=ticket; Action=build; Action=card.

drawCard(SlotNo) :- SlotNo=0.
drawCardResult(Card).

selectTickets(TicketList, SelectedTickets) :- [_|SelectedTickets]=TicketList.
selectTicketResult(SelectedTickets).

buildRoad(City1, City2, CardList).
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