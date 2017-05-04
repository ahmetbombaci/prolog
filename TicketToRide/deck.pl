:- dynamic trainCarDeck/1.
:- dynamic ticketDeck/1.
:- dynamic trainCarDeckDiscardPile/1.
:- dynamic cardSlot/2.
:- dynamic deck/2.

cardSlot(1,empty).
cardSlot(2,empty).
cardSlot(3,empty).
cardSlot(4,empty).
cardSlot(5,empty).

deck(initial, [joker,yellow,green,purple,blue,orange,white,red,black,joker,yellow,green,purple,blue,orange,white,red,black,joker,yellow,green,purple,blue,orange,white,red,black,joker,yellow,green,purple,blue,orange,white,red,black,joker,yellow,green,purple,blue,orange,white,red,black,joker,yellow,green,purple,blue,orange,white,red,black,joker,yellow,green,purple,blue,orange,white,red,black,joker,yellow,green,purple,blue,orange,white,red,black,joker,yellow,green,purple,blue,orange,white,red,black,joker,yellow,green,purple,blue,orange,white,red,black,joker,yellow,green,purple,blue,orange,white,red,black,joker,yellow,green,purple,blue,orange,white,red,black,joker,joker]).

% --------------------------------
% Card slot methods
% --------------------------------

readCardSlot(SlotNo, Card) :- 
	cardSlot(SlotNo, Card).
	
setCardSlot(SlotNo, Card) :- 
	retractall(cardSlot(SlotNo, _)),  
	assert(cardSlot(SlotNo, Card)).
	
fillCardSlot(Slot) :- 
	drawTrainCarCard(Card), 
	setCardSlot(Slot, Card),
	checkFaceUpTrainCarCards.

fillAllCardSlots :- 
	forall( between(1, 5, SlotNo), 
		(
			drawTrainCarCard(Card),
			setCardSlot(SlotNo, Card)
		)
	), 
	checkFaceUpTrainCarCards.
	
checkFaceUpTrainCarCards :- 
	findall(_, readCardSlot(_,joker), Js), 
	length(Js, L), 
	L < 3
	;
	(discardFaceUpTrainCarCards, fillAllCardSlots).
	
discardFaceUpTrainCarCards :- 
	deck(discard, Pile0), 
	findall(Card, readCardSlot(_,Card), Cards), 
	append(Pile0, Cards, Pile),
	retractall(cardSlot),
	retractall(deck(discard, _)), 
	forall( between(1, 5, SlotNo), 
		assert(cardSlot(SlotNo,empty))
	),
	assert(deck(discard, Pile)).
	
% --------------------------------
% Train car deck methods
% --------------------------------
initializeTrainCarDeck :- deck(initial, Deck0), retractall(deck(discard, _)), assert(deck(discard, Deck0)), shuffleTrainCarDeck.
shuffleTrainCarDeck :- deck(discard, Deck0), shuffle(Deck0, Deck), retractall(deck(discard, _)), assert(deck(discard, [])), retractall(deck(traincar, _)), assert(deck(traincar, Deck)).

announceDeckShuffle :-
	forall( (getPlayerModule(_, Module)),
		(
			Module:announceAction(deckshuffled)
			;
			true
		)
	).		

drawTrainCarCard(Card) :- 
	deck(traincar, Deck), 
	length(Deck, L),
	(
		(
			L == 0,
			deck(discard, Deck0),
			length(Deck0, L1),
			(
				L1 \= 0,
				shuffleTrainCarDeck,
				announceDeckShuffle,
				drawTrainCarCard(Card)
			)
		)
		;
		(
			L \= 0,
			[Card|RemainingDeck] = Deck,
			retractall(deck(traincar, _)), 
			assert(deck(traincar, RemainingDeck))
		)
	)
	;
	Card = empty.

discardTrainCarCards(Cards) :-
	flatten(Cards, CardList),
	deck(discard, Deck), 
	append(CardList, Deck, NewDeck),
	retractall(deck(discard, _)), 
	assert(deck(discard, NewDeck)).

	
% --------------------------------
% Destination ticket deck methods
% --------------------------------

initializeTicketDeck :- findall(Ticket, ticket(Ticket,_,_,_), Tickets), shuffle(Tickets, Deck), retractall(deck(ticket, _)), assert(deck(ticket, Deck)).

drawTicketCard(Card) :- 
	deck(ticket, [C|RemainingDeck]), 
	Card = [C],
	retractall(deck(ticket, _)), 
	assert(deck(ticket, RemainingDeck))
	;
	Card = [].

returnTicketCards(Tickets) :-
	flatten(Tickets, TicketList),
	deck(ticket, Deck0), 
	append(Deck0, TicketList, Deck),
	retractall(deck(ticket, _)), 
	assert(deck(ticket, Deck)).
	
% --------------------------------
% Common methods
% --------------------------------

subtractlist(A, [], A) :- !.
subtractlist(A, [B|C], D) :-
        select(B, A, E), !,
        subtractlist(E, C, D).
		
shuffle([], []).
shuffle(Deck0, [C|Deck]) :-
	random_element(Deck0, C, Deck1),
	shuffle(Deck1, Deck).
	
random_element(List, Elt, Rest) :-
	length(List, Len),
	Len > 0,
	random(1, Len, Rand),
	select_nth1(Rand, List, Elt, Rest).
	
random(Lo, Hi, Rand) :-
	Rand is Lo+random(Hi-Lo+1).
	
	
select_nth1(N, List, Elt, Rest) :-
	length([_|Front], N), % infinite solutions if N unbound
	append(Front, [Elt|Back], List),
	append(Front, Back, Rest).
%— or —
%select_nth1(1, [Elt|Rest], Elt, Rest).
%select_nth1(N, [E|Es], Elt, [E|Rest]) :-
%	N > 1, % only works if N is bound
%	N1 is N - 1,
%	select nth1(N1, Es, Elt, Rest).