:- use_module(library(pce)).
	
boardFile('./board/board.jpg').
cardFile(black,   './board/black_small.jpg').
cardFile(blue,     './board/blue_small.jpg').
cardFile(green,   './board/green_small.jpg').
cardFile(joker,   './board/joker_small.jpg').
cardFile(orange, './board/orange_small.jpg').
cardFile(purple, './board/purple_small.jpg').
cardFile(red,       './board/red_small.jpg').
cardFile(white,   './board/white_small.jpg').
cardFile(yellow, './board/yellow_small.jpg').

updateCardSlotsGUI(1) :- readCardSlot(1, Color), cardFile(Color,File), free(@card1), send(@main, display, new(@card1, bitmap(File)), point(22,5)), send(@main, flush);  free(@card1).
updateCardSlotsGUI(2) :- readCardSlot(2, Color), cardFile(Color,File), free(@card2), send(@main, display, new(@card2, bitmap(File)), point(22,52)), send(@main, flush);  free(@card2).
updateCardSlotsGUI(3) :- readCardSlot(3, Color), cardFile(Color,File), free(@card3), send(@main, display, new(@card3, bitmap(File)), point(22,99)), send(@main, flush);  free(@card3).
updateCardSlotsGUI(4) :- readCardSlot(4, Color), cardFile(Color,File), free(@card4), send(@main, display, new(@card4, bitmap(File)), point(22,146)), send(@main, flush);  free(@card4).
updateCardSlotsGUI(5) :- readCardSlot(5, Color), cardFile(Color,File), free(@card5), send(@main, display, new(@card5, bitmap(File)), point(22,193)), send(@main, flush);  free(@card5).

updateCardSlotsGUI :- 
	forall( between(1, 5, SlotNo), 
		(
			updateCardSlotsGUI(SlotNo)
		)
	).

refreshGUI :- 
	updateCardSlotsGUI.
	
buildRoadGUI(_, []).
buildRoadGUI(Color, [X,Y|Coordinates]) :- 
	Radius is 20,
	send(@main, display, new(C, circle(Radius)), point(X-Radius/2,Y-Radius/2)),
	send(C, fill_pattern, colour(Color)),
	buildRoadGUI(Color, Coordinates).

initializeBoard :-
	new(@main, dialog('Ticket to Ride')),
	boardFile(BoardFile),
	send(@main, display, new(@board, bitmap(BoardFile)), point(0,0)),
	send(@main, open).
