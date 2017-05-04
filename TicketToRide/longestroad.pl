
longestroadPlayers(MaxPlayers) :-
	getPlayerList(PlayerList),
	findall(Length-Player, (member(Player,PlayerList),longestroad(Player,Length)), Ls),
	write('Longest roads: '),write(Ls),nl,
	findall(K-Ps, bagof(P, member(K-P, Ls), Ps), A), keysort(A, B),	reverse(B,[_-MaxPlayers|_]).

longestroad(Player,Length) :- 
	Player \= empty,
	setof(City1-City2-Length, Color^Coordinates^path(City1, City2, Length, Color, Coordinates, Player), Graph),
	setof(RoadLength, Start^Path^findRoad(Graph, Start, RoadLength, Path), Lengths),
	max_list(Lengths, Length).
	
findRoad([],_,0,[]).
findRoad(Graph,Start,Length,[Start|Path]) :- 
	(
		(
			select(Start-B-L,Graph,Res) ; select(B-Start-L,Graph,Res)
		)
		,
		(
			findRoad(Res,B,Length1,Path),
			Length is Length1 + L
		)
	)
	;
	(
		Length is 0,
		Path = []
	).