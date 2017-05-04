:- include(dijkstra).

connected(X,Y,Length,Color,Coordinates,Status) :- 
	path(X,Y,Length,Color,Coordinates,Status)
	;
	path(Y,X,Length,Color,Coordinates,Status).

playerCanConnect(City1, City2, Player, RoadCost) :- 
	   connected(City1, City2, _, _, _, Player),
	   RoadCost is 0
	   ;
	   connected(City1, City2, RoadCost, _, _, empty).
	   
% Convert graph to the representation needed for dijkstra code.
playerGraph(Player, Graph, dist) :- 
	setof(X-Ys,
		setof(Length-Y,
			playerCanConnect(X, Y, Player, Length)
		,Ys)
	,Graph).

playerGraph(Player, Graph, eff) :- 
	setof(X-Ys,
		setof(Cost-Y,
			Length^
			(
				playerCanConnect(X, Y, Player, Length),
				c(Length, Cost)
			)
		,Ys)
	,Graph).

playerGraph(Player, Graph, eff, Path) :-
	setof(X-Ys,
		setof(Cost-Y,
			Length^
			(
				playerCanConnect(X, Y, Player, Length),
					(Length \=0, (nextto(X,Y,Path);nextto(Y,X,Path))) -> Cost is 0 ; c(Length, Cost)
			)
		 ,Ys),
	Graph).


