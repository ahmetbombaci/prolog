%% Copyright (c) Michele Baldessari - 2000
%% michele@pupazzo.org


%%  Dijkstra Algorithm to find out the shortest path in a directed graph
%%  ----------------------------------------------------------------------

dijkstra(Graph, Source, Drain, Cost, Path) :-
	dijkstra_(Graph, [0-p(Source,[Source])], Drain, Cost, PathStack) ->
		reverse(PathStack, Path)
		;
		Cost is 1000,
		Path  = [].

dijkstra_(T, Lambda, Drain, Cost, Path) :-
	minimal_vertex(T, Lambda, Lu, U, Pu, Uads),
	(  U == Drain -> Cost = Lu, Path = Pu ;
	   recompute_lambda(Uads, T, Pu, Lu, Lambda, Lambda2),
	   ord_del_element(T, U-Uads, T2),
	   dijkstra_(T2, Lambda2, Drain, Cost, Path)
	).

minimal_vertex(Sgraph, CVlist, C, V, P, Ads) :-
	keysort(CVlist, Sorted),
	member(C-p(V,P), Sorted),
	memberchk(V-Ads, Sgraph),
	!.

recompute_lambda([], _T, _Pu, _Lu, Lambda,  Lambda).
recompute_lambda([We-V|R], T,  Pu,  Lu, Lambda0, Lambda) :-
	(  memberchk(V-_, T)
	-> LuWe is Lu + We,
	   (  memberchk(Lv-p(V,Pv), Lambda0)
	   -> (  Lv > LuWe
	      -> ord_del_element(Lambda0, Lv-p(V,Pv), Lambda1),
	         ord_add_element(Lambda1, LuWe-p(V,[V|Pu]), Lambda2)
	      ;  Lambda0 = Lambda2
	      )
	   ;  ord_add_element(Lambda0, LuWe-p(V,[V|Pu]), Lambda2)
	   )
	;  Lambda0 = Lambda2
	),
	recompute_lambda(R, T, Pu, Lu, Lambda2, Lambda).


%%  A bunch of predicates to insert and remove elements from sorted lists
%%  and remove_first which is just used in the distance predicate
%%  ---------------------------------------------------------------------
remove_first([_|T],T).

ord_add_element([], El, [El]). 
ord_add_element([H|T], El, Add) :- compare(Order, H, El), addel(Order, H, T, El, Add).

addel(<, H, T,  El, [H|Add]) :- ord_add_element(T, El, Add).
addel(=, H, T, _El, [H|T]). 
addel(>, H, T,  El, [El,H|T]).

ord_del_element([], _El, []).
ord_del_element([H|T], El, Del) :- compare(Order, H, El), delel(Order, H, T, El, Del).

delel(<,  H, T,  El, [H|Del]) :- ord_del_element(T, El, Del).
delel(=, _H, T, _El, T).
delel(>,  H, T, _El, [H|T]).


