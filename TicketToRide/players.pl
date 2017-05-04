:- dynamic player/3.

playerColor(player1, purple).
playerColor(player2, blue).
playerColor(player3, red).
playerColor(player4, yellow).
playerColor(player5, green).

% --------------------------------
% Initialize players
% --------------------------------

initializePlayers(PlayerModules) :-
	length(PlayerModules, PlayerCount),
	PlayerCount > 1, PlayerCount =< 5,
	retractall(player),
	forall(between(1, PlayerCount, PlayerNo),
		(
			nth1(PlayerNo, PlayerModules, PlayerModule),
			atom_concat(player, PlayerNo, Player),
			assert(player(Player, score, 0)),
			assert(player(Player, trainCarCards, [])),
			assert(player(Player, destinationTickets, [])),
			assert(player(Player, wagons, 45)),
			assert(player(Player, module, PlayerModule)),
			atom_concat('players/', PlayerModule, PlayerModuleFile),
			use_module(PlayerModuleFile),
			PlayerModule:initialize(Player, PlayerCount)
		)
	).

getPlayerModule(Player, Module) :-
	player(Player, module, Module).
	
updatePlayerScore(Player, ScoreUpdate) :-
	player(Player, score, CurrentScore),
	NewScore is CurrentScore + ScoreUpdate,
	retract(player(Player, score, _)),
	assert(player(Player, score, NewScore)).
	
% --------------------------------
% Train car card methods
% --------------------------------

assignTrainCarCard(Player, Card) :- 
	player(Player, trainCarCards, PlayerTrainCarCards), 
	retract(player(Player, trainCarCards, _)),
	assert(player(Player, trainCarCards, [Card|PlayerTrainCarCards])).

readPlayerTrainCarCards(Player, PlayerTrainCarCards) :- 
	player(Player, trainCarCards, PlayerTrainCarCards).
	
useTrainCarCards(Player, CardList) :- 
	player(Player, trainCarCards, PlayerTrainCarCards), 
	subtractlist(PlayerTrainCarCards, CardList, NewPlayerTrainCarCards),
	retract(player(Player, trainCarCards, _)),
	assert(player(Player, trainCarCards, NewPlayerTrainCarCards)).

% --------------------------------
% Wagon methods
% --------------------------------

readPlayerWagonCount(Player, PlayerWagonCount) :- 
	player(Player, wagons, PlayerWagonCount).
	
usePlayerWagons(Player, UsedWagonCount) :- 
	player(Player, wagons, PlayerWagonCount),
	NewWagonCount is PlayerWagonCount - UsedWagonCount,
	NewWagonCount >= 0,
	retract(player(Player, wagons, _)),
	assert(player(Player, wagons, NewWagonCount)).

% --------------------------------
% Destination ticket methods
% --------------------------------

assignTicketCards(Player, NewTicketCards) :- 
	player(Player, destinationTickets, PlayerTicketCards), 
	append(NewTicketCards, PlayerTicketCards, MergedTicketCards),
	retract(player(Player, destinationTickets, _)),
	assert(player(Player, destinationTickets, MergedTicketCards)).
	
readPlayerTicketCards(Player, PlayerTicketCards) :- 
	player(Player, destinationTickets, PlayerTicketCards).
