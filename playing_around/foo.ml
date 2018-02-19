let count (x : 'a) (xs : 'a list) : int =
	List.length (List.filter (fun p -> p == x) xs)

let nub (l : 'a list) : 'a list =
	List.fold_left (fun acc x -> 
		if List.exists (fun p -> p == x) acc 
		then acc 
		else acc@[x]
	) [] l


let rec do_nub2 (l : 'a list) (acc : 'a list) : 'a list = 
	match l with
	| []     -> acc
	| h :: t -> if List.exists (fun p -> p == h) acc
		    then do_nub2 t acc
		    else do_nub2 t (acc@[h])

let nub2 (l : 'a list) : 'a list =
	do_nub2 l []

let rec take (l : 'a list) (n : int) (acc : 'a list) =
	match l with
	| []     -> acc
	| h :: t -> if List.length acc == n
		    then acc
		    else take t n acc@[h] 

let rec subseq (xs : 'a list) (ys: 'a list) : bool =
	match xs with
	| []         -> true
        | xs_h::xs_t -> match ys with
                        | []         -> false
                        | ys_h::ys_t -> if xs_h == ys_h
                                        then subseq xs_t ys_t
                                        else subseq xs ys_t

let rec frontseq (xs : 'a list) (ys : 'a list) : bool =
	match xs with
	| []         -> true
	| xs_h::xs_t -> match ys with
                        | []         -> false
                        | ys_h::ys_t -> if xs_h == ys_h
                                        then frontseq xs_t ys_t
                                        else false

let rec seq (xs : 'a list) (ys : 'a list) : bool =
	match ys with
	| []         -> false
	| ys_h::ys_t -> if frontseq xs ys 
	                then true
	                else seq xs ys_t 

(* 7. A suitable datatype to represent turns may just be a tuple of length 2, with the
      first item in the tuple being player one's choice of number and the second item
      being the same but for player 2.
      A tagged tuple can be used to make this clearer, or perhaps some kind of record/map
      but a simple tuple of length 2 is the simplest and most efficient way.
      A series of turns then can be modelled via having a list of turns.

      I.e. a single turn = (PLAYER1_CHOICE, PLAYER2_CHOICE)
           a series of turns =	[
                               		(PLAYER1_CHOICE, PLAYER2_CHOICE); 
					(PLAYER1_CHOICE, PLAYER2_CHOICE)...
				]
*)

type result = P1Win | P2Win | Draw

let play_round (x : int) (y : int) : result =
	if x = y 
	then Draw
	else if (x + y) mod 2 = 0
	     then P1Win
             else P2Win

let play_sequence (xs : int list) (ys : int list) : result =
	let ziplist = List.map2 (fun a b -> (a, b)) xs ys in
	let wins    = List.map (fun pair -> match pair with | (a, b) -> play_round a b) ziplist in
	let normalised_wins = List.map (fun result -> match result with | P1Win -> 1 | P2Win -> -1 | Draw -> 0) wins in
	let result = List.fold_left (fun res acc -> acc + res) 0 normalised_wins in
	if result = 0
	then Draw
	else if result > 0
             then P1Win
             else P2Win

(* fun (plays : int list) -> List.hd (List.rev plays) *)

let play_strategy (plays : int list) strategy : int =
	strategy plays

let rec play_turns strategy_1 strategy_2 (p1_plays : int list) (p2_plays : int list) (turns : int) =
	if turns > 0
	then play_turns strategy_1 strategy_2 (p1_plays@[(play_strategy p2_plays strategy_1)]) (p2_plays@[(play_strategy p1_plays strategy_2)]) (turns - 1)
	else (p1_plays, p2_plays)

let rec play_strategies strategy_1 strategy_2 (turns : int) =
	play_turns strategy_1 strategy_2 [] [] turns
