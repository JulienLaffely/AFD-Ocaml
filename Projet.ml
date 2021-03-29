type expr_reg = Vide
		| Epsilon
		| S of char
		| Ou of expr_reg * expr_reg
		| Concat of expr_reg * expr_reg
		| Etoile of expr_reg
;;


(* 1 *)

let trans = [(0,[('a',0);('b',1)]) ; (1,[('b',1);('a',2)]) ;(2,[('a',2);('b',3)]) ; (3,[('a',0);('b',3)])] ;;

let generalisation_transaction = function liste_transaction -> List.map (fun (car,etat_suiv) -> (S car , etat_suiv)) liste_transaction ;;

let generaliser_afd = function transaction -> 
	List.map (fun (etat,liste_transaction) -> (etat , generalisation_transaction liste_transaction)) transaction;;

let trans_gene = generaliser_afd trans ;;

(* 2 *)

let rec expr_reg_to_string = function expr_reguliere -> 
	match expr_reguliere with 
	Vide -> " "
	|Epsilon -> "Eps" 
	|S carac -> Char.escaped carac
	|Ou (expression1,expression2)->  "("^ (expr_reg_to_string expression1) ^" | "^(expr_reg_to_string expression2) ^")"
	|Concat (expression1,expression2)-> "("^ (expr_reg_to_string expression1) ^" . "^(expr_reg_to_string expression2) ^")"
	|Etoile expression -> "("^(expr_reg_to_string expression) ^ ")*" ;; 

let test = Etoile (Concat (Ou (S 'c', Epsilon),S 'b'));;

expr_reg_to_string test ;;

(* 3 *)

(* remplacement des doublons par des OU *)

let rec retirer_doublons = function reste -> function num_doublons ->
	match reste with
	[] -> []
	|(car,num)::r -> if (num=num_doublons) then retirer_doublons r num_doublons else  (car,num)::(retirer_doublons r num_doublons) ;;

let rec elimination_doublons_transisition_etat_bis = function transisions_dun_etat ->
	match transisions_dun_etat with
	[] -> []
	|(car,num)::r ->let doublons = List.find_all (fun (x,y)-> y=num) r in 
			 if(doublons =[] )
			 then (car,num)::(elimination_doublons_transisition_etat_bis r)
			 else let reste_sans_doublons = retirer_doublons r num in
				((List.fold_left (fun x (y,z) -> Ou(x,y)) car doublons),num)::(elimination_doublons_transisition_etat_bis reste_sans_doublons) ;;

let rec elimination_doublons_transisition_etat = function transisitions ->
	match transisitions with
	[] -> []
	|(numetat,transisions_dun_etat)::r -> (numetat,elimination_doublons_transisition_etat_bis transisions_dun_etat)::elimination_doublons_transisition_etat r ;;

(* supprimer etat *)

let rec modifier_transaction_sans_boucle = function caracteres_transition_actuelle -> function etat_a_supprimer ->
	match etat_a_supprimer with 
	(numetat,[]) -> []
	|(numetat,(caractere,numetatsuiv)::r) -> if(numetat=numetatsuiv)
						 then modifier_transaction_sans_boucle caracteres_transition_actuelle (numetat,r)
						 else (Concat(caracteres_transition_actuelle,caractere),numetatsuiv)::modifier_transaction_sans_boucle caracteres_transition_actuelle (numetat,r);;

let modifier_transaction = function caractere_transaction_actuelle -> function etat_a_supprimer ->
	let couple_boucle = (List.find_opt (fun (x,y)->y=(fst etat_a_supprimer)) (snd etat_a_supprimer)) in 
	match couple_boucle with
	None -> modifier_transaction_sans_boucle caractere_transaction_actuelle etat_a_supprimer 
	|Some (premier,deuxieme) -> modifier_transaction_sans_boucle (Concat(caractere_transaction_actuelle,Etoile(premier))) etat_a_supprimer ;;
	

let rec modification_transaction = function transactions_etat_courant -> function etat_a_supprimer -> function num_etat_sup ->
	match transactions_etat_courant with
	[] -> []
	|(car,num)::r ->  if (num=num_etat_sup)
			  then (modifier_transaction car etat_a_supprimer)@(modification_transaction r etat_a_supprimer num_etat_sup)
			  else (car,num)::(modification_transaction r etat_a_supprimer num_etat_sup);;


let rec elimination_etat_bis = function liste_transitions -> function etat_a_supprimer -> function num_etat_sup ->
	match liste_transitions with
	[] -> []
	|(etat,transactions)::r -> if (etat=num_etat_sup)
				   then elimination_etat_bis r etat_a_supprimer num_etat_sup
				   else (etat,modification_transaction transactions etat_a_supprimer num_etat_sup)::(elimination_etat_bis r etat_a_supprimer num_etat_sup) ;;

let elimination_etat_avec_doublons   =  function num_etat_sup -> function liste_transitions -> 
					let etatsup = List.find_opt (fun (x,y)->x=num_etat_sup) liste_transitions
					in 
					match etatsup with 
					None -> liste_transitions 
					| Some (numetat,transitions) -> elimination_etat_bis liste_transitions (numetat,transitions) num_etat_sup;;

let elimination_etat = function num_etat_sup -> function liste_transitions ->
		elimination_doublons_transisition_etat (elimination_etat_avec_doublons num_etat_sup liste_transitions) ;;

elimination_etat 1 trans_gene ;;

let trans_doublons_gene  = generaliser_afd ([(0,[('a',0);('b',1);('b',2)]) ; (1,[('b',1);('a',2)]) ;(2,[('a',2);('b',3)]) ; (3,[('a',0);('b',3)])]) ;;

elimination_etat 1 trans_doublons_gene ;;


(* 4 *)

let rec eliminer_tous_bis = function etats_a_supprimer -> function transitions  

let rec eliminer_tous = function couple_initial_final -> function nb etat ->function transitions ->
	

(* 5 *)








