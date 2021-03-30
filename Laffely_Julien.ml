type expr_reg = Vide
		| Epsilon
		| S of char
		| Ou of expr_reg * expr_reg
		| Concat of expr_reg * expr_reg
		| Etoile of expr_reg
;;


(* 1 *)

(* Généralie la transition pour faire en sorte que les caractere soient reconnu en tant qu'expression réguliere *)

let trans = [(0,[('a',0);('b',1)]) ; (1,[('b',1);('a',2)]) ;(2,[('a',2);('b',3)]) ; (3,[('a',0);('b',3)])] ;;

let generalisation_transition = function liste_transition -> List.map (fun (car,etat_suiv) -> (S car , etat_suiv)) liste_transition ;;

let generaliser_afd = function transition -> 
	List.map (fun (etat,liste_transition) -> (etat , generalisation_transition liste_transition)) transition;;

let trans_gene = generaliser_afd trans ;;

(* 2 *)

(* Transforme une expression réguliere en une chaîne de caractères *)

let rec expr_reg_to_string = function expr_reguliere -> 
	match expr_reguliere with 
	Vide -> ""
	|Epsilon -> "Eps" 
	|S carac -> Char.escaped carac
	|Ou (expression1,expression2)->  "("^ (expr_reg_to_string expression1) ^"|"^(expr_reg_to_string expression2) ^")"
	|Concat (expression1,expression2)-> "("^ (expr_reg_to_string expression1) ^"."^(expr_reg_to_string expression2) ^")"
	|Etoile expression -> "("^(expr_reg_to_string expression) ^ ")*" ;; 

let test = Etoile (Concat (Ou (S 'c', Epsilon),S 'b'));;

expr_reg_to_string test ;;

(* 3 *)

(* Ces fonctions permettent de réduire la fonction de transition d'un AFD si un état est supprimer et que par conséquent plusieurs transitions vont vers le meme état au sein d'un état *)

(* lorsque un doublons à été trouvé et qu'ils ont été assemblé , alors le doublons est encore dans le reste donc on le supprimer du reste *)

let rec retirer_doublons = function reste -> function num_doublons ->
	match reste with
	[] -> []
	|(car,num)::r -> if (num=num_doublons) then retirer_doublons r num_doublons else  (car,num)::(retirer_doublons r num_doublons) ;;

(* si un doublons est trouvé alors on va mettre les deux transition dans le meme couple avec un Ou *)

let rec elimination_doublons_transisition_etat_bis = function transisions_dun_etat ->
	match transisions_dun_etat with
	[] -> []
	|(car,num)::r ->let doublons = List.find_all (fun (x,y)-> y=num) r in 
			 if(doublons =[] )
			 then (car,num)::(elimination_doublons_transisition_etat_bis r)
			 else let reste_sans_doublons = retirer_doublons r num in
				((List.fold_left (fun x (y,z) -> Ou(x,y)) car doublons),num)::(elimination_doublons_transisition_etat_bis reste_sans_doublons) ;;

(* permet de recreer la liste de transitions *)

let rec elimination_doublons_transisition_etat = function transisitions ->
	match transisitions with
	[] -> []
	|(numetat,transisions_dun_etat)::r -> (numetat,elimination_doublons_transisition_etat_bis transisions_dun_etat)::elimination_doublons_transisition_etat r ;;

(* on concatene la transition vers l'etat à supprimer avec les transitions de l'état à supprimer vers les autres états*)

let rec modifier_transition_sans_boucle = function caracteres_transition_actuelle -> function etat_a_supprimer ->
	match etat_a_supprimer with 
	(numetat,[]) -> []
	|(numetat,(caractere,numetatsuiv)::r) -> if(numetat=numetatsuiv)
						 then modifier_transition_sans_boucle caracteres_transition_actuelle (numetat,r)
						 else (Concat(caracteres_transition_actuelle,caractere),numetatsuiv)::modifier_transition_sans_boucle caracteres_transition_actuelle (numetat,r);;

(* avant de modifier la transition met déja en place une etoile si dans l'état à supprimer il y a une transition qu'il reste sur le meme etat , on concatene cette etoile avec la nouvelle transition )*)

let modifier_transition = function caractere_transition_actuelle -> function etat_a_supprimer ->
	let couple_boucle = (List.find_opt (fun (x,y)->y=(fst etat_a_supprimer)) (snd etat_a_supprimer)) in 
	match couple_boucle with
	None -> modifier_transition_sans_boucle caractere_transition_actuelle etat_a_supprimer 
	|Some (premier,deuxieme) -> modifier_transition_sans_boucle (Concat(caractere_transition_actuelle,Etoile(premier))) etat_a_supprimer ;;
	
(* Dans la liste de transition d'un état on regarde si une de ses transitions mene à l'etat à supprimer et si oui on appelle une autre fonction dans laquelle on modifiera cette transition , la liste est ensuite reformé *)

let rec modification_transition = function transitions_etat_courant -> function etat_a_supprimer -> function num_etat_sup ->
	match transitions_etat_courant with
	[] -> []
	|(car,num)::r ->  if (num=num_etat_sup)
			  then (modifier_transition car etat_a_supprimer)@(modification_transition r etat_a_supprimer num_etat_sup)
			  else (car,num)::(modification_transition r etat_a_supprimer num_etat_sup);;

(* on appelle chaque liste de transitions dans une autre fonction sauf l'etat à supprimer pour ensuite recreer une liste de transitions entiere avec l'etat à supprimer en moins *)

let rec elimination_etat_bis = function liste_transitions -> function etat_a_supprimer -> function num_etat_sup ->
	match liste_transitions with
	[] -> []
	|(etat,transitions)::r -> if (etat=num_etat_sup)
				   then elimination_etat_bis r etat_a_supprimer num_etat_sup
				   else (etat,modification_transition transitions etat_a_supprimer num_etat_sup)::(elimination_etat_bis r etat_a_supprimer num_etat_sup) ;;

(* on cherche si l'etat à supprimer existe et si oui on appelle une autre fonction ci dessus sinon on retourne la meme liste *)

let elimination_etat_avec_doublons   =  function num_etat_sup -> function liste_transitions -> 
					let etatsup = List.find_opt (fun (x,y)->x=num_etat_sup) liste_transitions
					in 
					match etatsup with 
					None -> liste_transitions 
					| Some (numetat,transitions) -> elimination_etat_bis liste_transitions (numetat,transitions) num_etat_sup;;

(* Appelle la fonction qui elimine vraiment l'état en supprimer les doublons de la sortie de cette fonction*)

let elimination_etat = function num_etat_sup -> function liste_transitions ->
		elimination_doublons_transisition_etat (elimination_etat_avec_doublons num_etat_sup liste_transitions) ;;

elimination_etat 1 trans_gene ;;

let trans_doublons_gene  = generaliser_afd ([(0,[('a',0);('b',1);('b',2)]) ; (1,[('b',1);('a',2)]) ;(2,[('a',2);('b',3)]) ; (3,[('a',0);('b',3)])]) ;;

elimination_etat 1 trans_doublons_gene ;;


(* 4 *) 

(* Elimine tous les etats pour réduire à deux états l'AFD *)

let rec eliminer_tous = function couple_initial_final ->function transitions ->
	let etat_a_supprimer = List.find_opt (fun (x,y)-> (x <> fst couple_initial_final) && (x <> snd couple_initial_final)) transitions in
	match etat_a_supprimer with
	 None -> transitions
	| Some(num,liste_transitions) -> eliminer_tous couple_initial_final (elimination_etat num transitions);;

let trans_gene_eliminertous = eliminer_tous (0,3) trans_gene ;;

(* transforme un AFD réduit à deux états en expression régulière *)

let expression_reguliere = function etat_initial -> function etat_final -> function transition ->
	let couple_initial = List.find (fun (x,y) -> x=etat_initial ) transition in
	let couple_final =  List.find (fun (x,y) -> x=etat_final ) transition in
	
	let debutexpression = 
	if(List.length (snd couple_initial)=2)
	then Concat(Etoile(fst (List.find (fun (x,y)-> y=etat_initial) (snd couple_initial))),fst (List.find (fun (x,y)-> y=etat_final) (snd couple_initial))) 
	else fst (List.find (fun (x,y)-> y=etat_final) (snd couple_initial)) 
	
	in

	let finexpression =
	if(List.length (snd couple_final)=2)
	then Ou(fst (List.find (fun (x,y)-> y=etat_final) (snd couple_final)),Concat(fst (List.find (fun (x,y)-> y=etat_initial) (snd couple_final)),debutexpression))
	else Concat(fst (List.find (fun (x,y)-> y=etat_initial) (snd couple_final)),debutexpression)

	in Concat(debutexpression,Etoile(finexpression)) ;; 
	
let test_er = expression_reguliere 0 3 trans_gene_eliminertous ;;

expr_reg_to_string test_er ;;
	

(* 5 *)

(* Disjonction des AFD réduits si il y à plusieurs états acceptants *)

let rec expression_reguliere_AFD_bis = function etat_initial -> function etats_finaux -> function transition ->
	match etats_finaux with 
	[] -> failwith "pas d'etats acceptants" 
	|numetat::[] -> expression_reguliere etat_initial numetat (eliminer_tous (etat_initial,numetat) transition)
	|numetat::r -> Ou(expression_reguliere etat_initial numetat (eliminer_tous (etat_initial,numetat) transition),expression_reguliere_AFD_bis etat_initial r transition);;

let expression_reguliere_AFD = function nbetats -> function etat_initial -> function liste_etats_acceptants -> function fonction_de_transition ->

	let transition = generaliser_afd fonction_de_transition in
	expr_reg_to_string (expression_reguliere_AFD_bis etat_initial liste_etats_acceptants transition) ;;


expression_reguliere_AFD 4 0 [3] trans ;;

(* Quelques exemples *) 

let transition1 = [(0,[('f',0);('a',1);('b',2);('c',3);('d',4)]);(1,[('a',1);('b',2);('c',3);('d',4)]);(2,[('a',1);('d',4)]);(3,[('c',3);('f',0)]);(4,[('a',1)])] ;;
let transition2 = [(0,[('a',0);('b',2);('c',3)]);(1,[('b',2);('c',3)]);(2,[('c',0);('b',2);('a',3)]);(3,[('b',0);('a',3)])]  ;; 
let transition3 = [(0,[('a',0);('b',1);('c',2)]);(1,[('a',1);('c',2);('b',0)]);(2,[('c',2);('a',1)])]  ;; 

expression_reguliere_AFD 5 0 [3;4] transition1 ;;
expression_reguliere_AFD 4 0 [2;3] transition2 ;; 
expression_reguliere_AFD 3 0 [2] transition3 ;; 




