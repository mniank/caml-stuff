(*                                             *)
(*                                             *)
(*                    Copter                   *) 
(*                                             *)
(*               Maxime Niankouri              *)
(*                 Janvier 2011                *)
(*                                             *)
(*                                             *)


#open "graphics";;



(*Génération de  nombre aléatoire : ici compris entre -2 et 2 *)

let alea r =   (* r est un pointeur pouvant faire avancer la suite *)
	let s = ref ((70 * !r + 31)/ 27 + 11) in 
	r := 100* !s / 7 / 3;
	s := !r mod (!s+5);
	r := !r - 13 * !s;
	if(!s<0) then s := - !s;          (* Caml peut renvoyer des % positifs *)
	(!s mod 5) - 2;    (* entre -2 et 2 *)
;;
	
	
	

(*Mouvement de l'engin*)


let move p =    (*t = couloir, v = obstacles, p = ref position actuelle*)
	set_color white;
	fill_rect (100-3) (p.(0)-3) 7 7;
	
	if(button_down()) then      (*Renvoie true lorsque qu'une touche est enfoncée *)
	(
		p.(1) <- p.(1) + 2 ;
	)
	else
	(
		p.(1) <- p.(1) - 3 ;
	);
	p.(0) <- p.(0) + (p.(1)/2);
	set_color red;
	fill_rect (100-3) (p.(0)-3) 7 7;
	p;
;;









(* Fonction créant le tableau formant le couloir. *)


let genere_couloir t r =    (* tableau avec la position actuelle du couloir *)
	for i=0 to (25-2) do
		set_color white;
		for j=0 to (20-1) do
			plot (20*i+j) (t.(i)).(0);
			plot (20*i+j) (t.(i)).(1);
		done;
		t.(i) <- t.(i+1);
		set_color black;
		for j=0 to (20-1) do
			plot (20*i+j) (t.(i)).(0);
			plot (20*i+j) (t.(i)).(1);
		done;	
	done;
	let m = ref (alea r + (t.(25-2)).(0)) in    
	let n = ref (alea r + (t.(25-2)).(1)) in
	if(!m < 0) then m:=0;
	if(!n > 350) then n:=350;	
	if(!r mod 5 > 1) then n := !n -1            (* Pour augmenter la difficulté au fil du temps *)
	else if(!r mod 5 < -1) then m := !m +1;     
	if(!m > !n-13) then
	(
		if(!n>350) then m := !n-13
		else            n := !m+13;
	);
	t.(25-1) <- [| !m ; !n |];
	t;
;;



(* Genere des obstacles *)

let genere_obstacle t v r =
	let i = ref 0 in
	let k = ref 0 in
	let s = ref !r in
	if(!s<0) then s := - !s;
	(* Partie d'affichage*)
	while((!i<15) & (v.(!i)).(1) <> -1) do    (* -1 étant le valeur de fin de tableau *)
		set_color white;
		for j=0 to (20-1) do
			plot (v.(!i)).(0) ((v.(!i)).(1)+j);
		done;
		(v.(!i)).(0) <- (v.(!i)).(0) - 20;		
		set_color blue;
		for j=0 to (20-1) do
			plot (v.(!i)).(0) ((v.(!i)).(1)+j);
		done;
		if((v.(!i)).(0) < -1) then
		(
			k := !i;
			while((!k<15-1) & ((v.(!k)).(1) <> -1)) do
				(v.(!k)).(1) <- (v.(!k+1)).(1);
				(v.(!k)).(0) <- (v.(!k+1)).(0);
				k:= !k+1;
			done;
			(v.(15-2)).(0) <- -1;
			(v.(15-2)).(1) <- -1;
			i := !i-1;
		);
		i:=!i+1;
	done;
	(* Génération de nouveaux obstacles *)
	if((!i<15) & ((!s mod 23) = 0) & ((t.(25-1)).(0) < (t.(25-1)).(1) - 41)) then
	(
		v.(!i) <- [| 25*20 ; (t.(25-1)).(0) + 1 + (!s mod ((t.(25-1)).(1)-(t.(25-1)).(0)-20)) |];
	);
	v;
;;
	
(* Gestion des collisions *)
(* On sait que notre carré est à x constant ( lorsque j'écris : de 100-3 à 100+3 ), donc on étudie ces deux bornes *)

let collide t v p =
	let b = ref true in
		 if((t.(4)).(0) >= (p-3)) then false     (* on regarde la partie gauche de carré en bas *)
	else if((t.(4)).(1) <= (p+3)) then false     (* on regarde la partie gauche de carré en haut *)
	else if((t.(5)).(1) <= (p+3)) then false     (* on regarde la partie droite de carré en haut *)
	else if((t.(5)).(0) >= (p-3)) then false     (* on regarde la partie droite de carré en bas *)
	else
	(
		for i=0 to 14 do
			if(((v.(i)).(0) <= 100+3) & ((v.(i)).(0) >= 100-3) & ((v.(i)).(1) >= p-3-20) & ((v.(i)).(1) <= p+3))
				then ( b := false; );
		done;
		!b;
	);
;;

(* Gestion du score *)

let score z =
	z := !z +1;
	let c = ref !z in
	let s = ref "" in
	set_color green;
	moveto 200 355;
	for i=1 to 8 do
		 s := (string_of_char (char_of_int (48 + (!c mod 10)))) ^ !s;
		 c:= !c/10;
	done;
	draw_string !s;
;;


(* Le main *)

let main() =
	open_graph " 500x435+0-0"; 
	let z = ref 0 in                        
	let v = ref (make_vect 15 [| -1 ; -1 |]) in (* Prépare la banque d'obstacles *)
	let t = ref (make_vect 25 [| 0 ; 350 |]) in (* Prépare le terrain en début de jeu *)
	let r = ref 3 in                            (* Pour la formation aléatoire *)
	let p = ref [| (350/2) ; 0 |] in            (* Position et vitesse initiales de l'engin *)
	while(collide !t !v (!p).(0)) do
		t := genere_couloir !t r;
		v := genere_obstacle !t !v r;
		p := move !p;
		score z;
	done;
	!z
;;
		
	
(* Lancement du jeu*)

main();;
	
	

