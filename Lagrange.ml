(* UTILISATION *)

(*---------------------

On table la liste de points à interpoler par :

let tableau = [| (x1,y1) ; ... ; (xn,yn) |]

Les xi, yi doivent tous être de même nature.

S'ils sont réels, on tape : lagrange (R tableau)
S'ils sont entiers, on tape : lagrange (N tableau)
(Ils peuvent être entiers relatifs)


Enjoy !

-----------------------*)



(* CODE A COMPILER *)

(* On commence par les opérations de base des polynômes ( un polynome étant une int list) *)

type polynome == float list;;
let test = [-1.;1.];;

let rec add = fun
	| a [] -> a
	| [] a -> a
	| (t::q) (a::b) -> (t+.a)::(add q b)
;;

let rec fact k = fun
	| [] -> []
	| (t::q) -> (k*.t)::(fact k q)
;;

let rec multiply = fun
	| a [] -> []
	| [] a -> []
 	| p (t::q) -> add (fact t p) (0.::(multiply p q))
;;

let rec puissance p = fun
	| 0 -> [1.]
	| n -> multiply p (puissance p (n-1))
;;

(* Lagrange prend un tableau de points en argument (x,y) et retourne le polynome de Lagrange associé *)

let lagrange_aux t = 
	let n = ((vect_length t) -1) in
	let result = ref [0.] in
	let produit = ref [1.] in
	for i=0 to n do
		produit := [1.];
		let xi,yi = t.(i) in
		for j=0 to n do
			if i<>j then (
				let xj,yj = t.(j) in
				produit := multiply !produit (fact (1./.(xj-.xi)) [xj;-1.])
			);
		done;
		result := add !result (fact yi !produit);
	done;
	!result
;;

lagrange_aux [| 0.,2. ; 1.,1.; -1.,1. |];;

(* On cherche ensuite à afficher un joli string bien lisible *)

let entier f = floor f = ceil f;;

let affiche p =
	let rec aux = fun
		| n [] -> ""
		| n (0.::q) -> aux (n+1) q
		| 0 (t::q) when entier t -> ((aux 1 q)^(string_of_int (int_of_float t)))^" + "
		| 1 (t::q) when t=1. -> (aux 2 q)^"x + "
		| 1 (t::q) when t=(-1.) -> (aux 2 q)^"-x + "
		| 1 (t::q) when entier t -> ((aux 2 q)^(string_of_int (int_of_float t)))^"x + "
		| n (t::q) when t=1. -> ((aux (n+1) q)^"x^")^((string_of_int n)^" + ")
		| n (t::q) when t=(-1.) -> ((aux (n+1) q)^"-x^")^((string_of_int n)^" + ")
		| n (t::q) when entier t -> ((aux (n+1) q)^(string_of_int (int_of_float t)))^(("x^"^(string_of_int n))^" + ")
		| 0 (t::q) -> ((aux 1 q)^(string_of_float t))^" + "
		| 1 (t::q) -> ((aux 2 q)^(string_of_float t))^"x + "
		| n (t::q) -> ((aux (n+1) q)^(string_of_float t))^(("x^"^(string_of_int n))^" + ")
	in let a = aux 0 p in
	sub_string a 0 ((string_length a) -3)
;;

let lagrange_float t = affiche (lagrange_aux t);;

(* Et on finit par autoriser les points entiers *)

type points = 
	|N of (int*int) vect
	|R of (float*float) vect
;;

let lagrange = fun
	| (R(t)) -> lagrange_float t
	| (N(t)) -> let floating (a,b) = (float_of_int a),(float_of_int b) in
		  let n = vect_length t in
		  let v = make_vect n (0.,0.) in
		  for i=0 to (n-1) do
		  	v.(i) <- floating t.(i)
		  done;
		  lagrange_float v
;;

lagrange (N [| 0,0 ; 1,-1; -1,1 |]);;


(* On cherche ensuite transformer un polynome en sa fonction associée (f P)  *)


let rec f = fun
	| [] x -> 0.
	| (t::q) x -> t+.(x*.(f q x))
;;

(* Et on teste tout ca *)

let p = [1.;0.;5.;1.2;0.;0.;-4.];;

let pv = [| (0.,1.) ; (1.,3.2) ; (2.,-225.4) ; (-1.,0.8) ; (-1.5,-37.3625) ; (0.5,2.3375) ; (0.1,1.051196) |];;

let e = [| 0., exp 0. ; 1., exp 1. ; 2., exp 2. ; 3., exp 3. ; 4., exp 4. ; 5., exp 5. ; 6., exp 6. ; 7., exp 7. |];;










