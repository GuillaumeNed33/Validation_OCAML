(* Par exemple, il peut evaluer une expression arithmétique: *)
1+2+4;;  (* une expression doit être terminée par ';;' *)

(* Ou bien évaluer une une expression qui définit une constante entière: *)
let pi = 3;;

(* ou bien une chaine de caractère: *)
let mon_nom = "toto";;

(* en encore, une fonction: *)
let carre x = x * x;;

(* que l'on peut appliquer à un nombre: *)
carre 3;;

(*** Q1: chaque valeur définie par 'let' possède un type. Quel est le
type de carre ? Selon vous, comment ce type a-t-il été calculé par
ocaml ? ***)
(*
Le type des variables définies par let est un type fonction.
*)

(* on peut egalement définir une fonction à plusieurs arguments: *)
let dist x y = sqrt (x *. x +. y *. y);;

(*** QUESTION 2 ***)
let dist x y = sqrt ( (carre x) + (carre y));;
(*
MARCHE PAS, erreur typage int à la place de float
*)

(*** QUESTION 3 ***)
let carre_float x = x *. x;;
let dist x y = sqrt ( (carre_float x) +. (carre_float y));;
(*
Marche car il n'y a que des float (passer des float en arguments)
*)


(* Les fonction peuvent être définie de façon récursive, dans ce cas,
le mot clé 'rec' doit être utilisé après le 'let'. *)
let rec fact n = if n=0 then 1 else n * fact (n-1) ;;

(* ou bien: *)
let rec fact =
function
| 0 -> 1
| n -> n * fact (n-1);;

(*** Q4: definir une fonction qui calcule la suite de Fibbonacci: ***)
let rec fibo =
function
| 0 -> 0
| 1 -> 1
| n -> (fibo(n-1)) + (fibo (n-2));;

(* un type de donnée fondamental est celui des listes: *)
let phrase = ["C'est"; "chouette"; "le"; "S4" ];;

(* on peut concatener deux listes :*)
let l' = [1;2;3] @ [4;5;6];;

(* ou bien ajouter un élément en tête de liste: *)
let l' = 1 :: [2;3;4;5;6];;

(* pour compter le nombre d'éléments d'une liste, on peut proceder comme suit: *)
let rec taille l =
match l with
| [] -> 0
| x::l' -> 1 + (taille l');;

(*** Q5: écrire une fonction d'insertion d'un élément dans une liste triée ***)
let rec insert elem liste =
match liste with
|  [] -> [elem]
|  tete::queue ->
if elem <= tete then elem :: liste
else tete :: (insert elem queue);;

(*** Q6: écrire une fonction de tri par insertion ***)
let rec tri_insertion =
function
|  [] -> []
|  tete::queue -> insert tete (tri_insertion queue);;


(* Les fonctions sont des valeurs comme les autres, on peut les
utiliser comme paramètres par exemple. *)
let rec existe f l =
match l with
[] -> false
| x::l' -> if (f x) then true else existe f l';;

(*** Q7: en utilisant la fonction précédente, écrire une fonction booléenne
qui exprime le fait qu'une liste comporte un élément positif ***)
let rec positif =
function
| 0 -> false
| tete -> if (tete > 0) then true else false;;

(*** Q8: avec l'aide de la documentation de ocaml (distribuée en version
texte), utilisez la fonction fold_left de la bibliothèque List et
la liste 'phrase' pour afficher -- C'est chouette le S4 -- ***)
print_string("C'est chouette le S4!");;
