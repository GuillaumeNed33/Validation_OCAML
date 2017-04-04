(*****************************************************************************)
(*****************************************************************************)

(* definition des arbres *)
type tree =
    Feuille of int
  | Noeud of tree * tree
;;

(* un exemple *)
let t =
  Noeud (Noeud (Feuille 1, Feuille 3),
	Noeud (Noeud (Feuille 1, Feuille 2), Noeud(Feuille 5, Feuille 10)))
;;

(* parcourt en profondeur *)
let rec list_of_tree t =
  match t with
    Feuille n -> [n]
  | Noeud (t1,t2) ->
      (list_of_tree t1) @ (list_of_tree t2)
;;

(* test *)
list_of_tree t;;

(* Q1: écrire une fonction qui calcule le nombre de noeuds d'un arbre *)

let rec count_noeud_tree t =
match t with
Feuille n -> 0
| Noeud (t1,t2) -> 1 + (count_noeud_tree t1) + (count_noeud_tree t2);;

(* Q2: écrire une fonction qui calcule la hauteur d'un arbre *)
let rec high_tree t =
match t with
Feuille _ -> 1
| Noeud(t1,t2) -> (max(high_tree t1) (high_tree t2)) + 1
;;

(* Q3: ajoutez des étiquettes aux noeuds internes et réécrire les
fonctions en conséquences *)

(*ON LAISSE BETON MEC*)

(* Q4: écrire une fonction qui à partir d'un arbre donné, construit un
   nouvel arbre obtenu en conservant la même forme, mais en ajoutant 1
   à toutes les étiquettes. *)

   let rec addEtiquette t =
   match t with
   Feuille n -> Feuille (n+1)
   | Noeud (t1,t2) -> Noeud(addEtiquette t1, addEtiquette t2)
   ;;

(* remarque *)

(* le maximum des d'une liste *)
let rec max l =
  match l with
    [] -> None
  | x::l' ->
      (match max l' with
	None -> Some x
      |	Some m' ->
	  if m' > x then Some m' else Some x)
;;

max [1;2;3];;
max [];;

(*****************************************************************************)
(*****************************************************************************)
