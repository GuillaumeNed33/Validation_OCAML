(*****************************************************************************)
(*****************************************************************************)

(* servez-vous du manuel de rÃ©fÃ©rence ocaml disponible Ã  l'url:
http://caml.inria.fr/distrib/ocaml-4.02/ocaml-4.02-refman.txt *)

(* On dÃ©sire fabriquer une table associant des nombres Ã  virgules
flottantes Ã  des chaÃ®nes de caractÃ¨res. On codera une telle table Ã
l'aide d'une liste de couples (chaine, nombre). Il nous faudra
Ã©galement une fonction de recherche et une fonction
d'insertion. *)

type str2float_tbl = (string * float) list;;

let insert tbl str x = (str,x)::tbl;;

(* Q1 : commentez le type de la fonction insert, en particulier celui
du paramÃ¨tre tbl; modifiez sa definition de telle sorte que son
type soit str2float_tbl -> string -> float -> str2float_tbl;
a-t-on besoin de specifier le type des parametres str et x ? *)
let insert (tbl:str2float_tbl) (id:string) (x:float) :str2float_tbl = (id,x)::tbl

(* Q2 : Ã©crire la fonction de recherche 'search' dont le type doit
Ãªtre
search : str2float_tbl -> string -> float option
voir documentation pour le type 'option'. *)
exception NotFound;;
let rec search tbl id =
match tbl with
| [] -> raise NotFound
| (id', x)::tbl' -> if id=id' then x else search tbl' id;;

type 'a option =
None
| Some of 'a

let rec search2 tbl id =
match tbl with
[] -> None
| (id',x)::tbl' -> if id = id' then (Some x) else (search2 tbl' id);;

(* par exemple, (search [("x", 1.1) ; ("y", 1.3)] "y") doit retourner (Some 1.3) *)

(* Q3: le type option offre un moyen d'Ã©crire proprement des fonctions
partielles, le mÃ©canisme des exceptions en est un autre (voir
documentation), utilisez ce dernier pour ecrire une nouvelle
version de la fonction search, cette fois-ci de type search :
str2float_tbl -> string -> float *)

(* attention, c'est un peu subtil, cette fois (search [("x", 1.1) ; ("y", 1.3)] "y")
doit retourner 1.3 et non (Some 1.3) ! *)

(* Un autre concept fondamental est celui de type inductif. Il permet
Ã  l'utilisateur de dÃ©finir de nouveaux types comme union de
types. L'exemple suivant dÃ©finit un nouveau type (recursif) qui
code les expressions arithmÃ©tiques: *)

type expr =
Const of float
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Prod of expr * expr
| Quot of expr * expr

(* par exemple l'expression "3x + 2" sera codÃ©e par (Sum (Prod (Const 3.0, Var "x"), Const 2.0)) *)

(* Q4: dÃ©finir un Ã©lÃ©ment de type expr qui code l'expression
2x + 4y + 1 *)

let expr = Sum(Prod(Const 2.0, Var "x"), Sum(Prod(Const 4.0, Var "y"), Const 1.0));;
(* Q5 : Ã©crire une fonction qui transforme une expression en une
chaine de caractÃ¨re lisible *)

let rec string_of_expr e =
match e with
Const x -> string_of_float x
| Var id -> id
| Sum(e1 , e2) -> (string_of_expr e1) ^ "+" ^ (string_of_expr e2)
| Prod(e1,e2) -> (string_of_expr e1) ^ "*" ^ (string_of_expr e2)
;;

(* Q6: que fait la fonction suivante (on utilise ici la derniÃ¨re
version de search) ? *)

let rec eval tbl e =
match e with
Const x -> x
| Var id -> search tbl id
| Sum (e1,e2) -> (eval tbl e1) +. (eval tbl e2)
| Diff (e1,e2) -> (eval tbl e1) -. (eval tbl e2)
;;
(* Q7: Commentez le warning emis par l'interpreteur, essayez cette
fonction avec l'expression definie prÃ©cÃ©demment. Corrigez le code de
eval pour eliminer le warning en question *)

(* WARNING CAR TOUS LES CAS NE SONT PAS GEREE*)

(* Q8: ecrire une fonction qui calcule l'expression de la dÃ©rivÃ©e d'une expression de
type expr relativement Ã  une variable dont l'identifiant sera donnÃ© par une chaine
de caractÃ¨re. Attention, son type sera string -> expr -> expr (la
fonction aura deux arguments, l'identifiant de la variable par
rapport Ã  laquelle on dÃ©rive et l'expression Ã  dÃ©river  *)

let rec derive e =
match e with
  Const x -> Const 0.0
| Sum(e1,e2) -> Sum((derive e1),(derive e2))
| Diff(e1,e2) -> Diff((derive e1),(derive e2))
| Prod(e1,e2) -> Sum(Prod(derive e1, e2), Prod(e1,derive e2))
| Quot(e1,e2) -> Quot(Diff(Prod(derive e1,e2), Prod(e1, derive e2)), Prod(e1,e2))
| Var x -> Const 1.0
;;

(* Q9: Ã©crire une fonction qui simplifie une expression. Par exemple, la dÃ©rivÃ©e
de l'expression donnÃ©e en Q4 par rapport Ã  x devrait Ãªtre simplifiÃ©e en "Const 2.0" ... *)

let rec simplif e =
match e with
| Sum(e1, e2) -> expr
| _ -> expr2
|

(*****************************************************************************)
(*****************************************************************************)
