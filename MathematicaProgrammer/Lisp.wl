(* ::Package:: *)

BeginPackage["Lisp`"]
cons::usage = "cons[a,b] gives the dotted pair (a.b)."
car::usage = "car[pair] gives the first element of pair."
cdr::usage = "cdr[pair] gives the second element of pair."
list::usage = "list[e1,e2,...,en] gives the List list(e1 e2 ... en)."

nil::usage = "nil is the empty List list."

pairQ::usage = "pairQ[e] is true, if e is a dotted pair."
atomQ::usage = "atomQ[e] is true, if e is an atom."
nullQ::usage = "nullQ[l] is true, if l is the empty list."

Begin["`Private`"]

$RecursionLimit = Infinity (* necessary for long lists *)

(* constructors *)
list[] = nil
list[e_, r___] := cons[e, list[r]]

(* selectors *)
car[cons[e_, l_]] := e
cdr[cons[e_, l_]] := l

(* predicates *)
atomQ[_?AtomQ] = True
atomQ[_cons] = False   (* leave undefined otherwise *)
pairQ[e_] := !atomQ[e]
nullQ[nil] = True
nullQ[_cons] = False

(* output formats *)
listToNest[nil] := {}
listToNest[c_cons] := {car[c],listToNest[cdr[c]]}
listToNest[l_] := Flatten[listToNest[l]]

listQ[nil] = True  (* this atom is also a list *)
listQ[l_cons] := listQ[cdr[l]]
listQ[_] = False

Format[nil] = "()"
Format[l_cons?listQ] := SequenceForm["(", Infix[listToList[l]," "], ")"]
Format[l_cons] := SequenceForm["(",Infix[l, " . " ], ")"]

(* fix Infix problem for functions with one argument *)
protected = Unprotect[Infix]
Infix[_[e_], h_:Null] := e
Protect[Evaluate[protected]]

End[]
Protect[ car, cdr, cons, pairQ, atomQ, nullQ, list ]
EndPackage[]
 
