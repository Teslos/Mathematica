(* ::Package:: *)

(* the initial algebra for the specification Nat1 *)
(* sorts *)
int
bool
(* Sigma *)
{z}    (* \[Rule] int *)
{t,f}  (* \[Rule] bool *)
{s}    (* int\[Rule]int *)
{isz}  (* int\[Rule] bool *)
{not}  (* bool\[Rule] bool *)
{add,mult} (* int int \[Rule] int *)

(* equations turned into rewrite rules *)
isz[z] := t
isz[s[n_]] := f

not[t] := f
not[f] := t

add[n_,z] := n
add[n_,s[m_]] := s[add[n,m]]

mult[n_,z] := z
mult[n_,s[m_]] := add[mult[n,m],n]
