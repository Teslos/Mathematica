(* ::Package:: *)

BeginPackage["Modular`"]
SetModulus::usage="SetModulus[p] sets the modulus to be used."
MakeModular::usage="MakeModular[n] returns n mod p."
Representitive::usage="Representative[m] returns a representative of the modular number m."

Begin["`Private`"]
`theModulus   (* a private static variable *)
`mod          (* the private name of the data type *)

(* constructors *)
MakeModular[n_Integer] := mod[ Mod[n, theModulus] ]
SetModulus[p_Integer?Positive] := (theModulus = p)

(* Selectors *)
Representative[mod[n_]] := n
(* Arithmetic *)
mod/: a_mod + b_mod := MakeModular[ Representative[a] + Representative[b] ]
mod/: a_mod * b_mod := MakeModular[ Representative[a] * Representative[b] ]
mod/: a_mod ^ q_Integer := MakeModular[ PowderMod[Representative[a],q,theModulus] ]

mod/: a_mod + b_Integer := a + MakeModular[b]
mod/: a_mod * b_Integer := a * MakeModular[b]

(* Output Formatting *)
Format[m_mod] := SequenceForm[Representative[m]," mod ", theModulus]
End[]
EndPackage[]
