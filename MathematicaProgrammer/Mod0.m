(* ::Package:: *)

makemod[n_Integer] := mod[ Mod[n,p]]
rep[ mod[n_] ] := n

mod/: m1_mod + m2_mod := makemod[ rep[m1] + rep[m2] ]
mod/: m1_mod * m2_mod := makemod[ rep[m1] * rep[m2] ]  
