(* ::Package:: *)

BeginPackage["NDArray`"];

eye
zeros
ones
fill
rand
linspace
sum
prod
argmax
argmin
argsort
reshape
einsum

Begin["`Private`"];

(*Array creation*)
eye[N_]:=IdentityMatrix[N];
zeros[shape__]:=ConstantArray[0,{shape}];
ones[shape__]:=ConstantArray[1,{shape}];
fill[c_,shape__]:=ConstantArray[c,{shape}];
rand[shape__]:=Array[RandomReal[]&,{shape}];
linspace[start_,stop_,num_]:=Range[start,stop,(stop-start)/(num-1)]
sum[array_]:=Total[array];
prod[array_]:=Module[{flat},
flat=Flatten[array];
If[Length[array]==1,flat[[1]],Fold[Times,flat[[1]],flat[[2;;]]]]
];

(*Sort*)
argmax[array_]:=Position[array,Max[array]];
argmin[array_]:=Position[array,Min[array]];
argsort[array_]:=Position[array,#]&/@Sort[DeleteDuplicates[array]]//Flatten;

(*Indices*)
reshape[array_,shape__]:=Module[{pos,newshape={shape}},
pos=Position[newshape,-1];
If[pos!={},newshape[[pos[[1]]]]=-prod[Dimensions[array]]/prod[newshape]];
ArrayReshape[array,newshape]
];

(*Manipulation*)
einsum[Ts_,s_:{}]:=Activate@TensorContract[Inactive[TensorProduct]@@Ts,s];
einsum[Ts_,s_:{},{}]:=einsum[Ts,s];
einsum[Ts_,s_:{},ex:{_Integer..}]:=Activate@Transpose[TensorContract[Inactive[TensorProduct]@@Ts,s],Ordering@ex];
einsum[Ts_,s_:{},ex:{_List..}]:=Module[{ten,dim,shape},
ten=einsum[Ts,s,Flatten@ex];
dim=Dimensions[ten];
shape=Times@@Take[dim,#]&/@MapThread[{#1+1,#2}&,Through@{Most,Rest}@Prepend[Accumulate[Length/@ex],0]];
ArrayReshape[If[ArrayDepth[ten]==0,{ten},ten],shape]
];

End[];

EndPackage[];
