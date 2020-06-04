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
argwhere

shape
reshape
moveaxis

atleast1d
atleast2d
vstack
hstack

einsum

Begin["`Private`"];

(*-----Array creation-----*)
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

(*-----Sorting and searching-----*)
argmax[array_]:=Position[array,Max[array]];
argmin[array_]:=Position[array,Min[array]];
argsort[array_]:=Position[array,#]&/@Sort[DeleteDuplicates[array]]//Flatten;
argwhere[array_,ele_]:=Position[array,#]&/@ele;

(*-----Manipulation-----*)
(*Basic operations*)
shape[array_]:=Dimensions[array];
(*Changing array shape*)
reshape[array_,shape__]:=Module[{pos,newshape={shape}},
pos=Position[newshape,-1];
If[pos!={},newshape[[pos[[1]]]]=-prod[Dimensions[array]]/prod[newshape]];
ArrayReshape[array,newshape]
];
(*Transpose-like operations*)
moveaxis[array_,source_,destination_]:=Module[{k0,order,pos,s,d},
k0=Range[ArrayDepth[array]];
order=argsort[destination];
s=source[[order]];
d=destination[[order]];
pos=argwhere[k0,d]//Flatten;
k0=Delete[k0,{pos}//Transpose];
For[i=1,i<=Length[d],i++,k0=Insert[k0,d[[i]],s[[i]]]];
Print[k0];
Transpose[array,k0]
];
(*Changing number of dimensions*)
atleast1d[array_]:=If[Length[array]==0,{array},array];
atleast2d[array_]:=Module[{depth},
depth=ArrayDepth[array];
Piecewise[{{{{array}},depth==0},{{array},depth==1}},array]
];
(*Joining arrays*)
vstack[tup_]:=Join@@atleast2d/@tup;
hstack[tup_]:=Transpose@*atleast2d/@tup//vstack//Transpose;

(*-----Tensor-----*)
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
