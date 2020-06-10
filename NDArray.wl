(* ::Package:: *)

BeginPackage["NDArray`"];

eye::usage="eye[n]"
zeros::usage="zeros[n1,n2,...]"
ones::usage="ones[n1,n2,...]"
fill::usage="fill[c,n1,n2,...]"
rand::usage="rand[n1,n2,...]"
linspace::usage="linspace[start,stop,step]"

argmax::usage="argmax[array]"
argmin::usage="argmin[array]"
argsort::usage="argsort[array]"
argwhere::usage="argwhere[array,element]"

shape::usage="shape[array]"
sum::usage"sum[array]"
prod::usage"prod[array]"
reshape::usage="reshape[array,n1,n2,...]"
moveaxis::usage="moveaxis[array,{source},{destination}]"

atleast1d::usage="atleast1d[array]"
atleast2d::usage="atleast2d[array]"
vstack::usage="vstack[{array1,array2,...}]"
hstack::usage="hstack[{array1,array2,...}]"

einsum::usage="einsum[{array1,array2,...},{{i1,j2},{i2,j2},...},{k1,k2,...}]"

Sp::usage="Sp[J]"
Sm::usage="Sm[J]"
Sx::usage="Sx[J]"
Sy::usage="Sy[J]"
Sz::usage="Sz[J]"
S0::usage="S0[J]"

ReduceSpace::usage="ReduceSpace[vectorspace]"
KrylovSpace::usage="KrylovSpace[operator,vectorspace]"

Begin["`Private`"];

(*-----Array creation-----*)
eye[N_]:=IdentityMatrix[N];
zeros[shape__]:=ConstantArray[0,{shape}];
ones[shape__]:=ConstantArray[1,{shape}];
fill[c_,shape__]:=ConstantArray[c,{shape}];
rand[shape__]:=Array[RandomReal[]&,{shape}];
linspace[start_,stop_,num_]:=Range[start,stop,(stop-start)/(num-1)]

(*-----Sorting and searching-----*)
argmax[array_]:=Position[array,Max[array]];
argmin[array_]:=Position[array,Min[array]];
argsort[array_]:=Position[array,#]&/@Sort[DeleteDuplicates[array]]//Flatten;
argwhere[array_,ele_]:=Position[array,#]&/@ele;

(*-----Manipulation-----*)
(*Basic operations*)
shape[array_]:=Dimensions[array];
sum[array_]:=Total[Flatten[array]];
prod[array_]:=Module[{flat},
flat=Flatten[array];
If[Length[array]==1,flat[[1]],Fold[Times,flat[[1]],flat[[2;;]]]]
];
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

(*-----Spin operators-----*)
Sp[J_]:=Table[KroneckerDelta[n,m+1]*Sqrt[(J-m)(J+m+1)],{n,-J,J,1},{m,-J,J,1}];
Sm[J_]:=Table[KroneckerDelta[n,m-1]*Sqrt[(J+m)(J-m+1)],{n,-J,J,1},{m,-J,J,1}];
Sx[J_]:=(Sp+Sm)/2;
Sy[J_]:=(Sp-Sm)/(2*I);
Sz[J_]:=DiagonalMatrix[Range[-J,J,1]];
S0[J_]:=IdentityMatrix[2*J+1];

(*-----Vector space-----*)
ReduceSpace[vs_]:=Module[{v=vs,d,nv},
   d=Dimensions[v][[2]];
   nv=Simplify@NullSpace[v];
   If[nv=={},IdentityMatrix[d],Simplify@NullSpace[nv]]
];
KrylovSpace[H_,vs_]:=Module[{r,nr,h=H,v=vs},
   r=MatrixRank[v];
   v=ReduceSpace@Join[v,(h.#&)/@v];
   nr=Length[v];
   While[nr>r,r=nr;v=ReduceSpace@Join[v,(h.#&)/@v]; 
   nr=Length[v]];
   v
];
End[];

EndPackage[];
