(* ::Package:: *)

BeginPackage["NDArray`"];
(*Public functions*)
Eye::usage="Eye[n]"
Zeros::usage="Zeros[n1,n2,...]"
Rand::usage="Rand[n1,n2,...]"
Linspace::usage="Linspace[start,stop,step]"
Kron::usage="Kron[m1,m2,...]"

Argmax::usage="Argmax[array]"
Argmin::usage="Argmin[array]"
Argsort::usage="Argsort[array]"
Argwhere::usage="Argwhere[array,element]"
Where::usage="Where[array,cond]"

shape::usage="shape[array]"
shape::usage="shape[array,n]"
ndims::usage="ndims[array]"
sum::usage"sum[array]"
prod::usage"prod[array]"
reshape::usage="reshape[array,n1,n2,...]"
moveaxis::usage="moveaxis[array,{source},{destination}]"

atleast1d::usage="atleast1d[array]"
atleast2d::usage="atleast2d[array]"
vstack::usage="vstack[array1,array2,...]"
hstack::usage="hstack[array1,array2,...]"

einsum::usage="einsum[{array1,array2,...},{{i1,j2},{i2,j2},...},{k1,k2,...}]"

Sp::usage="Sp[J]"
Sm::usage="Sm[J]"
Sx::usage="Sx[J]"
Sx::usage="Sx[J,i,n]"
Sy::usage="Sy[J]"
Sy::usage="Sy[J,i,n]"
Sz::usage="Sz[J]"
Sz::usage="Sz[J,i,n]"
S0::usage="S0[J]"

ReduceSpace::usage="ReduceSpace[vectorspace]"
KrylovSpace::usage="KrylovSpace[operator,vectorspace]"

savetxt::usage="savetxt[dir,vector]"
loadtxt::usage="loadtxt[dir]"

Begin["`Private`"];

(*-----Array creation-----*)
Eye[N_]:=IdentityMatrix[N];
Zeros[shape__]:=ConstantArray[0,{shape}];
Rand[shape__]:=Array[RandomReal[]&,{shape}];
Linspace[start_,stop_,num_]:=Range[start,stop,(stop-start)/(num-1)];
Kron[mats__]:=KroneckerProduct[mats];

(*-----Sorting and searching-----*)
Argmax[array_]:=Block[{pos},
    pos=FirstPosition[array,Max[array]];
    If[ArrayDepth[array]==1,pos[[1]],pos]
];
Argmin[array_]:=Block[{pos},
    pos=FirstPosition[array,Min[array]];
    If[ArrayDepth[array]==1,pos[[1]],pos]
];
Argsort[array_]:=Position[array,#]&/@Sort[DeleteDuplicates[array]]//Flatten;
Argwhere[array_,ele_]:=Module[{d,de,pos,trim},
    d=ArrayDepth[array];
    de=ArrayDepth[ele];
    pos=If[de==0,Position[array,ele],Position[array,#]&/@ele];
    If[d==1,
        trim=Transpose[#][[1]]&;
        If[de==0,pos=trim[pos],pos=trim/@pos],
        pos
    ]
];
Where[array_,cond_]:=Flatten@Position[cond/@array,True];

(*-----Manipulation-----*)
(*Basic operations*)
shape[array_]:=Dimensions[array];
shape[array_,n_]:=Dimensions[array,n];
ndims[array_]:=ArrayDepth[array];
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
   Transpose[array,k0]
];
(*Changing number of dimensions*)
atleast1d[array_]:=If[Length[array]==0,{array},array];
atleast2d[array_]:=Module[{depth},
   depth=ArrayDepth[array];
   Piecewise[{{{{array}},depth==0},{{array},depth==1}},array]
];
(*Joining arrays*)
vstack[tup__]:=Join@@atleast2d/@{tup};
hstack[tup__]:=Transpose@vstack@Transpose@*atleast2d/@{tup};

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
Sp[J_]:=Table[KroneckerDelta[n,m+1]*Sqrt[(J-m)(J+m+1)],{n,J,-J,-1},{m,J,-J,-1}];
Sm[J_]:=Table[KroneckerDelta[n,m-1]*Sqrt[(J+m)(J-m+1)],{n,J,-J,-1},{m,J,-J,-1}];
Sx[J_]:=(Sp[J]+Sm[J])/2;
Sy[J_]:=(Sp[J]-Sm[J])/(2*I);
Sz[J_]:=DiagonalMatrix[Range[J,-J,-1]];
S0[J_]:=IdentityMatrix[2*J+1];
Sx[J_,i_,n_]:=Module[{lmat,rmat,s},
   s=2*J+1;
   lmat=eye[s^(i-1)];
   rmat=eye[s^(n-i)];
   kron[lmat,Sx[J],rmat]
];
Sy[J_,i_,n_]:=Module[{lmat,rmat,s},
   s=2*J+1;
   lmat=eye[s^(i-1)];
   rmat=eye[s^(n-i)];
   kron[lmat,Sy[J],rmat]
];
Sz[J_,i_,n_]:=Module[{lmat,rmat,s},
   s=2*J+1;
   lmat=eye[s^(i-1)];
   rmat=eye[s^(n-i)];
   kron[lmat,Sz[J],rmat]
];

(*-----Vector space-----*)
ReduceSpace[vs_]:=Module[{rv},rv=RowReduce[vs];rv[[1;;MatrixRank[rv],;;]]];
KrylovSpace[H_,vs_]:=Module[{r,nr,h=H,v=vs},
   r=MatrixRank[v];
   v=ReduceSpace@Join[v,(h.#&)/@v];
   nr=Length[v];
   While[nr>r,r=nr;v=ReduceSpace@Join[v,(h.#&)/@v]; 
   nr=Length[v]];
   v
];

(*-----Data IO-----*)
savetxt[dir_,vec_]:=Module[{f,n},
   f=OpenWrite[dir,FormatType->OutputForm];
   n=Length[vec];
   Do[Write[f,vec[[i]]],{i,1,n}];
   Close[f];
];
loadtxt[dir_]:=Module[{f,data},
   f=OpenRead[dir];
   data=ReadList[f];
   Close[f];
   data
];

End[];

EndPackage[];
