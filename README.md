# NDArray
Mathematical package for multi-dimensional array.

Put NDArray.wl file to the notebook directory, and run
```mathematica
If[!MemberQ[$Path,NotebookDirectory[]],AppendTo[$Path,NotebookDirectory[]]]
Needs["NDArray`"];
```
to use the functions.

Most functions have the same name and input as numpy. The current functions include:
- Array creation
```mathematica
eye[n]
zeros[n1,n2,...]
ones[n1,n2,...]
fill[c,n1,n2,...]
rand[n1,n2,...]
linspace[start,stop,step]
kron[m1,m2,...]
```
- Sorting and searching
```mathematica
argmax[array]
argmin[array]
argsort[array]
argwhere[array,element]
```
- Manipulation
```mathematica
shape[array]
shape[array,n]
dims[array]
sum[array]
prod[array]
reshape[array,n1,n2,...]
moveaxis[array,{source},{destination}]

atleast1d[array]
atleast2d[array]
vstack[array1,array2,...]
hstack[array1,array2,...]
```
- Tensor contraction
```mathematica
einsum[{array1,array2,...},{{i1,j2},{i2,j2},...},{k1,k2,...}]
```
The tensor contraction ```einsum``` is a little different. For example, the following code
```mathematica
a = rand[2,3,4];
b = rand[5,4,3,7];
c = einsum[{a,b},{{2,6},{3,5}},{4,1,7}]
```
will return a ```5×2×7``` tensor.
- Spin operators
```mathematica
Sp[J]
Sm[J]
Sx[J]
Sy[J]
Sz[J]
S0[J]
```
- Vector space
```mathematica
ReduceSpace[vectorspace]
KrylovSpace[operator,vectorspace]
```
