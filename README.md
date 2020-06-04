# NDArray
Mathematical package for multi-dimensional array.

Put NDArray.wl file to the notebook directory, and run
```mathematica
AppendTo[$Path, NotebookDirectory[]];
Needs["NDArray`"];
```
to use the functions.

Most functions have the same name and input as numpy:
```python
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
```
The tensor contraction ```einsum``` is a little different. For example, the following code
```mathematica
a = rand[2,3,4];
b = rand[5,4,3,7];
c = einsum[{a,b},{{2,6},{3,5}},{4,1,7}]
```
will return a $5 \times 2 \times 7$ tensor.
