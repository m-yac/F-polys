# Cluster Algebras and the HOMFLY Polynomial

### Companion code to [this paper]().

This code is meant to be used in an interactive session. Try it out [here]()! (or clone this repo and run `ghci Main.hs`)

The file `homfly200c.txt` comes from [this website](http://www.pdmi.ras.ru/~arnsem/dataprog/), which is the result of the paper ["A formula for the HOMFLY polynomial of rational links" by Sergei Duzhin and Mikhail Shkolnikov](https://arxiv.org/abs/1009.1800).

## Examples

```haskell
> evenCF (38/11)
[4,-2,6]
```
```haskell
> poset [2,3,-4,2,3,1]
  2                       14
 / \                     /  
1   3   6              13   
     \ / \            /     
      4   7   10    12      
           \ /  \  /        
            8    11         
```
```haskell
> poset [2,3]
  2    
 / \   
1   3  
     \ 
      4
> ideals (poset [2,3])
[[],[1],[4],[1,4],[3,4],[1,3,4],[1,2,3,4]]
> Jones.sF [2,3]
1 + 2t^(-2) - t^(-1) - t^(-3) + t^(-4) - t^(-5)
```
```haskell
> HOMFLY.lookup [2] -- from Duzhin-Shkolnikov
- l^(-3) z^(-1) + l^(-1) z^(-1) + l^(-1) q^(1/2) - l^(-1) q^(-1/2)
> HOMFLY.msF [2] -- = m[2] sF[2], identical to the above!
- l^(-3) z^(-1) + l^(-1) z^(-1) + l^(-1) q^(1/2) - l^(-1) q^(-1/2)
```

The functions `checkHOMFLY`, `checkAlexander`, and `checkJones` provide a summary of these values:

```haskell
> checkHOMFLY [-2]
Even CF: [-2] = (-2)
 1
Ideals: [], [1]
Specialized F-poly:      1 + q^(-2) w
Normalized Spec. F-poly: - l q^(1/2) - l z^(-1) + l^3 z^(-1) + l q^(-1/2)
HOMFLY Polynomial:       - l z^(-1) + l^3 z^(-1) - l q^(1/2) + l q^(-1/2)
```
```haskell
> checkAlexander [2,-4,2]
Even CF: [2,-4,2] = 12/7
       5  
      / \ 
 1   4   7
  \ /     
   3      
Ideals: [], [3], [7], [1,3], [3,4], [3,7], [1,3,4], [1,3,7], [3,4,7], [1,3,4,7], [3,4,5,7], [1,3,4,5,7]
Specialized F-poly:      2 - 4t^(-1) + 4t^(-2) - 2t^(-3)
Normalized Spec. F-poly: - 4t^(1/2) + 4t^(-1/2) + 2t^(3/2) - 2t^(-3/2)
Alexander Polynomial:    - 4t^(1/2) + 4t^(-1/2) + 2t^(3/2) - 2t^(-3/2)
(sgn(c0)t^e0 = t^(3/2), m[..] = t^(3/2))
```

Note that sometimes the values of `HOMFLY.lookup` and `HOMFLY.msF` may not look identical, e.g.
```haskell
> checkHOMFLY [2,2]
Even CF: [2,2] = 5/2
   2  
  / \ 
 1   3
Ideals: [], [1], [3], [1,3], [1,2,3]
Specialized F-poly:      1 + l^2 w^(-1) - q^(-1) - l^2 q^(-1) w^(-1) + l^4 w^(-1)
Normalized Spec. F-poly: l^(-2) q^(1/2) z^(-1) - q^(1/2) z^(-1) - q + 2 - l^(-2) q^(-1/2) z^(-1) + q^(-1/2) z^(-1) - q^(-1) + l^2
HOMFLY Polynomial:       l^(-2) + 1 + l^2 - q - q^(-1)
```
But, using the fact that `z = q^(1/2) - q^(-1/2)`, a computer algebra system can do the rest (e.g. Wolfram|Alpha can do it for [this case](https://www.wolframalpha.com/input/?i=simplify+l%5E%28-2%29+q%5E%281%2F2%29+z%5E%28-1%29+-+q%5E%281%2F2%29+z%5E%28-1%29+-+q+%2B+2+-+l%5E%28-2%29+q%5E%28-1%2F2%29+z%5E%28-1%29+%2B+q%5E%28-1%2F2%29+z%5E%28-1%29+-+q%5E%28-1%29+%2B+l%5E2%2C+z+%3D+q%5E%281%2F2%29+-+q%5E%28-1%2F2%29)).
