# Cluster Variables, Path Posets, and the HOMFLY Polynomial

### Companion code to [this paper]().

More detail (including the definitions in `Specializations.hs`) will be added once the pdf is posted.

## Examples

```{haskell}
> evenCF (38/11)
[4,-2,6]
```
```{haskell}
> poset [2,3,-4,2,3,1]
  2                       14
 / \                     /  
1   3   6              13   
     \ / \            /     
      4   7   10    12      
           \ /  \  /        
            8    11         
```
```{haskell}
> poset [2,3]
  2    
 / \   
1   3  
     \ 
      4
> ideals (poset [2,3])
[[],[1],[4],[1,4],[3,4],[1,3,4],[1,2,3,4]]
> sFJones [2,3]
1 + 2t^(-2) - t^(-1) - t^(-3) + t^(-4) - t^(-5)
```
