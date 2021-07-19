# GTP
The Geodesic Treepath Problem (GTP) algorithm is a polynomial algorithm to compute geodesic distance between two phylogenetic trees in tree space [1]. The algorithm is proposed in [2].

## Two Examples 

### 1. Geodesic computation 

This example computes the geodesic from the starting tree and ending tree. For example, you may choose the folloing trees as your starting/enging tree, which has 6 leaves. 

Starting tree: (1:0.86,(2:0.58,3:0.30,4:0.97,5:0.50,6:0.86):0.41,0:0)

Ending tree: (((1:0.87,2:0.12,3:0.20):0.14,4:0.50):0.54,(5:0.63,6:0.22):0.42,0:0)

The algorithm calculates the geodesic path in tree space and then further calculates the geodesic distance. 

### 2. Treepath interpolation

This part aims to interpret the intermediate trees in between the staring tree and ending tree. Given any $\theta\in [0,1]$, the algorithm returns the intermediate tree.  


## Rederences
[1] Billera, L. J., Holmes, S. P., & Vogtmann, K. (2001). Geometry of the space of phylogenetic trees. Advances in Applied Mathematics, 27(4), 733-767.
[2] Owen, M., & Provan, J. S. (2010). A fast algorithm for computing geodesic distances in tree space. IEEE/ACM Transactions on Computational Biology and Bioinformatics, 8(1), 2-13.
