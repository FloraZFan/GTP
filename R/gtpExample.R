## testing example for GTP algorithm

require(igraph)

# load tree files
treeFile = './tree file.txt' 

# read newick format trees
## option 1: load a tree file using function read.newick()
tree.data = read.newick(treeFile)

# choose starting tree index in the tree file
t1.idx = 1 
# choose ending tree index in the tree file
t2.idx = 2 

# starting tree
t1 = tree.data[[t1.idx]] 

# ending tree 
t2 = tree.data[[t2.idx]] 

## option 2: read a given newick form tree using function newick()

## t1 = newick('(1:0.86,(2:0.58,3:0.30,4:0.97,5:0.50,6:0.86):0.41,0:0);')

## t2 = newick('(((1:0.87,2:0.12,3:0.20):0.14,4:0.50):0.54,(5:0.63,6:0.22):0.42,0:0);')

# calculate geodesic distance between t1 and t2

path.dist.cal = get.path.geodist(t1,t2)

## geodesic distance 
geo.dist = path.dist.cal$geo.dist

## tree path (matrix form) from t1 to t2
## nrow: number of orthants that tree travels
## ncol: number of edges in t1 and t2
path = path.dist.cal$path.mat
 
