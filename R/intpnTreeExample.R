## get interpolation tree on the geodesic that 
## travels from t1 to t2

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

## theta in [0,1]
theta = 0.8

# get the interpolation tree 
intpn.tree = find.intpn.tree(theta,t1,t2)

## test whether the interpolation tree is the correct one 
## we calculate the geodesic distance between t1 and intpn.tree

## geodesic distance 
geo.dist = get.path.geodist(t1,t2)$geo.dist

part.dist.test = get.path.geodist(t1,intpn.tree)$geo.dist-theta*geo.dist 

if(abs(part.dist.test)>1e-3){
  stop('The interpolation tree is not correct!')
}
