## Given the split matrices of 2 trees
## find the common edge shared by the 2 trees 
## @ parameter t1.split.mat is the tree split matrix of tree 1 
##   (number of interior edges of tree 1 by number of leaves)
## @ parameter t2.split.mat is athe tree split matrix of tree 2 
##  (number of interior edges of tree 2 by number of leaves)
## @ return is an index matrix of the common edges 
##   NULL if there is no common edges 
##   else return a number of common edges by 2 matrix
##   each column comtains the row index of the corresponding tree split matrix 



find.common.edge <- function(t1.split.mat,t2.split.mat){
  
  common.edge.idx = NULL
  
  if(!is.null(t1.split.mat)&&!is.null(t2.split.mat)){
    for (i in 1:nrow(t1.split.mat)){
      
      for (j in 1:nrow(t2.split.mat)){
        
        edge.diff = t1.split.mat[i,]-t2.split.mat[j,]
        
        if(sum(abs(edge.diff))==0){
          common.edge.idx = rbind(common.edge.idx,c(i,j))
        }
      }
    }
  }
  
  return(common.edge.idx)
  
}
