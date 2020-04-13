## Given a tree split and an edge 
## split the tree into two parts without the given edge
## @ parameter tree.split.mat is a tree split matrix 
## @ parameter comm.edge.idx is an edge index 
## @ return sub.split.idx is a list of two trees 


subtree.split <- function(tree.split.mat,comm.edge.idx){
  
  nLeaves = ncol(tree.split.mat)
  
  lower.node.label = which(tree.split.mat[comm.edge.idx,]==1)
  
  
  upper.node.label = setdiff(1:nLeaves,lower.node.label)
  
  ## sub.tree1 is the tree keeps the upper tree structure
  sub1.split.idx = NULL
  if(length(upper.node.label)>0){
    
    for (tt in setdiff(1:nrow(tree.split.mat),comm.edge.idx)){
      buff= which(tree.split.mat[tt,upper.node.label]==1)
      if(length(buff)>0&&length(buff)<length(upper.node.label)){
        sub1.split.idx = c(sub1.split.idx,tt)
      }
    }
  }
  
  
  
  ## sub.tree2 is the tree keeps the lower tree structure
  sub2.split.idx = NULL
  
  if(length(lower.node.label)>0){
    for (tt in setdiff(1:nrow(tree.split.mat),comm.edge.idx)){
      buff= which(tree.split.mat[tt,lower.node.label]==1)
      if(length(buff)>0&&length(buff)<length(lower.node.label)){
        sub2.split.idx = c(sub2.split.idx,tt)
      }
    }
  }
  
  
  ## test the subtree splits correct or not
  if(is.null(sub1.split.idx)){
    row.test1 = 0 
  }else{ 
    row.test1 = length(sub1.split.idx) 
  }
  
  if(is.null(sub2.split.idx)){
    row.test2 = 0 
  }else{ 
    row.test2 = length(sub2.split.idx) 
  }
  
  if(nrow(tree.split.mat)!=row.test1+row.test2+length(comm.edge.idx)){
    stop('Subtree splits error!')
  }
  
  sub.split.idx = list(sub1.split.idx,sub2.split.idx)
  
  return(sub.split.idx)
  
}