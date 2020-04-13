## code interior edge via tree split
## @ parameter tip.int.node.label is a matrix (number of leaves by number of interior nodes)
## @ parameter int.edge.list is a matrix (number of interior edges by 2)
## @ return edge.split.mat is a matrix (nummber of interior edges by nummber of leaves)
##  with 1 indicates tree leaves below the tree level of the interior edge 
##  and 0 else



edgeSplit <- function(tip.int.node.label,int.edge.list){
  
  if(is.null(tip.int.node.label)){
    stop('No interior edges.')
  }else if(is.null(int.edge.list)){
    stop('No interior edges.')
  }else{
    Ntip = nrow(tip.int.node.label)
    
    Nedge = nrow(int.edge.list)
    
    edge.split.mat = matrix(0,nrow=Nedge,ncol=Ntip)
    
    for (i in 1:Nedge){
      # get the leave label with level below the interior edge 
      lower.idx1 = vector()
      lower.idx2 = vector()
      for (j in 1:Ntip){
        # starting interior node of the edge
        buff = which(tip.int.node.label[j,]==int.edge.list[i,1])
        if(length(buff>0)){
          lower.idx1 = c(lower.idx1,j)
        }
        # ending interior node of the edge
        buff = which(tip.int.node.label[j,]==int.edge.list[i,2])
        if(length(buff>0)){
          lower.idx2 = c(lower.idx2,j)
        }
      }
      
      lower.idx = intersect(lower.idx1,lower.idx2)
      edge.split.mat[i,lower.idx] = rep(1,length(lower.idx))
    }
  }
  
  return(edge.split.mat=edge.split.mat)
  
}
