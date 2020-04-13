## find tree spaces that geodesic travels from t1 to t2
## @ parameter bipar.mat: noncommon edges bipartition 
## @ nEdge1: number of edges in t1
## @ nEdge2: number of edges in t2
## @ drop.set: edge index in t1 to be dropped 
## @ add.set: edge index in t2 to be added
## @ return path.mat is a matrix of tree path
##    nrow: number of othants tha geodesic travels 
##    ncol: nEdge1+nEdge2

find.tree.path <- function(bipar.mat,nEdge1,nEdge2,drop.set,add.set){
  path.mat = NULL
  
    ## when we have common edges to drop some edges from t1
    if(!is.null(drop.set)&&length(drop.set)>0){
      for(i in 1:length(drop.set)){
        path.mat = matrix(c(rep(1,nEdge1),rep(0,nEdge2)),nrow=1)
        path.mat = rbind(path.mat,path.mat[nrow(path.mat),])
        path.mat[nrow(path.mat),drop.set[[i]]] = rep(0,length(drop.set[[i]]))
      }
      
    }else{
      path.mat = matrix(c(rep(1,nEdge1),rep(0,nEdge2)),nrow=1)
    }
    
    ## the sets Ak and Bk we have from bipartite procedure 
    if(!is.null(bipar.mat)){
      for (i in 1:nrow(bipar.mat)){
        
        drop.idx = which(bipar.mat[i,1:nEdge1]==1)
        add.idx = which(bipar.mat[i,-(1:nEdge1)]==1)+nEdge1
        
        if(length(drop.idx)>0){
          path.mat = rbind(path.mat,path.mat[nrow(path.mat),])
          path.mat[nrow(path.mat),drop.idx] = rep(0,length(drop.idx))
          
          if(length(add.idx)>0){
            path.mat[nrow(path.mat),add.idx] = rep(1,length(add.idx))

          }
          
        }else{
          if(length(add.idx)>0){
            path.mat = rbind(path.mat,path.mat[nrow(path.mat),])
            path.mat[nrow(path.mat),add.idx] = rep(1,length(add.idx))
          }
        }
        
      }
    }
    
   ## when we have some extra edges to add into tree2
    if(!is.null(add.set)&&length(add.set)>0){
      for(i in 1:length(add.set)){
        path.mat = rbind(path.mat,path.mat[nrow(path.mat),])
        path.mat[nrow(path.mat),nEdge1+add.set[[i]]] = rep(1,length(add.set[[i]]))
      }
      
    }
  
  return(path.mat)
  
}