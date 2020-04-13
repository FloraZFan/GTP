## Given 2 tree splits shared with common edges
## get the list of tree paires without common edges
## @ parameter t1.split.mat is the tree split of tree 1
## @ parameter t2.split.mat is the tree split of tree 2
## @ return subtree.pair is a list of subtree paires 
##     for each subtree paire, no common edges shared 


get.subtrees <- function(t1.split.mat,t2.split.mat){
  treesplit = TRUE 
  
  subtree.pair = list(list(1:nrow(t1.split.mat),1:nrow(t2.split.mat)))
  
  while(treesplit==TRUE){
    
    subtree.pair.upd = list()
    
    for(i in 1:length(subtree.pair)){
      
      tree.pair.cal = subtree.pair[[i]]
      
      if(!is.null(tree.pair.cal[[1]])&&!is.null(tree.pair.cal[[2]])){
        
        
        comm.edge.idx <- find.common.edge(matrix(t1.split.mat[tree.pair.cal[[1]],],
                                                 nrow=length(tree.pair.cal[[1]])),
                                          matrix(t2.split.mat[tree.pair.cal[[2]],],
                                                 nrow=length(tree.pair.cal[[2]])))
        
        if(!is.null(comm.edge.idx)){
          
          
          t1.subtree.idx = subtree.split(matrix(t1.split.mat[tree.pair.cal[[1]],],
                                                nrow=length(tree.pair.cal[[1]])),
                                         comm.edge.idx[1,1])
          
          for(j in 1:length(t1.subtree.idx)){
            idx = t1.subtree.idx[[j]]
            if(!is.null(idx)){
              t1.subtree.idx[[j]] = tree.pair.cal[[1]][idx]
            }
          }
          
          
          t2.subtree.idx = subtree.split(matrix(t2.split.mat[tree.pair.cal[[2]],],
                                                nrow=length(tree.pair.cal[[2]])),
                                         comm.edge.idx[1,2])
          
          for(j in 1:length(t2.subtree.idx)){
            idx = t2.subtree.idx[[j]]
            if(!is.null(idx)){
              t2.subtree.idx[[j]] = tree.pair.cal[[2]][idx]
            }
          }
          
          subtree.pair.upd[[length(subtree.pair.upd)+1]] = list(t1.subtree.idx[[1]],t2.subtree.idx[[1]])
          
          subtree.pair.upd[[length(subtree.pair.upd)+1]] = list(t1.subtree.idx[[2]],t2.subtree.idx[[2]])
          
        }else{
          subtree.pair.upd[[length(subtree.pair.upd)+1]] = subtree.pair[[i]]
        }
      }else{
        subtree.pair.upd[[length(subtree.pair.upd)+1]] = subtree.pair[[i]]
      }    
      
    }# end for i 
    
    if(length(subtree.pair)==length(subtree.pair.upd)){
      treesplit = FALSE
    }
    
    subtree.pair = subtree.pair.upd
    
    
  }#end while
  
  
  return(subtree.pair)
}