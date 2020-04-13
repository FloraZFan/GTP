## compute geodesic distance between t1 and t2
## @ parameter t1 is the list of starting tree
## @ parameter t2 is the list of ending tree
## the tree list should contain the following feaures  
## 1.tip.label: leaf lable
## 2.tip.length: leaf edge length
## 3.int.edges.length: interior edge length
## 4.int.edge.split: interior edge splits
## @ return geo.dist.cal is a list contains 
## 1. geo.dist: geodesic distance
## 2. path.mat: tree path matrix
## 3. noncomm.edge.dist: noncommon edge distance contribution to geodesic distance
## 4. comm.edge.dist: common edge distance contribution to geodesic distance
## 5. leaf.edge.dist: leaf edge distance contribution to geodesic distance
## 6. dist.seq: noncommon edge contibution in each othant



get.path.geodist <- function(t1,t2){
  
  if(is.list(t1)&&is.list(t2)){
    if(length(t1$tip.label)!=length(t2$tip.label)){
      stop('Trees with different number of leaves cannot be compared for GTP.')
      
    }else{
      # number of tree leaves  
      nLeaves = length(t1$tip.label)
      # number of interior edges in t1 and t2
      nEdge1 = length(t1$int.edges.length)
      nEdge2 = length(t2$int.edges.length)
      
      ## get edge split
      t1.split = t1$int.edge.split
      
      t2.split = t2$int.edge.split
      
      ## check if there is a permutation between the order of tree leaves
      se.map.mat <- st.map(t1$tip.label,t2$tip.label)
      ## align the leave lables of ending tree t2 with the starting tree t1
      t2.split <- t2.split%*%se.map.mat 
      
      ## check if the trees have common edges
      common.edge.idx = find.common.edge(t1.split,t2.split)
      ## common/non-common edge contribution
      comm.edge.ctbt = noncomm.edge.ctbt = 0
      
      ## calculate if there edges to be dropped off from t1 
      ## or to be added in to t2
      drop.set.norm = add.set.norm = vector()
      
      ## calculate tree path space and geodesic 
      if(is.null(common.edge.idx)){
        # no common edge case when common.edge.idx is null
        
        # get the edge bipartite
        bipar.mat = bipartite.edges.nocomm(t1.split,t2.split,
                                           t1$int.edges.length,t2$int.edges.length)
        
        
        # calculate geodesic (L2 norm)
        dist.cal = cal.geo.nocomm(bipar.mat,t1$int.edges.length,t2$int.edges.length)
        
        dist.seq = dist.cal$dist.seq
        
        #sub.geo.dist = norm2(apply(dist.seq,1,sum))^2
        
        # get tree path
        path.mat = find.tree.path(bipar.mat,nEdge1,nEdge2,drop.set=NULL,add.set=NULL)
        
      }else{
        ## the case with common edges
        
        ## calculate common edge contribution (squared)
        for (jj in 1:nrow(common.edge.idx)){
          comm.edge.ctbt = comm.edge.ctbt +(t1$int.edges.length[common.edge.idx[jj,1]]-t2$int.edges.length[common.edge.idx[jj,2]])^2
        }
        
        ## subtree splitting (by cutting off common trees)
        subtree.pair = get.subtrees(t1.split,t2.split)
        
        ## calculate geodesic distance between subtree paires
        bipar.mat = NULL
        dist.seq = NULL
        drop.set = add.set = list()
        
        # compute distance bewteen subtree paires
        for (ii in 1:length(subtree.pair)){
          subtree.cal = subtree.pair[[ii]]
          
          if(!is.null(subtree.cal[[1]])&&!is.null(subtree.cal[[2]])){
            # case: two subtrees all not null
            # get the edge bipartite 
            bipar.mat.tmp = bipartite.edges.nocomm(matrix(t1.split[subtree.cal[[1]],],nrow=length(subtree.cal[[1]])),
                                                   matrix(t2.split[subtree.cal[[2]],],nrow=length(subtree.cal[[2]])),
                                                   t1$int.edges.length[subtree.cal[[1]]],
                                                   t2$int.edges.length[subtree.cal[[2]]])
            
            
            
            # calculate geodesic
            dist.cal = cal.geo.nocomm(bipar.mat.tmp,t1$int.edges.length[subtree.cal[[1]]],
                                      t2$int.edges.length[subtree.cal[[2]]])
            
            dist.seq = rbind(dist.seq,dist.cal$dist.seq)
            
            # rewrite bipar.mat with the entries in tree1 and tree2 
            bipar.mat = matrix(0,nrow=nrow(bipar.mat.tmp),ncol=nEdge1+nEdge2)
            bipar.mat[,subtree.cal[[1]]] = bipar.mat.tmp[,1:length(subtree.cal[[1]])]
            bipar.mat[,subtree.cal[[2]]+nEdge1] =  bipar.mat.tmp[,-(1:length(subtree.cal[[1]]))]
            
            
          }else{
            # more than one paires have no interior edges
            
            if(!is.null(subtree.cal[[1]])){
              # dropping set  
              drop.set[[length(drop.set)+1]] = subtree.cal[[1]]
              # geodesic distance (L2 norm)
              drop.set.norm[length(drop.set.norm)+1] = norm2(t1$int.edges.length[subtree.cal[[1]]])
            }else{
              
              if(!is.null(subtree.cal[[2]])){
                # adding set
                add.set[[length(add.set)+1]] = subtree.cal[[2]]
                
                # geodesic distance (L2 norm)
                add.set.norm[length(add.set.norm)+1] = norm2(t2$int.edges.length[subtree.cal[[2]]])
              }
              
              
              
            }
          }
          
        }# end of subtree pairs
        
        
        # get tree path
        path.mat = find.tree.path(bipar.mat,nEdge1,nEdge2,drop.set,add.set)
        
        # update dist.seq by adding drop.set and add.set
        if (!is.null(drop.set.norm)&&length(drop.set.norm)>0){
          dist.seq = rbind(cbind(drop.set.norm,rep(0,length(drop.set.norm))),dist.seq)
        }
        
        if (!is.null(add.set.norm)&&length(add.set.norm)>0){
          dist.seq = rbind(dist.seq,cbind(rep(0,length(add.set.norm)),add.set.norm))
        }
        
      }# end of common edge 
      
      # non common edge contribution 
      if(!is.null(dist.seq)){
        noncomm.edge.ctbt = norm2(apply(dist.seq,1,sum))
      }

      ## leaf edge contribution (squared)
      leaf.edge.ctbt = norm2(t1$tip.length-t2$tip.length%*%se.map.mat)
      
      ## return the geodesic distance
      geo.dist = sqrt(leaf.edge.ctbt^2+comm.edge.ctbt+noncomm.edge.ctbt^2) 
    
    }

  }else{
    stop('Tree structure is not a list!')
  }
  
  geo.dist.cal = list(path.mat=path.mat, geo.dist=geo.dist, dist.seq=dist.seq,
                      leaf.edge.dist=leaf.edge.ctbt, 
                      comm.edge.dist=sqrt(comm.edge.ctbt),
                      noncomm.edge.dist=noncomm.edge.ctbt)
  
  return(geo.dist.cal)

} 
