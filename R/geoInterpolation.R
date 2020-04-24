## geodesic interpolation: given theta in (0,1)
## find the interpolation tree on geodesic
## @ parameter theta: parameter for interpolation 
## @ paramter t1/t2: starting/ending tree list
## @ return intpn.tree is a tree list which contains:
## feature tip.label: leaf edge label 
## feature tip.length: leaf edge length 
## feature int.edges.length: interior edge length
## feature int.edge.split: interior edge split


find.intpn.tree <- function(theta,t1,t2){
  
  # calculate geodesic 
  path.dist.cal = get.path.geodist(t1,t2)
  
  path.mat = path.dist.cal$path.mat
  noncomm.dist.seq = path.dist.cal$dist.seq
  geo.dist = path.dist.cal$geo.dist
  noncomm.dist = path.dist.cal$noncomm.edge.dist
  comm.dist = path.dist.cal$comm.edge.dist 
  leaf.dist = path.dist.cal$leaf.edge.dist
 
  # find interpolation tree
  nEdge1 = length(t1$int.edges.length)
  nEdge2 = length(t2$int.edges.length)
  
  # combine edge length
  edge.length = c(t1$int.edges.length, t2$int.edges.length)
  
  # find common edge
  comm.edge.idx = find.common.edge(t1$int.edge.split,t2$int.edge.split)
  
  # distance list for noncomm/comm/leaf edge distance 
  acc.dist.seq = vector()
  # noncommon edge distance
  if(!is.null(noncomm.dist.seq)){
    num.noncomm = nrow(noncomm.dist.seq)
    for (i in 1:num.noncomm){
      acc.dist.seq[i] = norm2(apply(matrix(noncomm.dist.seq[1:i,],
                                           ncol=2),1,sum))
    }
  }else{
    num.noncomm = 0
  }
  # common edge distance
  acc.dist.seq[length(acc.dist.seq)+1] = norm2(c(noncomm.dist,comm.dist)) 
  # leaf edge distance
  acc.dist.seq[length(acc.dist.seq)+1] = norm2(c(noncomm.dist,comm.dist,leaf.dist))
  
  # find the index for theta in the distance list
  idx.theta = min(which(acc.dist.seq>=theta*geo.dist))
  
  # settings for the interpolation tree
  intpn.edge.idx = rep(0,nEdge1+nEdge2)
  ## leaf edge changes
  ## align with t1
  intpn.tip.label = t1$tip.label
  intpn.tip.length = t1$tip.length
  
  # get distance changes for the interpolation tree
  if(idx.theta==1){
    part.dist = theta*geo.dist
    full.dist = acc.dist.seq[idx.theta]
  }else{
    part.dist = sqrt(theta^2*geo.dist^2-acc.dist.seq[idx.theta-1]^2)
    full.dist = sqrt(acc.dist.seq[idx.theta]^2-acc.dist.seq[idx.theta-1]^2)
  }
  
  
  if(idx.theta<=num.noncomm){
    ## part of noncomm edge changes
    intpn.edge.idx = path.mat[idx.theta+1,]
    
    intpn.edge.length = rep(0,nEdge1+nEdge2)
    
    intpn.edge.length[which(intpn.edge.idx==1)] = edge.length[which(intpn.edge.idx==1)]
    
    # index for the edges to be dropped from t1
    drop.edge.idx = setdiff(which(path.mat[idx.theta,(1:nEdge1)]==1),
                            which(path.mat[idx.theta+1,(1:nEdge1)]==1))
    # index for the edges to be added to t1
    add.edge.idx = setdiff(which(path.mat[idx.theta+1,-(1:nEdge1)]==1),
                           which(path.mat[idx.theta,-(1:nEdge1)]==1))+nEdge1
    
    ## dropping/adding procedure 
    if(part.dist<=noncomm.dist.seq[idx.theta,1]){
      # only dropping procedure 
      full.dist = noncomm.dist.seq[idx.theta,1]
      
      if(length(drop.edge.idx)>0){
        intpn.edge.idx[drop.edge.idx] = rep(1,length(drop.edge.idx))
      }
      
      # no adding procedure 
      if(length(add.edge.idx)>0){
        intpn.edge.idx[add.edge.idx] = rep(0,length(add.edge.idx))
        intpn.edge.length[add.edge.idx] = rep(0,length(add.edge.idx))
      }
      
      ## dropping edge length 
      v1 = edge.length[drop.edge.idx]
      v2 = rep(0,length(drop.edge.idx))
      
      intpn.edge.length[drop.edge.idx] = (v2-v1)*part.dist/full.dist+v1
      
      intpn.edge.length[drop.edge.idx]-v1 
      
      intpn.edge.length = intpn.edge.length[which(intpn.edge.length!=0)]
    }else{
      # proceed adding procedure after dropping procedure 
      part.dist = part.dist-noncomm.dist.seq[idx.theta,1]
      full.dist = noncomm.dist.seq[idx.theta,2]
      # start adding procedure 
      v1 = rep(0,length(add.edge.idx))
      v2 = edge.length[add.edge.idx]
      
      intpn.edge.length[add.edge.idx] = (v2-v1)*part.dist/full.dist+v1
      
      intpn.edge.length = intpn.edge.length[which(intpn.edge.length!=0)]
      
    }
    
  }else{
    ## the interpretation tree contains all noncomm edge changes
    intpn.edge.idx = c(rep(0,nEdge1),rep(1,nEdge2))
    intpn.edge.length = t2$int.edges.length
    
    if(idx.theta==length(acc.dist.seq)){
      # with all noncomm edge changes and common edge changes
      # part of leaf edge changes
      v1 = t1$tip.length
      v2 = t2$tip.length%*%st.map(t1$tip.label,t2$tip.label)
      
      intpn.tip.length = (v2-v1)*part.dist/full.dist+v1
    }else{
      # with all non common edge changes
      # part of common edge 
      v1 = t1$int.edges.length[comm.edge.idx[,1]]
      v2 = t2$int.edges.length[comm.edge.idx[,2]]
      
      intpn.edge.length[comm.edge.idx[,2]] = (v2-v1)*part.dist/full.dist+v1
      
    }
    
  }
  
  # get edge split
  intpn.edge.split = rbind(t1$int.edge.split,
                           t2$int.edge.split)[which(intpn.edge.idx==1),]
  
  
  # get interpolation tree
  intpn.tree = list(tip.label=intpn.tip.label, 
                    tip.length=intpn.tip.length,
                    int.edges.length=intpn.edge.length,
                    int.edge.split=intpn.edge.split)
  
  return(intpn.tree)
  

}




