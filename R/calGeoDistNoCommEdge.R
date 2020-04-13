## Given tree path (a sequence of orthants)
## calculate the geodesic distance 
## @ paramter orthant.mat is a tree path matrix 
##     with row: number of intermediate trees
##     with columns: number of noncommon edges in tree 1 and tree 2
## @ parameter ncomm.t1.el is the vector of edge lenght of tree 1
## @ parameter ncomm.t2.el is the vector of edge lenght of tree 2
## @ return geo.dist is the geodisc distance from tree 1 to tree 2
## @ return dist.seq is the matrix contain L2 norm of A_k and B_k 

cal.geo.nocomm <- function(graph.par.mat,ncomm.t1.el,ncomm.t2.el){
  
  if (ncol(graph.par.mat)!=length(ncomm.t1.el)+length(ncomm.t2.el)){
    stop('Geodesic calculation cannot be proceed with number of edges not match!')
  }else{
    st_vec = vector()
    et_vec = vector()
    
    for (i in 1:nrow(graph.par.mat) ){
      st.idx = which(graph.par.mat[i,1:length(ncomm.t1.el)]==1)
      st_vec[i] = norm2(ncomm.t1.el[st.idx])
      et.idx = which(graph.par.mat[i,(length(ncomm.t1.el)+1):(length(ncomm.t1.el)+length(ncomm.t2.el))]==1)
      et_vec[i] = norm2(ncomm.t2.el[et.idx])
    }
    
    dist.seq = cbind(st_vec,et_vec)
    
    geo.dist = norm2(st_vec+et_vec) 
  }
  return(list(geo.dist=geo.dist,dist.seq=dist.seq))
}


