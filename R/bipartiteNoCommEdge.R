## given 2 tree split matrix without common edge
## find the tree path
## @ parameter ncomm.t1.split is the tree split matrix of tree 1 
## @ parameter ncomm.t2.split is the tree split matrix of tree 2 
## @ parameter ncomm.t1.el is the vector contained edge length of tree 1
## @ parameter ncomm.t2.el is the vector contained edge length of tree 2 
## @ return orthants is a matrix with 
##    row: number of orthants
##    col: number of noncommon edges of tree 1 and tree 2 

bipartite.edges.nocomm <- function(ncomm.t1.split,ncomm.t2.split,
                           ncomm.t1.el,ncomm.t2.el){
  
  test1 = nrow(ncomm.t1.split)!=length(ncomm.t1.el)
  test2 = nrow(ncomm.t2.split)!=length(ncomm.t2.el)
  
  
  if (test1||test2){
    stop('Bipartite cannot be proced with trees lable and length not match!')
    
  }else{
    
    ## compatibility matrix of edges (upper triangular matrix)
    ## 1 means imcompatible; 0 means compatible
    nEdge1 = nrow(ncomm.t1.split)
    nEdge2 = nrow(ncomm.t2.split)
    
    graph.edge.mat.part = matrix(0,nrow=nEdge1,ncol=nEdge2)
    
    
    for (i in 1:nEdge1){
      e1 = ncomm.t1.split[i,]
      for (j in 1:nEdge2){
        e2 = ncomm.t2.split[j,]
        comp = check.compat(e1,e2)
        if (comp==0){
          # if incompatible 
          # there is an edge
          graph.edge.mat.part[i,j]=1
        }
      }
    }
    
    ## start from the trival bipartite
    graph.bipar.mat <- matrix(1,nrow=1,ncol=nEdge1+nEdge2)
    
    bipar = TRUE 
    
    
    while(bipar==TRUE){
      
      num_bipar = nrow(graph.bipar.mat)
      
      graph.bipar.mat.tmp = NULL 
      
      
      for (kk in 1:num_bipar){
        
        graph.bipar.mat.kk = NULL 
        
        graph.v.idc <- graph.bipar.mat[kk,]
        
        
        ## set up the graph 
        
        nodesSet1 <- which(graph.v.idc[1:nEdge1]==1) # in 1:nEdge1
        nodesSet2 <- nEdge1 + which(graph.v.idc[(1+nEdge1):(nEdge1+nEdge2)]==1) # in 1:nEdge2
        
        len.set1 = length(nodesSet1)
        len.set2 = length(nodesSet2)
        
        ## set graph edges
        if(len.set1>0&&len.set2>0){
          
          edgeListVec <- vector()
          
          for (i in nodesSet1){
            for (j in nodesSet2){
              if(graph.edge.mat.part[i,j-nEdge1]==1){
                 
                edgeListVec <- c(edgeListVec,which(nodesSet1==i),which(nodesSet2==j)+len.set1)
              }
            }
          }
          
          
          # set graph vertices 
          g <- graph.empty(directed=FALSE)
          g <- add.vertices(g,nv=len.set1,
                            attr=list(name=nodesSet1,type=rep(TRUE,len.set1)))
          
          
          g <- add.vertices(g,nv=len.set2,attr=list(name=nodesSet2,
                                                    type=rep(FALSE,len.set2)))
          
          
          g <- add.edges(g,edgeListVec)
          
          # plot the graph
          #plot.igraph(g, layout=layout.bipartite,
          #            vertex.color=c("orange","green")[V(g)$type+1])
          
          max.idp.set = maximal_ivs(g)
          
          # find a non-trival bipartite 
          i = 1
          while(i<=length(max.idp.set)){
            idpt.set.idx = as.vector(max.idp.set[[i]]) 
            # location index in the union of nodesSet1 and nodesSet2
            
            c2.idx = idpt.set.idx[which(idpt.set.idx<=len.set1)] 
            
            
            d1.idx = idpt.set.idx[which(idpt.set.idx>len.set1)]
            
            
            c1.idx = setdiff(1:len.set1,c2.idx)
            
            d2.idx = setdiff((len.set1+1):(len.set1+len.set2),d1.idx)
            
            trival.test1 = length(c2.idx)==len.set1||length(c1.idx)==len.set1
            trival.test2 = length(d2.idx)==len.set2||length(d1.idx)==len.set2
            
            ## trival bipartite
            if(trival.test1&&trival.test2){
              i=i+1
              # no non-tival bipartite case 
              if(i==length(max.idp.set)+1){
                graph.bipar.mat.kk <- graph.v.idc
              }
            }else{
              # we have found a non-trival bipartite 
              graph.par.tmp.mat = matrix(0,nrow=2,ncol=nEdge1+nEdge2)
              
              ## starting tree part 
              c1.edge.idx = nodesSet1[c1.idx]
              c2.edge.idx = nodesSet1[c2.idx]
              
              graph.par.tmp.mat[1, c1.edge.idx] = rep(1,length(c1.idx))
              
              graph.par.tmp.mat[2,c2.edge.idx] = rep(1,length(c2.idx))
              
              
              ## ending tree part 
              d1.edge.idx = nodesSet2[d1.idx-len.set1]
              d2.edge.idx = nodesSet2[d2.idx-len.set1]
              
              graph.par.tmp.mat[1, d1.edge.idx] = rep(1,length(d1.idx))
              
              graph.par.tmp.mat[2,d2.edge.idx] = rep(1,length(d2.idx))
              
              ## check criterion P3 (ii) the length condition
              
              buff = norm2(ncomm.t1.el[c1.edge.idx])/norm2(ncomm.t2.el[d1.edge.idx-nEdge1])-
                norm2(ncomm.t1.el[c2.edge.idx])/norm2(ncomm.t2.el[d2.edge.idx-nEdge1])
              
              if(buff<0){
                
                graph.bipar.mat.kk = graph.par.tmp.mat
                
                i = length(max.idp.set)+1
              }else{
                # the partition does not satisfy the P3 (ii) length condition
                i=i+1
                if(i==length(max.idp.set)+1){
                  graph.bipar.mat.kk <- graph.v.idc
                }
              }
            }
          }# end for finding a non-trival independent set
          
          
        }else{
          # no need to bipartite 
          graph.bipar.mat.kk <- graph.v.idc
        }
        
        graph.bipar.mat.tmp = rbind(graph.bipar.mat.tmp,graph.bipar.mat.kk)
        
        
      }# end loop for kk
      
      graph.bipar.mat = graph.bipar.mat.tmp
      
      if (is.null(graph.bipar.mat)){
        bipar = FALSE
      }else if(nrow(graph.bipar.mat)==num_bipar){
        bipar = FALSE
        graph.par.mat = graph.bipar.mat
      }else{
        graph.par.mat = graph.bipar.mat
      }
      
    }# end whlie 
    
    
    
    
  }# end bipartite  
  
  return(graph.par.mat)
}






