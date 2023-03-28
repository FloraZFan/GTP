#some self-defined functions 

st.map <- function(st.label,et.label){
  if(length(st.label)!=length(et.label)){
    stop('Trees with different number of leaves cannot be mapped.')
  }else{
    Ntip = length(st.label)
    st.map.mat = matrix(0,nrow=Ntip,ncol=Ntip)
    
    for (i in 1:Ntip){
      idx <- which(et.label==st.label[i])
      st.map.mat[idx,i] <- 1
    }
  }
  
  return(st.map.mat)
}

## compute euclidean norm of a vector

norm2 <- function(x) {
  return(sqrt(sum (x^2)))
}


### find edge index in the edge split matrix

find.edge.idx <- function(edge.split.mat,test.edge){
  buff = apply(edge.split.mat,1,function(x){sum(abs(x-test.edge))}) 
  test.edge.idx = which(buff==0)
  
  return(test.edge.idx)
  
}

