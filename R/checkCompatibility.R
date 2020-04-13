## Given 2 edge splits 
## check the compatibility of the 2 edges 
## @ parameter split1 is a vector of tree split
## @ parameter split2 is a vector of tree split
## @ return compat is logical 
##     return 1 if the 2 edges are compatible
##     return 0 else

check.compat <- function(split1,split2){
  s1l <- which(split1==1)
  s1r <- which(split1==0)
  # union the tree root
  #s1r <- union(s1r,0)
  
  s2l <- which(split2==1)
  s2r <- which(split2==0)
  # union the tree root
  #s2r <- union(s2r,0)
  
  compat <- 0
  
  t1 = length(intersect(s1l,s2l))
  t2 = length(intersect(s1l,s2r))
  t3 = length(intersect(s1r,s2l))
  t4 = length(intersect(s1r,s2r))
  
  if(t1==0||t2==0||t3==0||t4==0){
    compat = 1
  }
  
  return(compat)
}
