## The following two functions 
## are written by Liam J. Revell 2011-2013, 2014

## you can find it at the link 
## https://github.com/liamrevell/phytools.git


# function gets label

getLabel<-function(text,start,stop.char=c(",",":",")",";")){
  i<-0
  while(is.na(match(text[i+start],stop.char))) i<-i+1
  label<-paste(text[0:(i-1)+start],collapse="")	
  return(list(label=paste(label,collapse=""),end=i+start))
}

# function gets branch length

getEdgeLength<-function(text,start){
  i<-start+1
  stop.char<-c(",",")",";")
  while(is.na(match(text[i],stop.char))) i<-i+1
  edge.length<-as.numeric(paste(text[(start+1):(i-1)],collapse=""))
  return(list(edge.length=edge.length,end=i))
}
