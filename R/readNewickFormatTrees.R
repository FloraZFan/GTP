## read trees in a file with Newick format
## @ parameter fileName is the tree file directory to read 
## @ return is a list of trees with features ready to use 

read.newick <- function(fileName){
  
  text <- scan(fileName,sep="\n",what="character")
  
  if(length(text)>1){
    ## when there are multiple trees stored in the tree file
    tree <- lapply(text,newick)
    
    class(tree) <- "multiPhylo"
  }else{
    tree <- newick(text)
  }
  
  return(tree)
}


## read a single tree with newick format
## @ parameter text is newick format tree
## @ return is a list of the following features for the tree
## 1. tip.label: leave nodes 
## 2. tip.length: leave edge length
## 3. int.edges.length: interior edge length 
## 4. int.edge.split: interior edge split


newick <- function(text){
  
  text <- unlist(strsplit(text, NULL))
  
  tip.label <- vector(mode="character") 
  # get leaf label
  
  leave.edge.length <- vector() 
  # get leaf edge length
  
  node.label.idx <- vector() 
  # an index to move between different levels of the tree
  
  tip.int.node.label <- NULL
  # label the interior nodes 
  
  interior.edges.length <- vector()
  # get interior edge length
  
  int.edge.list.start <- vector()
  # label interior edges
  
  int.edge.list.end <- vector()
  # label interior edges
  
  Nnode <- 0
  currnode <- 1 
  node.label.idx[currnode] <- c("O")
  
  i<- k <- num.interior.edges <-1 
  
  
  while(text[i]!=";"){# read the tree until ";"
    
    # main case 1: "("
    if(text[i]=="("){
      
      Nnode <- Nnode+1 # creating a new internal node
      
      currnode <- Nnode
      
      node.label.idx[currnode] <- c("O")
      
      i<-i+1
      # is the next element a label?
      if(is.na(match(text[i],c("(",")",",",":",";")))){
        temp<-getLabel(text,i)
        tip.label[k]<-temp$label
        i<-temp$end
        
        
        label.tmp <- which(node.label.idx=="O")
        
        maxlength <- max(ncol(tip.int.node.label),length(label.tmp))
        
        
        if(is.null(tip.int.node.label)){
          tip.int.node.label <- rbind(tip.int.node.label,label.tmp)
          
        }else{
          tmp <- matrix(NA,nrow=nrow(tip.int.node.label),ncol=maxlength)
          tmp[,1:ncol(tip.int.node.label)] <- tip.int.node.label
          label.tmp.na <- rep(NA,maxlength)
          label.tmp.na[1:length(label.tmp)] <-label.tmp
          tip.int.node.label <- rbind(tmp,label.tmp.na)
        }
        
        
        # is there a branch length?
        if(text[i]==":"){
          temp<-getEdgeLength(text,i)
          leave.edge.length[k]<-temp$edge.length
          i<-temp$end
        }	
        # counting number of leaves
        k<-k+1
        
      }
      
      # end of case 1: "("
      # main case 2: ")"
    } else if(text[i]==")"){
      i<-i+1
      
      # is there a interior edge length?
      if(text[i]==":"){
        node.label.idx[currnode] <- c("C")
        int.edge.list.start[num.interior.edges] <- currnode
        temp<-getEdgeLength(text,i)
        if(currnode>1) {
          interior.edges.length[num.interior.edges] <- temp$edge.length
          
          num.interior.edges <- num.interior.edges+1}
        
        i<-temp$end
        currnode <- max(which(node.label.idx=="O")) 
        # move to another level
        int.edge.list.end[num.interior.edges-1] <- currnode
        node.label.idx[currnode] <- c("O") 
        # reopen the lower level
      }	
      
      # end of main case 2: ")"
      # main case 3: "," 
    } else if(text[i]==","){
      
      i<-i+1
      # is the next element a label?
      if(is.na(match(text[i],c("(",")",",",":",";")))){
        temp<-getLabel(text,i)
        tip.label[k]<-temp$label
        i<-temp$end
        
        
        label.tmp <- which(node.label.idx=="O")
        maxlength <- max(ncol(tip.int.node.label),length(label.tmp))
        
        if(is.null(tip.int.node.label)){
          tip.int.node.label <- rbind(tip.int.node.label,label.tmp)
        }else{
          tmp = matrix(NA,nrow=nrow(tip.int.node.label),ncol=maxlength)
          tmp[,1:ncol(tip.int.node.label)]<- tip.int.node.label
          label.tmp.na <- rep(NA,maxlength)
          label.tmp.na[1:length(label.tmp)] <-label.tmp
          tip.int.node.label <- rbind(tmp,label.tmp.na)
        }
        
        
        
        # is there a branch length?
        if(text[i]==":"){
          temp<-getEdgeLength(text,i)
          leave.edge.length[k]<-temp$edge.length
          i<-temp$end
        }
        
        k<-k+1
      }
      
    }# end of main case 3: ","
    
    
  }# end of reading text
  
  int.edge.list <- cbind(int.edge.list.start, int.edge.list.end)
  
  # interior edge splits
  int.edge.split = edgeSplit(tip.int.node.label,int.edge.list)
  
  # assemble into a list of tree features 
  tree <- list(tip.label=tip.label, tip.length=leave.edge.length,
               int.edges.length=interior.edges.length,
               int.edge.split=int.edge.split)
  
  return(tree)
}
