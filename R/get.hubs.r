#
# get.hubs.R
#
#
#
#
#
# Copyright (C) 2011 : Khadija El Amrani <Khadija.Amrani@campus.lmu.de>
##############################################################################

#### This function returns nodes (hubs) incident to only edges colored with the given color ### 
get.hubs<-function(RgraphObj, color, p=20)
{

  colVec<-c()
  
  for(i in 1:length(edgeNames(RgraphObj)))
  {
    if(length(colVec)<3)
    {
      if(length(which(colVec==RgraphObj@AgEdge[[i]]@color))==0)
        {
           colVec<-c(colVec, RgraphObj@AgEdge[[i]]@color)         ## Build the color vector
        }
    }
  }
  ### the given color exists in the color vector
  if(length(which(colVec==color))!=0)
  {
   
      Num.edges<-length(edgeNames(RgraphObj))
      threshold<-((Num.edges * p)/100)       ## Calculate the threshold for a node to be considered as hub
      hubs<-c()
      Num.edgesOfhubs<-c()
      otherCols<-colVec[-(which(colVec==color))]
    
      edgesVec<-get.edges(RgraphObj, color)
      edgesVecC2<-c()
      edgesVecC3<-c()
      
       nodesUn<-c()
       nodesUnC2<-c()
       nodesUnC3<-c()
        
      if(length(otherCols)>=1)
      {
        edgesVecC2<-get.edges(RgraphObj, otherCols[1])
      }
      if(length(otherCols)>=2)
      {
        edgesVecC3<-get.edges(RgraphObj, otherCols[2])
      }
    if(length(edgesVecC2)>0)
    {
       edgesC2.list<-strsplit(edgesVecC2, "~")
       edgesC2.unlist<-unlist(edgesC2.list)
       nodesUnC2<-unique(edgesC2.unlist)
    }
      if(length(edgesVecC3)>0)
    {
       edgesC3.list<-strsplit(edgesVecC3, "~")
       edgesC3.unlist<-unlist(edgesC3.list)
       nodesUnC3<-unique(edgesC3.unlist)
    }
  
     if(length(edgesVec)>0)
    {
     edges.list<-strsplit(edgesVec, "~")
     edges.unlist<-unlist(edges.list)
     nodesUn<-unique(edges.unlist)
    }
    
    if(length(nodesUnC2)>0)
     {
      ii<-which(nodesUn %in% nodesUnC2)
      if(length(ii)>0)
      {
         nodesUn<-nodesUn[-ii]
      } 
     }
      if(length(nodesUnC3)>0)
     {
        iii<-which(nodesUn %in% nodesUnC3)
        if(length(iii)>0)
        {
         nodesUn<-nodesUn[-iii]
        }
     }
     
    if(length(nodesUn)>0)
    {
    for(i in 1:length(nodesUn))
    {
      ii<-length(which(edges.unlist==nodesUn[[i]]))
      
     if( ii >= threshold)                         ## Node degree is greater or equal the threshold
     {
      hubs<-c(hubs, nodesUn[[i]])                 ## Add node to hubs vector
      Num.edgesOfhubs<-c(Num.edgesOfhubs, ii)
     }
    }
       
    if(length(hubs)>0)
    {
       names(Num.edgesOfhubs)<-hubs
       return(sort(Num.edgesOfhubs, decreasing=TRUE)) ## Sort and return hubs
    }
    else{print("There are no hubs to the given threshold")}
    }
    else
    {
     print("There are no hubs to the given color")
    }
 }
    else{print("There are no hubs to the given color")}
   
}

