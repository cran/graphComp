#
# get.edges.R
#
#
#
#
#
# Copyright (C) 2011 : Khadija El Amrani <Khadija.Amrani@campus.lmu.de>
##############################################################################

get.edges<-function(RgraphObj, EdgColor)
{

   colVec<-c()
 #### get color vector
  for(i in 1:length(edgeNames(RgraphObj)))
  {
    if(length(colVec)<3)
    {
      if(length(which(colVec==RgraphObj@AgEdge[[i]]@color))==0)
        {
           colVec<-c(colVec, RgraphObj@AgEdge[[i]]@color)
        }
    }
 }
 
 if(length(which(colVec==EdgColor))!=0)
 {    
    lenEdges<-length(RgraphObj@AgEdge)
      Edges.vec<-c()
   for(i in 1:lenEdges)
   {
         if(RgraphObj@AgEdge[[i]]@color==EdgColor)             ## color of edge is equal the given edge
         {
            Edges.vec<-c(Edges.vec, paste(RgraphObj@AgEdge[[i]]@head, RgraphObj@AgEdge[[i]]@tail, sep="~")) ## add edge to the vector

         }
  }
  
  if(length(Edges.vec)>0)       ## if edge vector not empty
  {return(Edges.vec)}           ## return the found edges
 # else{print("There are no edges to the given color")}
  }
  else{  return(c())
      #print("There are no edges to the given color")
      
      }
}


