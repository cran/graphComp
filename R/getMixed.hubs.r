#
# getMixed.hubs.R
#
#
#
#
#
# Copyright (C) 2011 : Khadija El Amrani <Khadija.Amrani@campus.lmu.de>
##############################################################################

#### This function returns nodes (hubs) incident to edges colored with at least two colors ### 
getMixed.hubs<-function(RgraphObj, p=20)
{
  colVec<-c()

  for(i in 1:length(edgeNames(RgraphObj)))
  {
    if(length(colVec)<3)
    {
      if(length(which(colVec==RgraphObj@AgEdge[[i]]@color))==0)
        {
           colVec<-c(colVec, RgraphObj@AgEdge[[i]]@color)          ## Build the color vector
        }
    }
 }
   Num.edges<-length(edgeNames(RgraphObj))
   ## Calculate the threshold for a node to be considered as hub
   threshold<-((Num.edges * p)/100)
    myList<-list()
if(length(colVec)==3)
{
 edgesColVec1<-get.edges(RgraphObj, colVec[1])
 
  edgesVec1.list<-strsplit(edgesColVec1, "~")
  edgesVec1.unlist<-unlist(edgesVec1.list)
  nodesUnVec1<-unique(edgesVec1.unlist)

     edgesColVec2<-get.edges(RgraphObj, colVec[2])
     edgesVec2.list<-strsplit(edgesColVec2, "~")
     edgesVec2.unlist<-unlist(edgesVec2.list)
     nodesUnVec2<-unique(edgesVec2.unlist)

      edgesColVec3<-get.edges(RgraphObj, colVec[3])
        
      edgesVec3.list<-strsplit(edgesColVec3, "~")
      edgesVec3.unlist<-unlist(edgesVec3.list)
      nodesUnVec3<-unique(edgesVec3.unlist)
 
  ####colVec[1] vs. colVec[2]
  
   intersecV1AndV2<-nodesUnVec1[which(nodesUnVec1 %in% nodesUnVec2)]
   intersecV123Ind<-which(intersecV1AndV2 %in% nodesUnVec3)
   if(length(intersecV123Ind)>0)
   {
   intersecV1AndV2<-intersecV1AndV2[-intersecV123Ind]
   }
    hubs<-c()
    Num.edgesOfhubs<-c()
   if(length(intersecV1AndV2)>0)
   {
    for(i in 1:length(intersecV1AndV2))
    {
      ii<-length(which(c(edgesVec1.unlist, edgesVec2.unlist)==intersecV1AndV2[[i]]))
      
     if( ii >= threshold)
     {
       hubs<-c(hubs, intersecV1AndV2[[i]])
       Num.edgesOfhubs<-c(Num.edgesOfhubs, ii)
     }
    } 
    
     
  }
    if(length(hubs)>0)
    {
       NumEd1<-c()
       NumEd2<-c()
     for(i in 1:length(hubs))
    {
      NumEd1<-c(NumEd1, length(which(edgesVec1.unlist==hubs[[i]])))
      NumEd2<-c(NumEd2, length(which(edgesVec2.unlist==hubs[[i]])))
    }
       
     ResMat<-matrix(ncol=2, nrow=length(hubs))
     colnames(ResMat)<-c(colVec[1], colVec[2])
     rownames(ResMat)<-hubs
     ResMat[,1]<-NumEd1
     ResMat[,2]<-NumEd2
    
     myList[[length(myList)+1]]<-ResMat
     
    }
   
   
    
        
        
    #dataFrame1<-data.frame("Node"=hubs, c11=NumEd1,NumEd2) #, col.names=c("Node", colVec[1], colVec[2])
    #dataFrame1<-as.data.frame(matrix(nrow=length(hubs),ncol=3), colnames=c("Node")) #"Node"=hubs, c11=NumEd1,NumEd2)
   
  
#    dataFrame1
 ####colVec[1] vs. colVec[3]
    intersecV1AndV3<-nodesUnVec1[which(nodesUnVec1 %in% nodesUnVec3)]
    intersecV123Ind<-which(intersecV1AndV3 %in% nodesUnVec2)
    if(length(intersecV123Ind)>0)
    {
      intersecV1AndV3<-intersecV1AndV3[-intersecV123Ind]
    }
    hubs<-c()
    Num.edgesOfhubs<-c()
   
   if(length(intersecV1AndV3)>0)
   {
    for(i in 1:length(intersecV1AndV3))
    {
      ii<-length(which(c(edgesVec1.unlist, edgesVec3.unlist)==intersecV1AndV3[[i]]))
      
     if( ii >= threshold)
     {
       hubs<-c(hubs, intersecV1AndV3[[i]])
       Num.edgesOfhubs<-c(Num.edgesOfhubs, ii)
     }
    } 
  }
     
    
    if(length(hubs)>0)
    {
       NumEd1<-c()
       NumEd3<-c()
     for(i in 1:length(hubs))
     {
      NumEd1<-c(NumEd1, length(which(edgesVec1.unlist==hubs[[i]])))
      NumEd3<-c(NumEd3, length(which(edgesVec3.unlist==hubs[[i]])))
     }
        
    ResMat<-matrix(ncol=2, nrow=length(hubs))
    colnames(ResMat)<-c(colVec[1], colVec[3])
    rownames(ResMat)<-hubs
    ResMat[,1]<-NumEd1
    ResMat[,2]<-NumEd3
     
       myList[[length(myList)+1]]<-ResMat
      
    }
    
 
 ####colVec[2] vs. colVec[3]
  
    intersecV2AndV3<-nodesUnVec2[which(nodesUnVec2 %in% nodesUnVec3)]
    intersecV123Ind<-which(intersecV2AndV3 %in% nodesUnVec1)
    
      if(length(intersecV123Ind)>0)
    {
     intersecV2AndV3<-intersecV2AndV3[-intersecV123Ind]
    }
    
   
    hubs<-c()
    Num.edgesOfhubs<-c()
   
    if(length(intersecV2AndV3)>0)
    {
    
    for(i in 1:length(intersecV2AndV3))
    {
      ii<-length(which(c(edgesVec2.unlist, edgesVec3.unlist)==intersecV2AndV3[[i]]))
      
     if( ii >= threshold)
     {
       hubs<-c(hubs, intersecV2AndV3[[i]])
       Num.edgesOfhubs<-c(Num.edgesOfhubs, ii)
     }
    } 
     }
   
    if(length(hubs)>0)
    {
          NumEd2<-c()
       NumEd3<-c()
     for(i in 1:length(hubs))
     {
      NumEd2<-c(NumEd2, length(which(edgesVec2.unlist==hubs[[i]])))
      NumEd3<-c(NumEd3, length(which(edgesVec3.unlist==hubs[[i]])))
     }
        
    ResMat<-matrix(ncol=2, nrow=length(hubs))
    colnames(ResMat)<-c(colVec[2], colVec[3])
    rownames(ResMat)<-hubs
    ResMat[,1]<-NumEd2
    ResMat[,2]<-NumEd3
     
       myList[[length(myList)+1]]<-ResMat
  
    }
 
 
 ####colVec[1] vs. colVec[2] vs. colVec[3]
   
    intersecV2AndV3<-nodesUnVec2[which(nodesUnVec2 %in% nodesUnVec3)]
    intersecV123Ind<-which(intersecV2AndV3 %in% nodesUnVec1)
    
    intersecV123<-intersecV2AndV3[intersecV123Ind]
   
    hubs<-c()
    Num.edgesOfhubs<-c()
   
   if(length(intersecV123)>0)
   {
    for(i in 1:length(intersecV123))
    {
      ii<-length(which(c(edgesVec1.unlist, edgesVec2.unlist, edgesVec3.unlist)==intersecV123[[i]]))
      
     if( ii >= threshold)
     {
       hubs<-c(hubs, intersecV123[[i]])
       Num.edgesOfhubs<-c(Num.edgesOfhubs, ii)
     }
    }
    } 
      
    if(length(hubs)>0)
    {
       NumEd1<-c()
       NumEd2<-c()
       NumEd3<-c()
     for(i in 1:length(hubs))
     {
      NumEd1<-c(NumEd1, length(which(edgesVec1.unlist==hubs[[i]])))
      NumEd2<-c(NumEd2, length(which(edgesVec2.unlist==hubs[[i]])))
      NumEd3<-c(NumEd3, length(which(edgesVec3.unlist==hubs[[i]])))
     }
        
    ResMat<-matrix(ncol=3, nrow=length(hubs))
    colnames(ResMat)<-c(colVec[1], colVec[2], colVec[3])
    rownames(ResMat)<-hubs
    ResMat[,1]<-NumEd1
    ResMat[,2]<-NumEd2
    ResMat[,3]<-NumEd3
    
   
    myList[[length(myList)+1]]<-ResMat
        
  
    }
 
 
  if(length(myList)>0)
  {
   myList
  }
  else{ print("There are no mixed hubs to the given threshold")
        }
 

}  ########
else if(length(colVec)==2)
{
   edgesColVec1<-get.edges(RgraphObj, colVec[1])
 
  edgesVec1.list<-strsplit(edgesColVec1, "~")
  edgesVec1.unlist<-unlist(edgesVec1.list)
  nodesUnVec1<-unique(edgesVec1.unlist)

     edgesColVec2<-get.edges(RgraphObj, colVec[2])
     edgesVec2.list<-strsplit(edgesColVec2, "~")
     edgesVec2.unlist<-unlist(edgesVec2.list)
     nodesUnVec2<-unique(edgesVec2.unlist)
     
      intersecV1AndV2<-nodesUnVec1[which(nodesUnVec1 %in% nodesUnVec2)]
     
    hubs<-c()
    Num.edgesOfhubs<-c()
   if(length(intersecV1AndV2)>0)
   {
    for(i in 1:length(intersecV1AndV2))
    {
      ii<-length(which(c(edgesVec1.unlist, edgesVec2.unlist)==intersecV1AndV2[[i]]))
      
     if( ii >= threshold)
     {
       hubs<-c(hubs, intersecV1AndV2[[i]])
       Num.edgesOfhubs<-c(Num.edgesOfhubs, ii)
     }
    } 
    
     
    }
    if(length(hubs)>0)
    {
       NumEd1<-c()
       NumEd2<-c()
     for(i in 1:length(hubs))
    {
      NumEd1<-c(NumEd1, length(which(edgesVec1.unlist==hubs[[i]])))
      NumEd2<-c(NumEd2, length(which(edgesVec2.unlist==hubs[[i]])))
    }
       
     ResMat<-matrix(ncol=2, nrow=length(hubs))
     colnames(ResMat)<-c(colVec[1], colVec[2])
     rownames(ResMat)<-hubs
     ResMat[,1]<-NumEd1
     ResMat[,2]<-NumEd2
    
     myList[[length(myList)+1]]<-ResMat
     
    }
    if(length(myList)>0)
    {
      myList
    }
    else{
      print("There are no mixed hubs to the given threshold")
    }


}
else if(length(colVec)==1)
{
 print("There are no mixed hubs, please call the function get.hubs()")
}


}