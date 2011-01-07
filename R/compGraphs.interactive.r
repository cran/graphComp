#  compGraphs.interactive.R 
#
#  Visual comparison of graphs
#
#
#
#
#
#
#
# Copyright (C) 2011 : Khadija El Amrani <Khadija.Amrani@campus.lmu.de>
############################################################################
############### Compare two graphs with the same node-set ##################
############################################################################

compGraphs.interactive <-
function(graphList, ColorVector=c("blue", "green4", "red"), bgColor="white", unConNodColor="gray48", NodeLabCol="white" ,cexx=2.5,
         graphTitle="The comparative graph of graph1 and graph2", legendGr1="Edges of graph 1", legendGr2="Edges of graph 2",
         legendPosition="bottomright")
{
 if(class(graphList[[1]])== "graphNEL" && class(graphList[[2]])== "graphNEL")
{ 
     newWindow=TRUE
  if(length(setdiff(nodes(graphList[[1]]), nodes(graphList[[2]])))==0 &&    ### check whether the node sets of given graphs are
      length(setdiff(nodes(graphList[[2]]), nodes(graphList[[1]])))==0)       ### equal

{
  if(length(ColorVector)==(length(graphList)+1))                ### check whether the color list is valid
 {

 outGraph<-modCompare2Graphs(graphList=graphList,colorVector=ColorVector, bgColor=bgColor, unConNodColor=unConNodColor,
                  NodeLabCol=NodeLabCol ,cexx=cexx, graphTitle=graphTitle, legendGr1=legendGr1, legendGr2=legendGr2,
                  legendPosition=legendPosition)

     NNodesX<-c()
     NNodesY<-c()
     outGrlayout<- agopen(outGraph, name = "outGraph")
     AgN<-AgNode(outGrlayout)
  for(i in 1:length(nodes(outGraph)))
  {
       NNodesX<-c(NNodesX, AgN[[i]]@center@x)
       NNodesY<-c(NNodesY, AgN[[i]]@center@y)

  }

          nodeRWidth<- AgN[[1]]@rWidth
  
          clicked <-FALSE
          Mat1<-as(outGraph, "matrix")
          unverbNodes<-c()
          for(i in colnames(Mat1))
               {
                 if(length(which(Mat1[i,]>0))==0)
                 unverbNodes<-c(unverbNodes,i)
               }
    while(clicked==FALSE)
     {
               klick<-(locator(n=1))
               kx<-klick[[1]]
               ky<-klick[[2]]
               #which node is clicked
                NodVec<-c()
         for(i in 1:length(NNodesX))
         {
           if(kx<=(NNodesX[i]+nodeRWidth) && kx>=(NNodesX[i]-nodeRWidth) && ky<=(NNodesY[i]+nodeRWidth) && ky>=(NNodesY[i]-nodeRWidth))
           {
              ActNN<-nodes(outGraph)[i]
              if(length(which(unverbNodes==ActNN))==0)
              {
                     vec<-Mat1[ActNN, ]
                     NodVec<-c(ActNN)
                   for(j in 1:length(vec))
                   {
                     if(vec[[j]]==1)
                      {
                         NodVec<-c(NodVec, colnames(Mat1)[j])
                      }
                   }
                      gg1<-subGraph(NodVec, graphList[[1]])
                      gg2<-subGraph(NodVec, graphList[[2]])

                if(newWindow)
                 {
                     dev.new()
                 }
                   compGraphs.interactive(graphList=list(gg1, gg2),ColorVector=ColorVector, bgColor=bgColor, unConNodColor=unConNodColor,
                                NodeLabCol=NodeLabCol, cexx=1.0, graphTitle="Subgraph of the comparative graph", legendGr1=legendGr1,
                                legendGr2=legendGr2, legendPosition=legendPosition)#,
                                
             }
           }
         }
      }
  }else{ print("For every graph one color is needed + one color for commonEdges")}

 }else{print("The graphs should have the same node sets")} 
 }else{print("The graphs should be of class graphNEL!")}

}


