#  compare2Graphs.R 
#
#  Visual comparison of two graphs
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

compare2Graphs <-
function(graphList, colorVector=c("blue", "green4", "red"), bgColor="white", unConNodColor="gray48", NodeLabCol="white" ,cexx=2.5,
            graphTitle="The comparative graph of graph1 and graph2", legendGr1="Edges of graph 1", legendGr2="Edges of graph 2",
            legendPosition="bottomright")
{
      require(Rgraphviz)                                          ### load the Rgraphviz package 
if(class(graphList[[1]])== "graphNEL" && class(graphList[[2]])== "graphNEL")
{      
  if(length(setdiff(nodes(graphList[[1]]), nodes(graphList[[2]])))==0 &&    ### check whether the node sets of given graphs are 
      length(setdiff(nodes(graphList[[2]]), nodes(graphList[[1]])))==0)     ### equal
{
  if(length(colorVector)==(length(graphList)+1))                ### check whether the color list is valid   
 {


  edges.list<-list()                                          ### initialize a list to save edges

  for(i in 1:length(graphList))                               ### save edges of the given graphs
  {
   edges.list[[i]]<-edgeNames(graphList[[i]])                 ### in a list
  }

   commonEdg <- .commonEdgesInd(edges.list)                    ### get common edges 
                                                            
   indicesComEdg<-which(edges.list[[1]] %in% commonEdg)       ### get indices of common edges
   indicesGr1<-which(!edges.list[[1]] %in% edges.list[[2]])   ### get indices of edges only in graph2 
   indicesGr2<-which(!edges.list[[2]] %in% edges.list[[1]])   ### get indices of edges only in graph1

  # if(length(indicesGr1) <= length(indicesGr2))
  # {
    outGraph<-graphList[[1]]                                   ### choose the structure of the first graph as structure for the output graph
   #}else{}
   
   
    for(i in 2:length(edges.list))                            ### create the adjacency matrix of the common graph (with all edges of the 
                                                              ### given graphs)
    {
     edges <- edges.list[[i]][which(!edges.list[[i]] %in% edges.list[[1]])]

        List<-strsplit(edges, "~")
        Mat<-as(outGraph, "matrix")
        if(length(edges)!=0)
         {
          for(j in 1:length(edges))
           {
              Mat[List[[j]][1], List[[j]][2]]=1
              Mat[List[[j]][2], List[[j]][1]]=1
           }
         }

    }
    
    Mat[Mat<0]=1
    Mat[Mat>0]=1

   ####
       outGraph<-as(Mat,"graphNEL")                           ### create the common graph from the adjacency matrix

    
    edges.outGr<-edgeNames(outGraph)                          ### save edges of the output graph in a vector

    

    col.vec<-rep("black", length(edges.outGr))                ### initialize the color vector with black 
    names(col.vec)<-edges.outGr                               ### name color vector with names of edges of the output graph
    col.vec[edges.list[[1]][indicesComEdg]] <- colorVector[3]   ### color common edges with the last color in the color list
    col.vec[edges.list[[1]][indicesGr1]] <- colorVector[1]      ### color edges of graph 1 with the first color in the color list
    col.vec[edges.list[[2]][indicesGr2]] <- colorVector[2]      ### color edges of graph 2 with the second color in the color list

   
     n<-length(nodes(outGraph))                               ### n is the number of nodes of the output graph
     m<-matrix(ncol=3, nrow=n)                                ### initialize a matrix with 3 columns and n rows
     rownames(m) <- nodes(outGraph)

     L1<-unlist(strsplit(edges.list[[1]][indicesGr1], "~"))
     L2<-unlist(strsplit(edges.list[[2]][indicesGr2], "~"))
    ######

     if(commonEdg[1]!=-1)
     {
       Lcom<-unlist(strsplit(commonEdg, "~"))
     }
     else  {Lcom<-c()}
     for(i in 1:n)
     {
         actNode<-nodes(outGraph)[i]
         nL<-(sum(L1==actNode)+sum(L2==actNode)+sum(Lcom==actNode))
         if(nL!=0)
         {
            m[i,1]=(sum(L1==actNode))/nL      ### for each node, save in the firt column of the matrix the proportion of edges of graph 1 
            m[i,2]=(sum(L2==actNode))/nL      ### for each node, save in the firt column of the matrix the proportion of edges of graph 2
            m[i,3]=(sum(Lcom==actNode))/nL    ### for each node, save in the firt column of the matrix the proportion of common edges
         }
          else
         {                                    ### if the node is unconnected, write 1 in the first column of the matrix

            m[i,1]=1
            m[i,2]=0
            m[i,3]=0

         }
     }



           colList<-colorVector
     
           #Title<-graphTitle

           ###  unconnected nodes
               unverbNodes<-c()

               for(i in colnames(Mat))
               {
                 if(length(which(Mat[i,]>0))==0)
                 unverbNodes<-c(unverbNodes,i)
               }


     drawNodeAsPieChart <- function(x)      ### Function to draw nodes as pie charts
     {                                        ### The function pieGlyph is avialable in the Rgraphviz package, this function was 
                                              ### modified 
        force(x)
        function(node, ur, attrs, radConv)
      {
        nc <- getNodeCenter(node)
         nNN<-name(node)

                  ##



         if(length(which(unverbNodes==name(node)))==0)
          {        ##
             .pieGlyph1(x, xpos = getX(nc), ypos = getY(nc), radius = getNodeRW(node),
              col = colList, labels=NA, border=NA)
               text(getX(nc), getY(nc), nNN, cex = cexx,
               col =NodeLabCol, font = 4)

          }
         else{
                  .pieGlyph1(x, xpos = getX(nc), ypos = getY(nc), radius = getNodeRW(node),
                   col = c(unConNodColor,unConNodColor, unConNodColor), labels=NA, border=NA)
                     text(getX(nc), getY(nc), nNN, cex = cexx,
                       col = NodeLabCol, font = 4)

            }
      }
    }



        drawF <- apply(m, 1, drawNodeAsPieChart)

        edgeRenderInfo(outGraph)=list(col=col.vec)
        hh<-rep("white", length(nodes(outGraph)))
        nodeRenderInfo(outGraph)=list(textCol=hh)


        outGrlayout<- agopen(outGraph, name = "outGraph")

        ee<-AgEdge(outGrlayout)


        for(i in 1:length(ee))
        {
          actEdge<-paste(ee[[i]]@tail, ee[[i]]@head, sep="~")
          ee[[i]]@color<-col.vec[[actEdge]]
        }
        AgEdge(outGrlayout)<-ee

          nn<-AgNode(outGrlayout)
         # aa<-strsplit(edgeNames(outGraph), "~")
         # unLaa<-unlist(aa)
         # ii<-which(!nn %in% unLaa)
      

          AgNode(outGrlayout)<-nn
          
          
        
        par(bg=bgColor)
         if(bgColor=="black")
           {
               txtCol<-"white"
               mainCol<-"white"
           }
       else{
               txtCol<-"black"
               mainCol<-"black"
           }
       plot(outGrlayout, drawNode = drawF, main = graphTitle, col.main=mainCol)


             graphL<-c()
           if(commonEdg[1]!=-1)
           {

               graphL<-c("Common edges")
               colList<-c(colorVector[3])
               lwds<-c(2)

               if(length( indicesGr1)!=0)
               {

               graphL<-c(graphL, legendGr1)
               colList<-c(colList, colorVector[1])
               lwds<-c(lwds, 2)

               }
                if(length( indicesGr2)!=0)
               {

               graphL<-c(graphL, legendGr2)
               colList<-c(colList, colorVector[2])
               lwds<-c(lwds, 2)

               }


                 legend(legendPosition, legend=graphL, col=colList, lwd=lwds, text.col=txtCol, box.col=txtCol)
           }

           else{
                  lwds<-c()
               if(length( indicesGr1)!=0)
               {

                  graphL<-c(graphL, legendGr1)
                  colList<-c(colList, colorVector[1])
                  lwds<-c(lwds, 2)

               }
                if(length( indicesGr2)!=0)
               {

                  graphL<-c(graphL, legendGr2)
                  colList<-c(colList, colorVector[2])
                  lwds<-c(lwds, 2)

               }
                 legend(legendPosition, legend=graphL, col=colList, lwd=lwds, text.col=txtCol, box.col=txtCol)

           }


myDataFrame<-data.frame(Color=c(colorVector, "sum"), Number.edges=c(length(get.edges(outGrlayout, colorVector[1])), length(get.edges(outGrlayout, colorVector[2])), length(get.edges(outGrlayout, colorVector[3])), length(edgeNames(outGrlayout))))

   print(myDataFrame)
       return(outGrlayout)
  }
   else{ print("For every graph one color is needed + one color for commonEdges")}

 }else{print("The graphs should have the same node sets")}

 }else{print("The graphs should be of class graphNEL!")}
}



