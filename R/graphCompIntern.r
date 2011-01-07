######################################################################
############ function to determine common edges ######################
######################################################################

##This function will be called internelly in the function colorNodes()
##It returns common edges

.commonEdgesInd <-
function(edgesList){
 inter <- unique(unlist(edgesList))
for(i in 1:length(edgesList)) {
   inter <- intersect(inter, edgesList[[i]])
   if(length(inter)==0)
   {
    result <- -1
    return (result)
   }
}
result<-inter
return (result)
}



###########################################################################
#########Modified function from Rgraphviz package ########################################
###########################################################################

 .pieGlyph1 <- function (x, xpos, ypos, labels = names(x), edges = 200,
    radius = 0.8, density = NULL, angle = 45, col = NULL, border =
    NULL, lty = NULL, main = NULL, ...)
{
    if (!is.numeric(x) || any(is.na(x)))
        stop("pie: `x' values must be positive.")
    if (is.null(labels))
        labels <- as.character(1:length(x))
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    if (is.null(col))
        col <- if (is.null(density))
            c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk", "white")
        else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    for (i in 1:nx)
  {
        n <- max(2, floor(edges * dx[i]))
        t2p <- 2 * pi * seq(x[i], x[i + 1], length = n)
        xc <- c(cos(t2p), 0) * radius+xpos
        yc <- c(sin(t2p), 0) * radius+ypos
        polygon(xc, yc, density = density[i], angle = angle[i],
            border = border[i], col = col[i], lty = lty[i])
        ## plot labels (this is a patch by Fraser Sim)
        t2p <- 2 * pi * mean(x[i + 0:1])
        xc <- cos(t2p) * radius * c(1,1.1,1.2) + xpos
        yc <- sin(t2p) * radius * c(1,1.1,1.2) + ypos
        lab <- as.character(labels[i])

        if (!is.na(lab) && nzchar(lab))
        {
            lines(xc[1:2], yc[1:2])
            text(xc[3], yc[3], labels[i], xpd = TRUE,
                 adj = ifelse(xc < xpos, 1, ifelse(xc == xpos, 0.5,
                                                   0)), ...)
        }
    }
    invisible(NULL)
}

###############################################################################
##### Modified compare2Graphs() function ######################################
###############################################################################

compGraphs <-
function(graphList, colorVector=c("blue", "green4", "red"), bgColor="white",
          unConNodColor="gray48", NodeLabCol="white" ,cexx=2.0, 
          TitleOfGraph="The comparative graph of graph1 and graph2")
{
  if(length(setdiff(nodes(graphList[[1]]), nodes(graphList[[2]])))==0 &&
      length(setdiff(nodes(graphList[[2]]), nodes(graphList[[1]])))==0)
{
  if(length(colorVector)==(length(graphList)+1))
 {
  require(Rgraphviz)

  edges.list<-list()

  for(i in 1:length(graphList))
  {
   edges.list[[i]]<-edgeNames(graphList[[i]])
  }

   commonEdg <- .commonEdgesInd(edges.list)

   indicesComEdg<-which(edges.list[[1]] %in% commonEdg)
   indicesGr1<-which(!edges.list[[1]] %in% edges.list[[2]])
   indicesGr2<-which(!edges.list[[2]] %in% edges.list[[1]])


   outGraph<-graphList[[1]]   # choose the structure of the first graph as structure for the output graph

    for(i in 2:length(edges.list))
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
       outGraph<-as(Mat,"graphNEL")


    edges.outGr<-edgeNames(outGraph)

    col.vec<-rep("black", length(edges.outGr))
    names(col.vec)<-edges.outGr
    col.vec[edges.list[[1]][indicesComEdg]] <- colorVector[3]
    col.vec[edges.list[[1]][indicesGr1]] <- colorVector[1]
    col.vec[edges.list[[2]][indicesGr2]] <- colorVector[2]

   

     n<-length(nodes(outGraph))
     m<-matrix(ncol=3, nrow=n)
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
            m[i,1]=(sum(L1==actNode))/nL
            m[i,2]=(sum(L2==actNode))/nL
            m[i,3]=(sum(Lcom==actNode))/nL
         }
          else
         {

            m[i,1]=1
            m[i,2]=0
            m[i,3]=0

         }
     }
     
     
          if(length(unique(colorVector))==3)
          {
           colList<-colorVector
           GrToPlot<-0
           Title<- "The comparative graph of graph1 and graph2"
           
           ###  unconnected nodes
               unverbNodes<-c()
                
               for(i in colnames(Mat))
               {
                 if(length(which(Mat[i,]>0))==0)
                 unverbNodes<-c(unverbNodes,i)
               }
          } else {
                  if(colorVector[2]==bgColor && colorVector[3]==bgColor)
                  { colList<-rep(colorVector[1],3)
                      GrToPlot<-1
                      Title<-"Graph with edges existing in graph1 only"
                      
                       edgesGr1<- edges.list[[1]][indicesGr1]
                       if(length(edgesGr1)>0)
                       {
                         verbNGr1<-unique(unlist(strsplit(edgesGr1, "~")))
                         indVerbNodesGr1<-which(nodes(outGraph) %in% verbNGr1)
                         unverbNodes<-nodes(outGraph)[-indVerbNodesGr1]
                       }
                       else{
                              unverbNodes<-nodes(outGraph)
                           }
                       
                  }
                  else if(colorVector[1]==bgColor && colorVector[3]==bgColor) 
                  { colList<-rep(colorVector[2],3)
                   GrToPlot<-2
                   Title<-"Graph with edges existing in graph2 only"
                   
                   edgesGr2<- edges.list[[2]][indicesGr2]
                   if(length(edgesGr2)>0)
                   {
                        verbNGr2<-unique(unlist(strsplit(edgesGr2, "~")))
                        indVerbNodesGr2<-which(nodes(graphList[[2]]) %in% verbNGr2)
                        unverbNodes<-nodes(graphList[[2]])[-indVerbNodesGr2]
                   }
                   else{
                           unverbNodes<-nodes(graphList[[2]])
                       }
                  }
                   else if(colorVector[1]==bgColor && colorVector[2]==bgColor)
                    {
                    colList<-rep(colorVector[3],3)
                     GrToPlot<-3
                     Title<-"Graph with common edges"
                     
                     verbNComGr<-unique(unlist(strsplit(commonEdg, "~")))
                      indVerbNComGr<-which(nodes(outGraph) %in% verbNComGr)
                      unverbNodes<-nodes(outGraph)[-indVerbNComGr]
                    }
                  }
     
 
     makeNodeDrawFunction <- function(x)
     {
        force(x)
        function(node, ur, attrs, radConv)
      {
        nc <- getNodeCenter(node)
         nNN<-name(node)
          
                  
         if(length(which(unverbNodes==name(node)))==0)
          {     
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
        drawFuns <- apply(m, 1, makeNodeDrawFunction)

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
          aa<-strsplit(edgeNames(outGraph), "~")
          unLaa<-unlist(aa)
          ii<-which(!nn %in% unLaa)
         
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
        plot(outGrlayout, drawNode = drawFuns, main = Title, col.main=mainCol)

        if( GrToPlot==0)
        {       
           if(length(commonEdg)!=0)
           {
                 graphL<-c("Common edges")
                 colList<-c(colorVector[3])
                 lwds<-c(2)

               if(length( indicesGr1)!=0)
               {

                 graphL<-c(graphL, "Edges of graph 1")
                 colList<-c(colList, colorVector[1])
                 lwds<-c(lwds, 2)

               }
                if(length( indicesGr2)!=0)
               {

                 graphL<-c(graphL, "Edges of graph 2")
                 colList<-c(colList, colorVector[2])
                 lwds<-c(lwds, 2)

               }
                  

                 legend("bottomright", legend=graphL, col=colList, lwd=lwds, text.col=txtCol, box.col=txtCol)
           }

           else{

                 if(length( indicesGr1)!=0)
                  {
                     graphL<-c(graphL, "Edges of graph 1")
                     colList<-c(colList, colorVector[1])
                     lwds<-c(lwds, 2)

                  }
                  if(length( indicesGr2)!=0)
                  {
                    graphL<-c(graphL, "Edges of graph 2")
                    colList<-c(colList, colorVector[2])
                    lwds<-c(lwds, 2)
                  }
                 legend("bottomright", legend=graphL, col=colList, lwd=lwds, text.col=txtCol, box.col=txtCol)

                }
           }

       return(outGrlayout)
  }
   else{ print("For every graph one color is needed + one color for commonEdges")}

 }else{print("The graphs should have the same node-set")}
}

 ############modCompare2Graphs()########################################################
modCompare2Graphs <-
function(graphList, colorVector=c("blue", "green4", "red"), bgColor="white", unConNodColor="gray48", NodeLabCol="white" ,cexx=2.5,
            graphTitle="The comparative graph of graph1 and graph2", legendGr1="Edges of graph 1", legendGr2="Edges of graph 2",
            legendPosition="bottomright", nW=TRUE)
{
      require(Rgraphviz)                                          ### load the Rgraphviz package

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


   outGraph<-graphList[[1]]                                   ### choose the structure of the first graph as structure for the output graph

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

    #edges.outGr[indicesComEdg]

    col.vec<-rep("black", length(edges.outGr))                ### initialize the color vector with black
    names(col.vec)<-edges.outGr                               ### name color vector with names of edges of the output graph
    col.vec[edges.list[[1]][indicesComEdg]] <- colorVector[3]   ### color common edges with the last color in the color list
    col.vec[edges.list[[1]][indicesGr1]] <- colorVector[1]      ### color edges of graph 1 with the first color in the color list
    col.vec[edges.list[[2]][indicesGr2]] <- colorVector[2]      ### color edges of graph 2 with the second color in the color list

    #index2 <-which(edgeNames(gr) %in% edges)
    #col.vec[index2] <- color2

    #names(col.vec)<-edges.outGr

 ###   edgeRenderInfo(outGraph)=list(col=col.vec)
  ###  y=layoutGraph(outGraph, attrs=list(node=list(shape="ellipse",
  ###  fixedsize=FALSE)))
      #hier
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
         #  GrToPlot<-0
           Title<-graphTitle

           ###  unverbundene Knoten
               unverbNodes<-c()

               for(i in colnames(Mat))
               {
                 if(length(which(Mat[i,]>0))==0)
                 unverbNodes<-c(unverbNodes,i)
               }



      #   m1<-matrix(0,ncol=3, nrow=length(unverbNodes))
      #    rownames(m1) <-unverbNodes
      #    m1[,1]=1

        #outGrlayout<- agopen(outGraph, name = "outGraph")
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

    # outGrlayout=layoutGraph(outGraph, attrs=list(node=list(shape="ellipse",
  #  fixedsize=FALSE)))
  #
        outGrlayout<- agopen(outGraph, name = "outGraph")

        ee<-AgEdge(outGrlayout)

      if(length(ee)>0)
      {
        for(i in 1:length(ee))
        {
          actEdge<-paste(ee[[i]]@tail, ee[[i]]@head, sep="~")
          ee[[i]]@color<-col.vec[[actEdge]]
        }
        AgEdge(outGrlayout)<-ee
       }
          nn<-AgNode(outGrlayout)
          aa<-strsplit(edgeNames(outGraph), "~")
          unLaa<-unlist(aa)
          ii<-which(!nn %in% unLaa)
       #   for(i in 1:length(ii))
       #   {
        #   nn[[ii[i]]]@color<-"gray"
        #  }

          AgNode(outGrlayout)<-nn


        #  renderGraph(y)
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
           
           # if(nW==TRUE)
                # {
                #     dev.new()
                # }
       plot(outGrlayout, drawNode = drawF, main = graphTitle, col.main=mainCol)


             graphL<-c()
           if(commonEdg[1]!=-1)
           {

               graphL<-c("Common edges")
               colList<-c(colorVector[3])
               lwds<-c(2)

               if(length(indicesGr1)!=0)
               {

               graphL<-c(graphL, legendGr1)
               colList<-c(colList, colorVector[1])
               lwds<-c(lwds, 2)

               }
                if(length(indicesGr2)!=0)
               {

               graphL<-c(graphL, legendGr2)
               colList<-c(colList, colorVector[2])
               lwds<-c(lwds, 2)

               }


                 legend(legendPosition, legend=graphL, col=colList, lwd=lwds, text.col=txtCol, box.col=txtCol)
           }else{

                  lwds<-c()
                  colList<-c()
               if(length(indicesGr1)!=0)
               {

                  graphL<-c(graphL, legendGr1)
                  colList<-c(colList, colorVector[1])
                  lwds<-c(lwds,2)

               }


                if(length(indicesGr2)!=0)
               {

                  graphL<-c(graphL, legendGr2)
                  colList<-c(colList, colorVector[2])
                  lwds<-c(lwds, 2)

               }

                 legend(legendPosition, legend=graphL, col=colList, lwd=lwds, text.col=txtCol, box.col=txtCol)


           }
           return(outGraph)

  }
   else{ print("For every graph one color is needed + one color for commonEdges")}

 }else{print("The graphs should have the same node sets")}

}



