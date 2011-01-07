\name{compGraphs.interactive}
\Rdversion{1.1}
\alias{compGraphs.interactive}

\title{Interactive comparison of graphs
}
\description{This function supports additionally to the comparison of two graphs, the visualization of subgraphs of the comparative graph.
 
}
\usage{
compGraphs.interactive(graphList, ColorVector = c("blue", "green4", "red"), bgColor = "white",
 unConNodColor = "gray48", NodeLabCol = "white", cexx = 2.5, 
 graphTitle = "The comparative graph of graph1 and graph2", 
 legendGr1 = "Edges of graph 1", legendGr2 = "Edges of graph 2",
  legendPosition = "bottomright")
}

\arguments{
  \item{graphList}{A list of two \link[graph:graphNEL-class]{graphNEL} graphs to be compared. The graphs should have the same node set.
}
  \item{ColorVector}{A vector of three colors used to distinguish edges of the given graphs and their common edges.  
 The first or second color in the vector is used to color edges of the first or second graph (in the graphList),
 respectively, and the last color is used to color the common edges of the given graphs.   
 Default: blue, green4 and red.
}
  \item{bgColor}{The color to be used for the background of the plot. Default: \code{white}.
}
  \item{unConNodColor}{The color to be used to color unconnected nodes. Default: \code{gray48}.
}
  \item{NodeLabCol}{The color to be used for node labels. Default: \code{white}.
}
  \item{cexx}{Numeric character expansion factor; multiplied by \link[graphics]{par}("cex") yields the final character size. Default: 2.5.  
}
  \item{graphTitle}{The main title (on top) of the plot. If not specified the title        
   \code{"The comparative graph of graph1 and graph2"} is used as default.
}
  \item{legendGr1}{ Expression  to appear in the legend referring to edges of the first
   graph in the graph list. Default: \code{"Edges of graph 1"}.
}
  \item{legendGr2}{Expression  to appear in the legend referring to edges of the second
  graph in the graph list. Default: \code{"Edges of graph 2"}. 
}
  \item{legendPosition}{A keyword from the list \code{"topright", "topleft", "bottomrigth"} and \code{"bottomleft"}.  
  This places the legend on the inside of the plot frame at the given location. Default: \code{"bottomright"}.  
}

}
\details{
To visualize a subgraph of the comparative graph, click on any node and the subgraph with the clicked node and its direct neighbors 
will be plotted in a new window.
}

\author{Khadija El Amrani \email{Khadija.Amrani@campus.lmu.de}, Ulrich Mansmann \email{mansmann@ibe.med.uni-muenchen.de}
}




\examples{
\dontrun{ 
 if(require(graph)&& require(Rgraphviz))
 {
  set.seed(1)
  g1<-randomGraph(letters[1:6], 1:4, p= 0.3)
  g2<-randomGraph(letters[1:6], 1:4, p= 0.3)                                                                                           
  par(mfrow=c(1,3))                                                                                                       
  plot(g1, main="Graph1")                                                                                             
  plot(g2, main="Graph2")                                                                                             
  compGraphs.interactive(list(g1, g2), cexx=0.5)                                                                          
 }                                                                                                                           
}                                                                                                                      
}
 
\keyword{graph}        
\keyword{comparison}   
