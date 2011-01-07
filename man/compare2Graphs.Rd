\name{compare2Graphs}
\Rdversion{1.1}
\alias{compare2Graphs}

\title{Visual comparison of graphs 
  
}
\description{
Function to compare two graphs on the same node set
}
\usage{
compare2Graphs(graphList, colorVector = c("blue", "green4", "red"), bgColor = "white", 
unConNodColor = "gray48", NodeLabCol = "white", cexx = 2.5, 
graphTitle = "The comparative graph of graph1 and graph2", legendGr1 = "Edges of graph 1", 
legendGr2 = "Edges of graph 2", legendPosition = "bottomright")
}

\arguments{
  \item{graphList}{A list of two \link[graph:graphNEL-class]{graphNEL} graphs to be compared. The graphs should have the same node set.
}
  \item{colorVector}{A vector of three colors used to distinguish edges of the given graphs and their common edges.
   The first or second color in the vector is used to color edges of the first or second graph (in the graphList), 
   respectively, and the last color is used to color the common edges of the given graphs. 
   Default: blue, green4 and red.
 
}
  \item{bgColor}{  The color to be used for the background of the plot. Default: \code{white}.
}
  \item{unConNodColor}{ The color to be used to color unconnected nodes. Default: \code{gray48}.

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
This function enables the visualization of the comparative graph of two \link[graph:graphNEL-class]{graphNEL} graphs with the same node set. 
}
\value{
compare2Graphs returns an \link[Rgraphviz:Ragraph-class]{Ragraph} object, which can be given as parameter
to the subfunctions: \code{\link{get.edges}}, \code{\link{get.hubs}} and \code{\link{getMixed.hubs}} to get edges or hubs.
}

\author{Khadija El Amrani \email{Khadija.Amrani@campus.lmu.de}, Ulrich Mansmann \email{mansmann@ibe.med.uni-muenchen.de}
}



\examples{
\dontrun{
 if(require(graph) && require(Rgraphviz))
{
 set.seed(1)
 g1<-randomGraph(letters[1:6], 1:4, p= 0.3)
 g2<-randomGraph(letters[1:6], 1:4, p= 0.3)
 par(mfrow=c(1,3))
 plot(g1, main="Graph1")
 plot(g2, main="Graph2")
 compare2Graphs(list(g1, g2), cexx=0.5)
}
}
}

\keyword{graph}
\keyword{comparison}
