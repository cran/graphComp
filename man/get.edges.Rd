\name{get.edges}
\Rdversion{1.1}
\alias{get.edges}
\title{ Get colored edges
 
}
\description{
The function can be used to get edges of the comparative graph (returned from the function \code{\link{compare2Graphs}}) that are colored with 
the selected color.
}
\usage{
get.edges(RgraphObj, EdgColor)
}

\arguments{
  \item{RgraphObj}{ An \link[Rgraphviz:Ragraph-class]{Ragraph} object returned from the function \code{\link{compare2Graphs}}.
}
  \item{EdgColor}{A color from the color list, which is used by the function 
  \link[graphComp]{compare2Graphs} to color edges of the comparative graph.
}
}
\details{
The above described two parameters are necessary.
}
\value{
This function returns edges of the comparative graph that are colored with the given color.
}

\author{Khadija El Amrani \email{Khadija.Amrani@campus.lmu.de}, Ulrich Mansmann \email{mansmann@ibe.med.uni-muenchen.de}

}

\examples{
\dontrun{
 if(require(graph) &&  require(Rgraphviz))
 {
  set.seed(1)
  g1<-randomGraph(letters[1:6], 1:4, p= 0.3)
  g2<-randomGraph(letters[1:6], 1:4, p= 0.3) 
  
    compGr<-compare2Graphs(list(g1, g2), cexx=0.5)
    get.edges(compGr, "blue")
    get.edges(compGr, "green4")
    get.edges(compGr, "red")
  }
 }
}

\keyword{graph}
\keyword{comparison}
