\name{get.hubs}
\Rdversion{1.1}
\alias{get.hubs}
\title{Get hubs of the comparative graph
}
\description{
This function returns hubs incident to edges colored with the given color and whose degrees are above the given threshold.
}
\usage{
get.hubs(RgraphObj, color, p = 20)
}

\arguments{
  \item{RgraphObj}{An \link[Rgraphviz:Ragraph-class]{Ragraph} object returned from the function \code{\link{compare2Graphs}}.
}
  \item{color}{A color from the color list, which is used by the function \code{\link{compare2Graphs}} to color edges of the comparative graph.
}
 \item{p}{A numerical value (between 1 and 100) to define a node as hub. A node is considered as hub if it is incident 
to at least p percent of the total number of edges in the comparative graph. Default: 20.
}
}

\details{The first two parameters are necessary.
}
\value{
The function returns nodes whose degrees greater or equal the given threshold. The returned nodes are incident to edges colored with only 
one color. 
}

\author{Khadija El Amrani \email{Khadija.Amrani@campus.lmu.de}, Ulrich Mansmann \email{mansmann@ibe.med.uni-muenchen.de}

}


\seealso{
\code{\link{getMixed.hubs}}
}
\examples{
\dontrun{
 if(require(graph) && require(Rgraphviz))
{
set.seed(1)
g1<-randomGraph(letters[1:6], 1:4, p= 0.3)
g2<-randomGraph(letters[1:6], 1:4, p= 0.3)
compGr<-compare2Graphs(list(g1, g2), cexx=0.5)
get.hubs(compGr, "blue", p=30)
get.hubs(compGr, "green4", p=30)
get.hubs(compGr, "red", p=2)
}
}
}

\keyword{graph}
\keyword{comparison}
\keyword{hubs}
