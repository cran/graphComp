\name{compGraphs.vis}
\Rdversion{1.1}
\alias{compGraphs.vis}

\title{ Visual comparison of graphs
}
\description{Function to compare two graphs on the same node set.

}
\usage{
compGraphs.vis(graphList)
}

\arguments{
  \item{graphList}{A list of two \link[graph:graphNEL-class]{graphNEL} graphs to be compared. The graphs should have the same node set.
}
}
\details{The function has to be called with the mandatory argument \code{graphList}. Other additional parameters could be passed via the control 
panel, which appears after calling the function.
}
\value{no returned value.
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
   compGraphs.vis(list(g1, g2))
  }
 }
}

\keyword{graph}
\keyword{comparison}
