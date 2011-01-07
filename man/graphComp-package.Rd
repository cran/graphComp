\name{graphComp-package}
\Rdversion{1.1}
\alias{graphComp-package}
\alias{graphComp}
\docType{package}
\title{
Visual Graph Comparison
}
\description{
The package contains functions to support the visual comparison of graphs on the same node set. 
}
\details{
\tabular{ll}{
Package: \tab graphComp\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-01-07\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}

}
\author{
Khadija El Amrani <Khadija.Amrani@campus.lmu.de>, Ulrich Mansmann <mansmann@ibe.med.uni-muenchen.de>


Maintainer: Khadija El Amrani <Khadija.Amrani@campus.lmu.de>
}

\keyword{ package }
\keyword{ graph }
\keyword{ comparison }
\examples{
\dontrun{
 if(require(graph) && require(Rgraphviz))
{
g1<-randomGraph(letters[1:6], 1:4, p= 0.3)
g2<-randomGraph(letters[1:6], 1:4, p= 0.3)
par(mfrow=c(1,3))
plot(g1, main="Graph1")
plot(g2, main="Graph2")
compare2Graphs(list(g1, g2), cexx=0.5)
}
}
}
