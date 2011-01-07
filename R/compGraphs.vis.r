#
# compGraphs.vis.R
#
#
#
#
#
# Copyright (C) 2011 : Khadija El Amrani <Khadija.Amrani@campus.lmu.de>
##############################################################################


compGraphs.vis<-function(graphList)
{
if(class(graphList[[1]])== "graphNEL" && class(graphList[[2]])== "graphNEL")
{ 
    library(rpanel)
    
      if(length(setdiff(nodes(graphList[[1]]), nodes(graphList[[2]])))==0 &&    ### check whether the node sets of given graphs are 
      length(setdiff(nodes(graphList[[2]]), nodes(graphList[[1]])))==0)     ### equal
{
  tmp=-1
  ## Identify functions to be called when buttons are pressed
  plot.compGraph<-function(panel)
  {
  tmp<-0
  with(panel,{
   Colors<-as.character(Colors)                                             
    compGraphs(graphList=graphList, colorVector=c(Colors[1], Colors[2], Colors[3]),
    bgColor=Colors[4], unConNodColor=Colors[6], NodeLabCol=Colors[5] ,cexx=panel$cexx )
    })
    panel
    }
 ###g1
 plot.graph1<-function(panel){
 tmp<-1
  with(panel,{
   Colors<-as.character(Colors)
   
 compGraphs(graphList=graphList, colorVector=c(Colors[1], Colors[4], Colors[4]),
  bgColor=Colors[4], unConNodColor=Colors[6], NodeLabCol=Colors[5] ,cexx=panel$cexx )
    })
    panel
    }
 ##g2
 plot.graph2<-function(panel){
 tmp<-2
     with(panel,{
   Colors<-as.character(Colors)
  compGraphs(graphList=graphList, colorVector=c(Colors[4], Colors[2], Colors[4]),
  bgColor=Colors[4], unConNodColor=Colors[6], NodeLabCol=Colors[5] ,cexx=panel$cexx )
   })
   panel
   }
##g3
 plot.comGraph<-function(panel){
 tmp<-3
   with(panel,{
   Colors<-as.character(Colors)
   compGraphs(graphList=graphList, colorVector=c(Colors[4], Colors[4], Colors[3]),
   bgColor=Colors[4], unConNodColor=Colors[6], NodeLabCol=Colors[5] ,cexx=panel$cexx )
  })
  panel
 }
 
  changeCex<-function(panel){
   with(panel,{
  if(tmp==0) plot.compGraph()
  else if(tmp==1) plot.graph1()
  else if(tmp==2) plot.graph2()
  else if(tmp==3)  plot.comGraph()
  })
  panel
 }
          ## Create a panel object
 
   panel <- rp.control("Visual Comparison of Graphs")
 
 ## Add buttons to the panel
       rp.button(panel, action = plot.compGraph, title = "The comparative graph")
       rp.button(panel, action = plot.graph1, title = "Graph1")
       rp.button(panel, action = plot.graph2, title = "Graph2")
       rp.button(panel, action =  plot.comGraph, title = "Common graph")
 ## Add text entries to the panel
       rp.textentry(panel,Colors, initval=c("blue", "green4", "red", "white", "white", "gray48"), labels =c("color graph1", "color graph2", "color com.edges", "background color", "node labels color", "unconnected nodes color"))
 ## Add Slider to the panel
        rp.slider(panel, var=cexx, action=changeCex,initval=2.0,from=0.2, to=4.5, title="cex")

 }else{print("The graphs should have the same node sets")}
 }else{print("The graphs should be of class graphNEL!")}
}








