#### Libraries ####

library('EpiModel')
library("latticeExtra")
library("RColorBrewer")
library("viridisLite")
library(Cairo)
library(gridExtra)

#### Loading the data ####

simdata_jn   <- read.csv("...\\Data\\simdata_jn.csv")
simdata_jn25 <- read.csv("...\\Data\\simdata_jn25.csv")
simdata_jn50 <- read.csv("...\\Data\\simdata_jn50.csv")
simdata_in   <- read.csv("...\\Data\\simdata_in.csv")
simdata_ij   <- read.csv("...\\Data\\simdata_ij.csv")


#### Setting defaults ####

yscale.components.HEAT <- function(...)
  
{
  ans <- yscale.components.default(...)
  ans$right <- ans$left
  ticklabels1 <- seq(0,1000,100)
  ans$left$ticks$at  <- ticklabels1
  ans$left$labels$at <- ticklabels1
  ans$left$labels$labels <- ticklabels1
  ans
}

xscale.components.HEAT <- function(...)
  
{
  ans <- xscale.components.default(...)
  ans$top <- ans$bottom
  ticklabels2 <- seq(10,100,10)
  ans$bottom$ticks$at  <- ticklabels2
  ans$bottom$labels$at <- ticklabels2
  ans$bottom$labels$labels <- ticklabels2
  ans
}

zscale.components.HEAT <- function(...)
  
{
  ans <- xscale.components.default(...)
  ans$top <- ans$bottom
  ticklabels3 <- seq(0.5,1.5,0.1)
  ans$bottom$ticks$at  <- ticklabels3
  ans$bottom$labels$at <- ticklabels3
  ans$bottom$labels$labels <- ticklabels3
  ans
}

zzscale.components.HEAT <- function(...)
  
{
  ans <- yscale.components.default(...)
  ans$right <- ans$left
  ticklabels4 <- seq(0.5,1.5,0.1)
  ans$left$ticks$at  <- ticklabels4
  ans$left$labels$at <- ticklabels4
  ans$left$labels$labels <- ticklabels4
  ans
}

#### Plotting ####

##  Long  = Full Duration     (analysing the long-term effects)
##  Short = Partial Duration  (analysing the short-term effects)

#### Mean Degree vs Mean Partnership Duration ####

long_ij <- levelplot(proportion ~ meanduration * meandeg, simdata_ij, panel = panel.levelplot.points, cex = 0, 
                     col.regions=terrain.colors(105), colorkey = list(height = .6, space = "right", col=terrain.colors(105), 
                                                                      at=seq(0,1,0.01), labels=list(at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1)),
                                                                      title=list("Proportion", rot = 90, x=grid::unit(+2,"mm"))), strip = TRUE, 
                     pretty = FALSE, cuts = 15, grid = FALSE,
                     ylab=list('Mean Degree',cex=1.2), xlab=list('Mean Partnership Duration',cex=1.2), 
                     main=list("",cex=1.2,y=grid::unit(-2, "mm")),
                     yscale.components = zzscale.components.HEAT, xscale.components = xscale.components.HEAT,
                     scales = list(tck = c(1,0)),  par.settings = list(
                       layout.widths = list(ylab.axis.padding = -0.3))) +
  layer_(panel.2dsmoother(..., n = 500))
long_ij

short_ij <- levelplot(shortprop ~ meanduration * meandeg, simdata_ij, panel = panel.levelplot.points, cex = 0, 
                     col.regions=terrain.colors(105), colorkey = list(height = .6, space = "right", col=terrain.colors(105), 
                                                                      at=seq(0,1,0.01), labels=list(at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1)),
                                                                      title=list("Proportion", rot = 90, x=grid::unit(+2,"mm"))), strip = TRUE, 
                     pretty = FALSE, cuts = 15, grid = FALSE,
                     ylab=list('Mean Degree',cex=1.2), xlab=list('Mean Partnership Duration',cex=1.2), 
                     main=list("",cex=1.2,y=grid::unit(-2, "mm")),
                     yscale.components = zzscale.components.HEAT, xscale.components = xscale.components.HEAT,
                     scales = list(tck = c(1,0)),  par.settings = list(
                       layout.widths = list(ylab.axis.padding = -0.3))) +
  layer_(panel.2dsmoother(..., n = 500))
short_ij

#### Mean Degree vs Number of Nodes ####

long_in <- levelplot(proportion ~ meandeg * nodes, simdata_in, panel = panel.levelplot.points, cex = 0, 
                     col.regions=terrain.colors(105), colorkey = list(height = .6, space = "right", col=terrain.colors(105), 
                                                                      at=seq(0,1,0.01), labels=list(at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1)),
                                                                      title=list("Proportion", rot = 90, x=grid::unit(+2,"mm"))), strip = TRUE, 
                     pretty = FALSE, cuts = 15, grid = FALSE,
                     ylab=list('Number of Nodes',cex=1.2), xlab=list('Mean Degree',cex=1.2), 
                     main=list("",cex=1.2,y=grid::unit(-2, "mm")),
                     yscale.components = yscale.components.HEAT, xscale.components = zscale.components.HEAT,
                     scales = list(tck = c(1,0)),  par.settings = list(
                       layout.widths = list(ylab.axis.padding = -0.3))) +
  layer_(panel.2dsmoother(..., n = 500))
long_in

short_in <- levelplot(shortprop ~ meandeg * nodes, simdata_in, panel = panel.levelplot.points, cex = 0, 
                     col.regions=terrain.colors(105), colorkey = list(height = .6, space = "right", col=terrain.colors(105), 
                                                                      at=seq(0,1,0.01), labels=list(at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1)),
                                                                      title=list("Proportion", rot = 90, x=grid::unit(+2,"mm"))), strip = TRUE, 
                     pretty = FALSE, cuts = 15, grid = FALSE,
                     ylab=list('Number of Nodes',cex=1.2), xlab=list('Mean Degree',cex=1.2), 
                     main=list("",cex=1.2,y=grid::unit(-2, "mm")),
                     yscale.components = yscale.components.HEAT, xscale.components = zscale.components.HEAT,
                     scales = list(tck = c(1,0)),  par.settings = list(
                       layout.widths = list(ylab.axis.padding = -0.3))) +
  layer_(panel.2dsmoother(..., n = 500))
short_in

#### Mean Partnership Duration vs Number of Nodes ####

long_jn <- levelplot(proportion ~ meanduration * nodes, simdata_jn, panel = panel.levelplot.points, cex = 0, 
                      col.regions=terrain.colors(105), colorkey = list(height = .6, space = "right", col=terrain.colors(105), 
                                                                       at=seq(0,1,0.01), labels=list(at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1)),
                                                                       title=list("Proportion", rot = 90, x=grid::unit(+2,"mm"))), strip = TRUE, 
                      pretty = FALSE, cuts = 15, grid = FALSE,
                      ylab=list('Number of Nodes',cex=1.2), xlab=list('Mean Partnership Duration',cex=1.2), 
                      main=list("",cex=1.2,y=grid::unit(-2, "mm")),
                      yscale.components = yscale.components.HEAT, xscale.components = xscale.components.HEAT,
                      scales = list(tck = c(1,0)),  par.settings = list(
                        layout.widths = list(ylab.axis.padding = -0.3))) +
  layer_(panel.2dsmoother(..., n = 500))
long_jn

short_jn <- levelplot(shortprop ~ meanduration * nodes, simdata_jn, panel = panel.levelplot.points, cex = 0, 
                     col.regions=terrain.colors(105), colorkey = list(height = .6, space = "right", col=terrain.colors(105), 
                                                                      at=seq(0,1,0.01), labels=list(at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1)),
                                                                      title=list("Proportion", rot = 90, x=grid::unit(+2,"mm"))), strip = TRUE, 
                     pretty = FALSE, cuts = 15, grid = FALSE,
                     ylab=list('Number of Nodes',cex=1.2), xlab=list('Mean Partnership Duration',cex=1.2), 
                     main=list("",cex=1.2,y=grid::unit(-2, "mm")),
                     yscale.components = yscale.components.HEAT, xscale.components = xscale.components.HEAT,
                     scales = list(tck = c(1,0)),  par.settings = list(
                       layout.widths = list(ylab.axis.padding = -0.3))) +
  layer_(panel.2dsmoother(..., n = 500))
short_jn

#### Mean Partnership Duration vs Number of Nodes (25% Concurrency) ####

long_jn25 <- levelplot(proportion ~ meanduration * nodes, simdata_jn25, panel = panel.levelplot.points, cex = 0, 
                     col.regions=terrain.colors(105), colorkey = list(height = .6, space = "right", col=terrain.colors(105), 
                                                                      at=seq(0,1,0.01), labels=list(at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1)),
                                                                      title=list("Proportion", rot = 90, x=grid::unit(+2,"mm"))), strip = TRUE, 
                     pretty = FALSE, cuts = 15, grid = FALSE,
                     ylab=list('Number of Nodes',cex=1.2), xlab=list('Mean Partnership Duration',cex=1.2), 
                     main=list("",cex=1.2,y=grid::unit(-2, "mm")),
                     yscale.components = yscale.components.HEAT, xscale.components = xscale.components.HEAT,
                     scales = list(tck = c(1,0)),  par.settings = list(
                       layout.widths = list(ylab.axis.padding = -0.3))) +
  layer_(panel.2dsmoother(..., n = 500))
long_jn25

short_jn25 <- levelplot(shortprop ~ meanduration * nodes, simdata_jn25, panel = panel.levelplot.points, cex = 0, 
                      col.regions=terrain.colors(105), colorkey = list(height = .6, space = "right", col=terrain.colors(105), 
                                                                       at=seq(0,1,0.01), labels=list(at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1)),
                                                                       title=list("Proportion", rot = 90, x=grid::unit(+2,"mm"))), strip = TRUE, 
                      pretty = FALSE, cuts = 15, grid = FALSE,
                      ylab=list('Number of Nodes',cex=1.2), xlab=list('Mean Partnership Duration',cex=1.2), 
                      main=list("",cex=1.2,y=grid::unit(-2, "mm")),
                      yscale.components = yscale.components.HEAT, xscale.components = xscale.components.HEAT,
                      scales = list(tck = c(1,0)),  par.settings = list(
                        layout.widths = list(ylab.axis.padding = -0.3))) +
  layer_(panel.2dsmoother(..., n = 500))
short_jn25

#### Mean Partnership Duration vs Number of Nodes (50% Concurrency) ####

long_jn50 <- levelplot(proportion ~ meanduration * nodes, simdata_jn50, panel = panel.levelplot.points, cex = 0, 
                     col.regions=terrain.colors(105), colorkey = list(height = .6, space = "right", col=terrain.colors(105), 
                                                                      at=seq(0,1,0.01), labels=list(at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1)),
                                                                      title=list("Proportion", rot = 90, x=grid::unit(+2,"mm"))), strip = TRUE, 
                     pretty = FALSE, cuts = 15, grid = FALSE,
                     ylab=list('Number of Nodes',cex=1.2), xlab=list('Mean Partnership Duration',cex=1.2), 
                     main=list("",cex=1.2,y=grid::unit(-2, "mm")),
                     yscale.components = yscale.components.HEAT, xscale.components = xscale.components.HEAT,
                     scales = list(tck = c(1,0)),  par.settings = list(
                       layout.widths = list(ylab.axis.padding = -0.3))) +
  layer_(panel.2dsmoother(..., n = 500))
long_jn50

short_jn50 <- levelplot(shortprop ~ meanduration * nodes, simdata_jn50, panel = panel.levelplot.points, cex = 0, 
                      col.regions=terrain.colors(105), colorkey = list(height = .6, space = "right", col=terrain.colors(105), 
                                                                       at=seq(0,1,0.01), labels=list(at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1)),
                                                                       title=list("Proportion", rot = 90, x=grid::unit(+2,"mm"))), strip = TRUE, 
                      pretty = FALSE, cuts = 15, grid = FALSE,
                      ylab=list('Number of Nodes',cex=1.2), xlab=list('Mean Partnership Duration',cex=1.2), 
                      main=list("",cex=1.2,y=grid::unit(-2, "mm")),
                      yscale.components = yscale.components.HEAT, xscale.components = xscale.components.HEAT,
                      scales = list(tck = c(1,0)),  par.settings = list(
                        layout.widths = list(ylab.axis.padding = -0.3))) +
  layer_(panel.2dsmoother(..., n = 500))
short_jn50

#### Exporting Graphs (Manually config settings - Can be looped through) ####

# Cairo(file="long_jn50.png", 
#       type="png",
#       units="in",
#       bg="white",
#       width=5, 
#       height=4,
#       dpi=250)
# long_jn50
# dev.off()
