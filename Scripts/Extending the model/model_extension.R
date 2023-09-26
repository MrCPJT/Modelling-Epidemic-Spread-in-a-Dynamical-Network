# Extending the model

# Libraries

library('EpiModel')
library("latticeExtra")
library("RColorBrewer")
library("viridisLite")
library(Cairo)
library(gridExtra)


rm(list = ls())

# Loading progress and update functions

source("...\\Scripts\\Extending the model\\progress.R")
source("...\\Scripts\\Extending the model\\update.R")

# Situation: 
# Want to improve realism: 
# - Update Model to SIRS. Account for variations of the virus. 
# - Account for holidays and lockdowns
# - Want to compare individuals that do and do not isolate
# - Compare vaccine vs NO vaccine case

#### Network ####

n = 1000
n.sims = 10
n.steps = 1000

n.iso = n/2
n.niso = n/2

nw <- network_initialize(n)

nw <- set_vertex_attribute(nw, "group", rep(1:2, c(n.iso, n.niso)))

formation <- ~edges + nodefactor("group")

target.stats <- c(n*1.5/2, n*1/2)

coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 30)

est <- netest(nw, formation, target.stats, coef.diss)

#### Simulation ####

param <- param.net(inf.prob = 0.05,
                   act.rate = 9.6, rs.rate = 0.011,
                   ir.rate = 0.05,
                   param.updater.list = list.of.updaters) 

init <- init.net(i.num = 1, i.num.g2 = 1, 
                 r.num = 0, r.num.g2 = 0)

control <- control.net(type = NULL,
                       nsteps = n.steps,
                       nsims = n.sims,
                       updater.FUN = updater.net,
                       infect.FUN = infection.net,
                       progress.FUN = progress,
                       recovery.FUN = NULL)

sim <- netsim(est, param, init, control)

xup = 1000
yup = 750

# Infections Plot
par(mar = c(3,3,1,1), mgp = c(2,1,0))
Cairo(file="Duration30Plot.png", 
      type="png",
      units="in",
      bg="transparent",
      width=8, 
      height=6,
      dpi=250)
plot(sim, y = c("r.num", "s.num", "s.num.g2", "i.num","i.num.g2"), mean.col = c("chartreuse4","dodgerblue","cornflowerblue", "firebrick", "darkorange"), 
                qnts.col = c("chartreuse4","dodgerblue","cornflowerblue", "firebrick", "darkorange"),
                ylim = c(0,yup), xlim = c(0,xup), mean.lwd = 1, mean.smooth = FALSE,
     qnts = 0.8, qnts.alpha = 0.5, qnts.smooth = FALSE,
     ylab=list('Number',cex=1.2), xlab=list('Time',cex=1.2), legend = TRUE, 
     main=list("",cex=1.2)) + title("Duration = 30", line = -0.7) + axis(side=1, at=seq(0,xup,100), labels = seq(0,xup,100)) + axis(side=2, 
              at=seq(0,yup,100), labels = seq(0,yup,100))
dev.off()

yscale.components.HEAT1 <- function(...)
  
#### Setting defaults ####
  
{
  ans <- yscale.components.default(...)
  ans$right <- ans$left
  ticklabels1 <- seq(0,1000,100)
  ans$left$ticks$at  <- ticklabels1
  ans$left$labels$at <- ticklabels1
  ans$left$labels$labels <- ticklabels1
  ans
}

xscale.components.HEAT1 <- function(...)
  
{
  ans <- xscale.components.default(...)
  ans$top <- ans$bottom
  ticklabels2 <- seq(100,1000,100)
  ans$bottom$ticks$at  <- ticklabels2
  ans$bottom$labels$at <- ticklabels2
  ans$bottom$labels$labels <- ticklabels2
  ans
}
 

