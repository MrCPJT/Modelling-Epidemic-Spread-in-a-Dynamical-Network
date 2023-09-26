# Number of Nodes vs Concurrency

# Libraries

library('EpiModel')
library("latticeExtra")
library("RColorBrewer")
library("viridisLite")
library(Cairo)
library(gridExtra)


rm(list = ls())

# Initialising Vectors

a1 = c(); b1 = c(); c1 = c(); d1 = c(); e1 = c(); f1 = c()
g1 = 0

# Simulations

n = seq(100, 1000, by=100)

system.time(result <- for (n in n) {


  nw <- network_initialize(n)
  formation <- ~edges + concurrent
  param <- param.net(inf.prob = 0.05, act.rate = 9.6, rec.rate = 0.05)
  init <- init.net(i.num = 1, r.num = 0)
  control <- control.net(type = "SIR", nsims = 10, nsteps = 1000)  
  
  conc = seq(4,40,4)
  
  for (conc in conc) {
    
    g1 = 0
    target.stats <- c(n/2, n*conc/200)
    coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 30)
    est <- netest(nw, formation, target.stats, coef.diss)
    
    invisible(capture.output(sim <- netsim(est, param, init, control)))
    df <- as.data.frame(sim)
    final <- subset(df, time == 1000)
    final$epidemic <- final$s.num < final$r.num + final$i.num
    
    for (q in seq(1,10,1)) {
      
      short <- subset(df, sim == q)
      short$epidemic <- (n*0.2) < max(short$i.num)
      
      if (short$epidemic == TRUE){
        
        g1 = g1 + 1
        
      }
    }
    
    a1 <- c(a1,n)
    b1 <- c(b1,conc)
    c1 <- c(c1,sum(final$epidemic)/10)
    d1 <- c(d1,max(df$i.num))
    e1 <- c(e1, short$i.num)
    f1 <- c(f1, g1/10)  
    
  }
}

)

simdata_nc <- data.frame(meandeg = 1,meanduration = 30,nodes = a1, act = 9.6, transmission = 0.05, recovery = 0.05, 
                         infected = 1, recovered = 0, sims = 10,proportion = c1,peak.inf = round(d1, 0), shortprop = f1, concurrency = b1)

result_nc <- data.frame('Mean Degree' = 1, 'Mean Duration' = 30, 'Nodes' = a1, 'Act Rate' = 9.6, 'Trans.Prob.' = 0.05, 'Recovery Rate' = 0.05
                        , 'Long-Term Prop.' = c1, 'Short-Term Prop.' = f1, 'Peak Infected' = round(d1, 0), 'Concurrency' = b1)

