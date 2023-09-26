# Mean Degree vs Mean Partnership Duration

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

i = seq(0.5, 1.5, by=0.1)

nw <- network_initialize(n = 100)
formation <- ~edges
param <- param.net(inf.prob = 0.05, act.rate = 9.6, rec.rate = 0.05)
init <- init.net(i.num = 1, r.num = 0)
control <- control.net(type = "SIR", nsims = 10, nsteps = 1000)

system.time(result <- for (i in i) {  
  
  j = seq(10, 100, by=10)
  
  for (j in j) {
    
    g1 = 0
    target.stats <- c(100*i/2)
    coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = j)
    est <- netest(nw, formation, target.stats, coef.diss)
    
    invisible(capture.output(sim <- netsim(est, param, init, control)))
    df <- as.data.frame(sim)
    final <- subset(df, time == 1000)
    final$epidemic <- final$s.num < final$r.num + final$i.num
    
    for (q in seq(1,10,1)) {
      
      short <- subset(df, sim == q)
      short$epidemic <- (100*0.2) < max(short$i.num)
      
      if (short$epidemic == TRUE){
        
        g1 = g1 + 1
        
      }
    }
    
    a1 <- c(a1,i)
    b1 <- c(b1,j)
    c1 <- c(c1,sum(final$epidemic)/10)
    d1 <- c(d1,max(df$i.num))
    e1 <- c(e1, short$i.num)
    f1 <- c(f1, g1/10)  
    
  }
}

)

# Degree and Duration - Nodes: 100

simdata_ij <- data.frame(meandeg = a1,meanduration = b1,nodes = 100, act = 9.6, transmission = 0.05, recovery = 0.05, 
                      infected = 1, recovered = 0, sims = 10,proportion = c1,peak.inf = round(d1, 0), shortprop = f1, concurrency = '0%')

result_ij <- data.frame('Mean Degree' = a1, 'Mean Duration' = b1, 'Nodes' = 100, 'Act Rate' = 9.6, 'Trans.Prob.' = 0.05, 'Recovery Rate' = 0.05
                     , 'Long-Term Prop.' = c1, 'Short-Term Prop.' = f1, 'Peak Infected' = round(d1, 0), 'Concurrency' = '0%')

