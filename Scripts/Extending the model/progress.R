# Progress function

progress <- function(dat, at) {
  
  # Get status of nodes
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  
  # Get parameters - instead of a single recovery rate
  # Progression rates inf -> rec and rec -> sus
  rs.rate <- get_param(dat, "rs.rate")
  ir.rate <- get_param(dat, "ir.rate")
  
  # Code for rec -> sus progression
  nInf <- 0
  idsEligInf <- which(active == 1 & status == "r")
  nEligInf <- length(idsEligInf)
  
  if (nEligInf > 0) {
    vecInf <- which(rbinom(nEligInf, 1, rs.rate) == 1)
    if (length(vecInf) > 0) {
      idsInf <- idsEligInf[vecInf]
      nInf <- length(idsInf)
      status[idsInf] <- "s"
    }
  }
  
  # Code for inf -> rec progression
  nRec <- 0
  idsEligRec <- which(active == 1 & status == "i")
  nEligRec <- length(idsEligRec)
  
  if (nEligRec > 0) {
    vecRec <- which(rbinom(nEligRec, 1, ir.rate) == 1)
    if (length(vecRec) > 0) {
      idsRec <- idsEligRec[vecRec]
      nRec <- length(idsRec)
      status[idsRec] <- "r"
    }
  }
  
  # Update the infection status of each node
  dat <- set_attr(dat, "status", status)
  
  # Update the simulation statistics
  dat <- set_epi(dat, "rs.flow", at, nInf)
  dat <- set_epi(dat, "ir.flow", at, nRec)
  
  dat <- set_epi(dat, "s.num", at,
                 sum(active == 1 & status == "s"))
  dat <- set_epi(dat, "r.num", at,
                 sum(active == 1 & status == "r"))
  
  # Return the current simulation data
  return(dat)
}