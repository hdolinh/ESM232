#'  More complex population growth
#' @param T  period of growth
#' @param P initial population
#' @param parms$maxtime time at which increasing growth rate stops
#' @param parms$r - base growth rate
#' @parms parm$maxP - maximum population - carrying capacity
#' @return change in population 
#'
dexppop_play = function(time, P, parms) {
  
  add = ifelse(time < parms$maxtime, 0.001*time, 0.001*parms$maxtime)
  growth_rate = parms$r+add
  dexpop = growth_rate*P
  dexpop = ifelse(P > parms$maxP, 0, dexpop)
  return(list(dexpop))
}