#'  More complex population growth
#' @param T  period of growth
#' @param P initial population
#' @param parms$r - base growth rate
#' @parms parms$K - carrying capacity
#' @return change in population 
#'
dexppop_play = function(time, P, parms) {
  
  
  dexpop = parms$r*P
  dexpop = ifelse(P > parms$K, 0, dexpop)
  return(list(dexpop))
}