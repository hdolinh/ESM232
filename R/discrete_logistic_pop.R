#'  Discrete Logistic population growth
#' @param p0 starting popultation
#' @param r growth rate
#' @param T number of timesteps
#' @param K carrying capacity
#' @return population after T

discrete_logistic_pop = function(P0,r,K, T=10) {
	pop=P0
	for (i in 1:T) {
	pop = pop + r*(1-pop/K)*pop
	}
	return(pop)
}
