
#' 1-Dimensional Diffusion
#'
#'
#' Diffuses a solute one dimension
#' @param initialC  initial concentration (mg/L)
#' @param dx length of each segment (m)
#' @param nx  number of discrete segments (m)
#' @param nt  number of discrete time intervals (s)
#' @param dt  seconds in each time interval (s)
#' @param area area of cross section of container
#' @param D  diffusivity (how easily the chemical diffuses
#' @examples
#' diff1(initialC=10, nx=10, dx=1, nt=8, dt=1, D=0.06, area=10)


diff1 = function(initialC, nx, dx, nt, dt,  D, area) {

	conc = matrix(nrow=nt, ncol=nx)
	qin = matrix(nrow=nt, ncol=nx)
	qout = matrix(nrow=nt, ncol=nx)
	conc[,] = 0.0
	qout[,] = 0.0
	qin[,] = 0.0
	conc[1,1] = initialC

	for ( t in 1:(nt-1)) {
		for (x in 1:nx) {
			qout[t,x] = ifelse((x < nx), dt*(0.5*D*area * (conc[t,x]-conc[t,x+1])),0)
			qin[t,x]  = ifelse((x > 1), dt*(0.5*D*area * (conc[t,x-1]-conc[t,x])),0)
			conc[t+1,x] = conc[t,x]+(qin[t,x]-qout[t,x])/(area*dx)
			}
		}


	return(list(conc=conc,qout=qout,qin=qin))
}

