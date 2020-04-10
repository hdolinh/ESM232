#' computes profit from power generation
#' @param  price ($/kj)
#' @param  energy (kj/yr)
#' @param discount rate (default 0.12)
#' @return data frame with estimate of profit
compute_profit_frompower = function(energy, price, discount=0.12) {

  # make sure values are reasonable
  if (length(energy) < 1)
    return(NA)
  
  # energy cannot be negative
  if (min(energy ) < 0)
    return(NA)
  
  years = seq(from=1, to=length(energy))
  yearprofit = data.frame(year=years, energy=energy)
  yearprofit$net =  yearprofit$energy*price

  # note how discount is passed through to this function
  yearprofit$netpre = mapply(compute_NPV, value=yearprofit$net, time=yearprofit$year, discount=discount )
  
  return(list(yearprofit=yearprofit, total = sum(yearprofit$netpre)))
}
