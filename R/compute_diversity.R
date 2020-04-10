#' Simpson's Species Diversity Index 
#' 
#' Compute a species diversity index
#' @param species list of species (names, or code) 
#' @return value of Species Diversity Index 
#' @examples
#' compute_simpson_index(c("butterfly","butterfly","mosquito","butterfly","ladybug","ladybug")))
#' @references
#' http://www.tiem.utk.edu/~gross/bioed/bealsmodules/simpsonDI.html

compute_diversity = function(species) {

species = as.factor(species)
ssp = summary(species)
tt  = sum(ssp) 

diversity = sum((ssp/tt)**2)

mostfreq = names(which.max(ssp))

return(list(simpson=diversity, mostfreq=mostfreq))
}


