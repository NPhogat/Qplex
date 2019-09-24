#'@name QplexConc
#'@aliases QplexConc
#'@title To compute concentration from intensity
#'@description Function \code{QplexConc} computes the concentation from
#'normalized intensity, based on the provided values of intensity, slope and
#'intercept.
#'@param y value of the normalized intensity, to be used for the
#'calculation of the concentration.
#'@param m value of the slope, according to linear model equation
#'@param c value of the intercept, according to linear model equation
#'@return The calculated value of the concentration.
#'@details Function \code{QplexConc} calculates the concentration from the
#'provided value of the normalized intensity, based on the other values of
#'slope and intercept, provided according to the linear model equation.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Concentration
#'@examples
#'#to compute the concentration from intensity. For example, intensity (y) is
#'# 2, slope (m) is 1 and intercept (c) is 0.5, then concentration
#'# will be
#' conc <- QplexConc(2, 1, 0.5)
#' # to visualize the calculated concentration
#' conc
#'@export
QplexConc <- function(y, m, c){

  concentration <- (y-c)/m

  concentration

}
