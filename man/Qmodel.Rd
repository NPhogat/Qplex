% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Qmodel.R
\docType{methods}
\name{Qmodel}
\alias{Qmodel}
\title{To develop the statistical linear models and graphs with R-square
values and linear model equation.}
\usage{
\S4method{Qmodel}{Qdata}(Qdata, greenQD = TRUE,
  graphtitle = "Green QD", W = 0.02, P = 0.02)
}
\arguments{
\item{Qdata}{object of S4 Class \code{Qdata}, which is an output of function
\code{Repsd}.}

\item{greenQD}{Choose between TRUE or FALSE. TRUE is for green QDs,
while FALSE is for red QDs. Default is TRUE.}

\item{graphtitle}{The title of the graph to be mentioned in the main of
the plot.}

\item{W}{a value to be provided for width. Default is 0.2}

\item{P}{a value to be provided for position_dodge. Default is 0.02}
}
\value{
Plot with R-square value, linear model equation and graph title.
}
\description{
Function \code{Qmodel} works on the output of the function
\code{Repsd}. Function \code{Qmodel} is designed for the streptavidin assay.
It develops the statistical linear fit models of Normalized
Intensities vs Concentration, where it represents the results in the
form of plots, with embedded R-square value, linear model equation and
graph title in the plot.
}
\details{
Allows the user to Function \code{Qmodel} works on the output of the function
\code{Repsd}. Function \code{Qmodel} is designed for the streptavidin assay.
Intensities vs Concentration, where it represents the results in the
form of plot, with embedded R-square value, linear model equation and graph title.
}
\examples{
file_cal_1 <- system.file("exData", "QD_calibration_Qplex_new.csv", package = "ReadqPCR")
data_stp <- readQD(file = file_cal_1, type = ".csv", decp = ",")
#to combine the replicates
data_rep_stp <- Repsd(data_stp)
# 1) linear model fitting for green QD
plot_stp_g <- Qmodel(data_rep_stp)
# to visualize the plot of green QD
plot_stp_g
# 2) linear model fitting for red QD
plot_stp_r <- Qmodel(data_rep_stp, greenQD = FALSE,
graphtitle = "Red QD")
# to visualize the plot of red QD
plot_stp_r
}
\author{
Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@stamats.de}
}
\keyword{Qdata}
