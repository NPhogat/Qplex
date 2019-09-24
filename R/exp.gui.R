#'@name exp.gui
#'@aliases exp.gui
#'@title Run the graphical user interface for analysis of the data of lateral
#'flow assay (sandwich assay)
#'@description Run the graphical user interface for sandwich assay
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@examples
#'exp.gui()
#'@export
exp.gui <- function() {

  runApp(system.file("exp.gui", package = "Qplex"))

}
