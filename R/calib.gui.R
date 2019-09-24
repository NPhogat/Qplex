#'@name calib.gui
#'@aliases calib.gui
#'@title Run the graphical user interface for analysis of the data of lateral
#'flow assay (streptavidin assay)
#'@description Run the graphical user interface for streptavidin assay
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@examples
#'calib.gui()
#'@export
calib.gui <- function() {

  runApp(system.file("calib.gui", package = "Qplex"))

}
