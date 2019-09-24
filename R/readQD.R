#'@name readQD
#'@aliases readQD
#'@title To read the raw intensity data files
#'@description Function \code{readQD} reads the raw intensity files (.csv or .txt) and populate
#'them in an object of Class \code{Qdata}
#'@param file raw data file of intensity to be read in
#'@param type choose between .csv or .txt, where .csv is default
#'@param decp choose between "." or ",", where "." is default
#'@return object of Class \code{"Qdata"} and save the initial data in slot initialData
#'@details Allows the user to read in the raw intensity data and save them in S4 class
#'\code{Qdata}
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Qdata
#'@examples
#'##To read the .csv file
#'file_cal_1 <- system.file("exData", "QD_calibration_Qplex_new.csv", package = "ReadqPCR")
#'data <- readQD(file = file_cal_1, type = ".csv", decp = ",")
#'#To visualize all the results
#'data
#'## to visualise the initial data
#'slot(data,"initialData")
#'## to read the .txt file
#'file_cal_2 <- system.file("exData", "ImageJ_CRPi_IL6d.txt", package = "ReadqPCR")
#'data2 <- readQD(file = file_cal_2, type = ".txt", decp =".")
#'## to visualize all the data with all the slots
#'data2
#'## to visualise the initial data
#'slot(data2,"initialData")
#'@export
readQD <- function(file, type = ".csv", decp = "."){

  if (type == ".csv"){

    data <- read.csv2(file, header = TRUE, dec = decp)

  }

  else if(type == ".txt"){

    data <- read.table(file, header = TRUE, fill = TRUE,skip = 0, sep = "\t", quote = "\"",
                       dec = decp, comment.char = "")

  }

  else{

    warning('Select the type of file out of ".txt" and ".csv" !' )

  }

  res <- new("Qdata", initialData = data)

  res

}
