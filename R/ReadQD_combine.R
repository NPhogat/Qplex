#'@name ReadQD_combine
#'@aliases ReadQD_combine
#'@title To combine the raw intensity data files of sandwich lateral
#'flow assay
#'@description Function \code{ReadQD_combine} combines the raw intensity
#'files, read by function \code{readQD} and saved in the object of S4 class
#'\code{Qdata}. After combining it again save the combined data in the
#'form of an object of S4 Class \code{Qdata}.
#'@param Qdata object of S4 Class \code{Qdata}. The first file read by
#'function \code{readQD} ans saved in the form of object of Class \code{Qdata}
#'@param Qdata2 object of S4 Class \code{Qdata}. The second file read by
#'function \code{readQD} ans saved in the form of object of Class \code{Qdata}
#'@return object of Class \code{"Qdata"} and save the resulting data in slot initialData
#'@details Allows the user to combine the two raw intensity data files of sandwich assay,
#'read by function \code{readQD} and saved in the form of object of S4 class
#'\code{Qdata}. The output of function \code{ReadQD_combine} is also saved in the
#'form of object of S4 Class \code{Qdata}.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Qdata
#'@examples
#'##To read the first file (.csv)
#'file_il_d_1 <- system.file("exData", "ImageJ_CRPi_IL6d_Qplex.csv", package = "Qplex")
#'data1 <- readQD(file = file_il_d_1, decp = ".")
#'data1
#'## to read the second file (.csv)
#'file_il_i_1 <- system.file("exData", "ImageJ_CRPi_IL6i_Qplex.csv", package = "Qplex")
#'data2 <- readQD(file = file_il_i_1, decp = ".")
#'## to combine the data1 and data2
#'data <- ReadQD_combine(data1, data2)
#'##to visualise all the data after combining together
#'data
#'# to visualize the combined data
#'slot(data,"initialData")
#'@export
setMethod("ReadQD_combine", signature = "Qdata", definition =

            function (Qdata,Qdata2){
              x_d <- slot(Qdata,"initialData")
              x_i <- slot(Qdata2,"initialData")
              x_d_g_cl <- x_d[,"Green_cl"]
              x_d_g_tl <- x_d[,"Green_tl"]
              x_d_r_cl <- x_d[,"Red_cl"]
              x_d_r_tl <- x_d[,"Red_tl"]

              x_i_g_cl <- x_i[,"Green_cl"]
              x_i_g_tl <- x_i[,"Green_tl"]
              x_i_r_cl <- x_i[,"Red_cl"]
              x_i_r_tl <- x_i[,"Red_tl"]

              x_g_cl <- (x_d_g_cl + rev(x_i_g_cl))/2
              x_g_tl <- ((x_d_r_cl) + rev(x_i_g_tl))/2

              x_r_cl <- (x_d_r_cl + x_i_r_cl)/2
              x_r_tl <- (x_d_r_tl + x_i_r_tl)/2

              conc_CRP <- x_d[,"Conc_CRP_Green"]
              conc_IL6 <- x_d[,"Conc_2_IL6_Red"]

              rep <- x_d[,"Replicate"]

              data <- as.data.frame(cbind(rep, conc_CRP, conc_IL6,
                                          x_g_cl,x_g_tl,x_r_cl,
                                          x_r_tl))

              colnames(data) <- c("Replicate", "Conc_CRP_Green",
                                  "Conc_2_IL6_Red", "Green_cl",
                                  "Green_tl","Red_cl","Red_tl")
              res <- new("Qdata",initialData = data)
              res
            })
