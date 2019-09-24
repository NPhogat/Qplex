#'@name QDConf
#'@aliases QDConf
#'@title To compute the 95% confidence intervals of normalized intensities of
#'combined replicates of green and red QDs
#'@description Function \code{QDConf} works on the output of the function
#'\code{Repsd}. Function \code{QDConf} computes the 95% confidence
#'interval of the combined replicates of green and red QDs.
#'@param Qdata object of S4 Class \code{Qdata}, which is an output of function
#'\code{Repsd}.
#'@return An object of S4 Class \code{Qdata}, with a new slot of CIData to
#'save the results of 95% confidence intervals of combined replicates
#'of green and red QDs.
#'@details Allows the user to compute the 95% confidence interval of the combined
#'replicates of green and red QDs. It works on the object of S4 Class \code{Qdata},
#'produced as an output of the function \code{Repsd}. The final results
#'of the 95% confidence interval are saved in the form of the object of S4 Class
#'\code{Qdata}.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Qdata
#'@examples
#'#To combine the replicates of streptavidin assay
#'file_cal_1 <- system.file("exData", "QD_calibration_Qplex_new.csv", package = "ReadqPCR")
#'data_stp <- readQD(file = file_cal_1, type = ".csv", decp = ",")
#'data_rep_stp <- Repsd(data_stp)
#'#To combine the replicates of sandwich assay
#'##To read the first file (.csv)
#'file_il_d_1 <- system.file("exData", "ImageJ_CRPi_IL6d_Qplex.csv", package = "Qplex")
#'data1 <- readQD(file = file_il_d_1, decp = ".")
#'## to read the second file (.csv)
#'file_il_i_1 <- system.file("exData", "ImageJ_CRPi_IL6i_Qplex.csv", package = "Qplex")
#'data2 <- readQD(file = file_il_i_1, decp = ".")
#'data <- ReadQD_combine(data1, data2)
#'data_rep_sand <- Repsd(data, calib = FALSE)
#'# to compute the 95% confidence interval
#'#1)streptavidin assay
#'ci_stp <- QDConf(data_rep_stp)
#'#to visualize all results
#'ci_stp
#'#to visualize only the 95% confidence interval
#'slot(ci_stp,"CIData")
#'#2)sandwich assay
#'ci_sand <- QDConf(data_rep_sand)
#'# to visualize all the results
#'ci_sand
#'# to visualize only the 95% confidence interval
#'slot(ci_sand,"CIData")
#'@export
setGeneric("QDConf", function (Qdata,...) standardGeneric("QDConf"))
setMethod("QDConf", signature = "Qdata", definition =

            function (Qdata){
              x.g <- slot(Qdata,"cgData")
              x.r <- slot(Qdata,"crData")

              x.g1 <- x.g[,"Green_NI"]

              x.r1 <- x.r[,"Red_NI"]

              calcconf <- function(x){
                x <- x[!is.na(x)]
                x.se <- sd(x)/sqrt(length(x))
                x2 <- as.numeric(x)
                x.mean <- mean(x2)
                ci.min <- (x.mean - 2*x.se)
                ci.max <- (x.mean + 2*x.se)
                ci <- as.data.frame(cbind(ci.min, ci.max))
                colnames(ci) <- c("Min.Value", "Max.Value")
                ci
              }

              confg1 <- calcconf(x.g1)
              confr1 <- calcconf(x.r1)
              confall <- as.data.frame(rbind(confg1,confr1))
              rownames(confall) <- c("Conf_G_NI","Conf_R_NI")

              res_data <- new("Qdata",CIData = confall)
              res_data
            })
