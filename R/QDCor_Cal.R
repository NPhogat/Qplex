#'@name QDCor_Cal
#'@aliases QDCor_Cal
#'@title To compute the correlation of the combined replicates in case
#'of streptavidin assay
#'@description Function \code{QDCor_Cal} works on the output of the function
#'\code{Repsd}. Function \code{QDCor_Cal} computes the correlation of the combined
#'replicates of green and red QDs with respect to the predicted values
#'through their respective linear fitted model in case of the streptavidin
#'assay. The output of the function \code{QDCor_Cal} are saved in the slot
#'slot corData of the object of S4 Class \code{Qdata}.
#'@param Qdata object of S4 Class \code{Qdata}, which is an output of function
#'\code{Repsd}.
#'@return An object of S4 Class \code{Qdata}, with a new slot of corData to
#'save the results of correlation of combined replicates of green
#'and red QDs.
#'@details Function \code{QDCor_Cal} works on the output of the function
#'\code{Repsd}. Function \code{QDCor_Cal} computes the correlation of the combined
#'replicates of green and red QDs with respect to the predicted values
#'through their respective linear fitted model in case of the streptavidin
#'assay. The output of the function \code{QDCor_Cal} are saved in the slot
#'slot corData of the object of S4 Class \code{Qdata}.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Qdata
#'@examples
#'#To combine the replicates of streptavidin assay
#'file_cal_1 <- system.file("exData", "QD_calibration_Qplex_new.csv", package = "ReadqPCR")
#'data_stp <- readQD(file = file_cal_1, type = ".csv", decp = ",")
#'data_rep_stp <- Repsd(data_stp)
#'#To compute the correlation
#'cor_stp <- QDCor_Cal(data_rep_stp)
#'# To visualize all the results
#'cor_stp
#'# To visualize only correlation results
#'slot(cor_stp,"corData")
#'@export
setMethod("QDCor_Cal", signature = "Qdata", definition =

            function (Qdata){
              x.g <- slot(Qdata,"cgData")
              x.r <- slot(Qdata,"crData")

              x.g1 <- x.g[,"Green_NI"]

              x.r1 <- x.r[,"Red_NI"]

              x.g1_new <- as.vector(as.numeric(x.g1))
              x.r1_new <- as.vector(as.numeric(x.r1))

              Concn <- slot(Qdata,"Concentration")
              x.Concn.new <- Concn[(1:nrow(x.g)),]
              conc.new <- as.vector(as.numeric(x.Concn.new))

              cal_cor <- function(x,y){
                y.model <- lm(y~x)
                y.pred <- predict(y.model)
                y.cor <- cor(y,y.pred)
                y.cor
              }

              cor.g1 <- cal_cor(conc.new,x.g1_new)
              cor.r1 <- cal_cor(conc.new,x.r1_new)

              correlation <- as.data.frame(rbind(cor.g1,cor.r1))
              rownames(correlation) <- c("Cor_G_NI","Cor_R_NI")
              colnames(correlation) <- ("Correlation")
              res_data <- new("Qdata", corData = correlation)
              res_data
            })
