#'@name QDCor_exp
#'@aliases QDCor_exp
#'@title To compute the correlation of the combined replicates in case
#'of sandwich assay
#'@description Function \code{QDCor_exp} works on the output of the function
#'\code{Repsd}. Function \code{QDCor_exp} computes the correlation of the combined
#'replicates of green and red QDs with respect to the predicted values
#'through their respective linear fitted model in case of the sandwich
#'assay. The output of the function \code{QDCor_exp} are saved in the slot
#'slot corData_exp of the object of S4 Class \code{Qdata}.
#'@param Qdata object of S4 Class \code{Qdata}, which is an output of function
#'\code{Repsd}.
#'@return An object of S4 Class \code{Qdata}, with a new slot of corData_exp to
#'save the results of correlation of combined replicates of green
#'and red QDs.
#'@details Function \code{QDCor_exp} works on the output of the function
#'\code{Repsd}. Function \code{QDCor_exp} computes the correlation of the combined
#'replicates of green and red QDs with respect to the predicted values
#'through their respective linear fitted model in case of the sandwich
#'assay. The output of the function \code{QDCor_exp} are saved in the slot
#'slot corData_exp of the object of S4 Class \code{Qdata}.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Qdata
#'@examples
#'##To read the first file (.csv)
#'file_il_d_1 <- system.file("exData", "ImageJ_CRPi_IL6d_Qplex.csv", package = "Qplex")
#'data1 <- readQD(file = file_il_d_1, decp = ".")
#'## to read the second file (.csv)
#'file_il_i_1 <- system.file("exData", "ImageJ_CRPi_IL6i_Qplex.csv", package = "Qplex")
#'data2 <- readQD(file = file_il_i_1, decp = ".")
#'data <- ReadQD_combine(data1, data2)
#'data_rep_sand <- Repsd(data, calib = FALSE)
#'# To compute the correlation
#'cor_sand <- QDCor_exp(data_rep_sand)
#'# to visualize all the results
#'cor_sand
#'# to visualize only the correlation results
#'slot(cor_sand,"corData_exp")
#'@export
setMethod("QDCor_exp", signature = "Qdata", definition =

            function (Qdata){
              x.g <- slot(Qdata,"cgData")
              x.r <- slot(Qdata,"crData")

              x.g1 <- x.g[,"Green_NI"]

              x.r1 <- x.r[,"Red_NI"]

              x.g1_new <- as.vector(as.numeric(x.g1))
              x.r1_new <- as.vector(as.numeric(x.r1))

              Conc_g <- slot(Qdata,"Concentration_CRP")
              x.Concn.new_g <- Conc_g[(1:nrow(x.g)),]
              conc.new_g <- as.vector(as.numeric(x.Concn.new_g))

              Conc_r <- slot(Qdata,"Concentration_IL6")
              x.Concn.new_r <- Conc_g[(1:nrow(x.g)),]
              conc.new_r <- as.vector(as.numeric(x.Concn.new_r))

              cal_cor <- function(x,y){
                y.model <- lm(y~x)
                y.pred <- predict(y.model)
                y.cor <- cor(y,y.pred)
                y.cor
              }

              cor.g1 <- cal_cor(conc.new_g,x.g1_new)
              cor.r1 <- cal_cor(conc.new_r,x.r1_new)

              correlation <- as.data.frame(rbind(cor.g1,cor.r1))
              rownames(correlation) <- c("Cor_G_NI","Cor_R_NI")
              colnames(correlation) <- ("Correlation")
              res_data <- new("Qdata", corData_exp = correlation)
              res_data
            })
