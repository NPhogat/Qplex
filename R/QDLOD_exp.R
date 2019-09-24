#'@name QDLOD_exp
#'@aliases QDLOD_exp
#'@title To compute the LOB, LOD and LOQ of sandwich assay
#'@description Function \code{QDLOD_exp} works on the output of the function
#'\code{Repsd}. Function \code{QDLOD_exp} computes the limit of blank (LOB),
#'limit of detection (LOD) and limit of quantification (LOQ) intensities
#'of the sandwich assay. These intensities can be used to compute
#'the LOB, LOD and LOQ concentrations, based on the linear model equation.
#'@param Qdata object of S4 Class \code{Qdata}, which is an output of function
#'\code{Repsd}.
#'@param LOB Choose between TRUE and FALSE. FALSE is for computing the
#'LOD and LOQ intensities by first method, without involving the LOB. TRUE is for computing all
#'three LOB, LOD and LOQ intensities by second method, with involvement
#'of LOB in calculation of LOD too. Second method is more accurate method.
#'@return An object of S4 Class \code{Qdata}, with a new slot of lobData to
#'save the results of LOB, LOD and LOQ of combined replicates of green
#'and red QDs.
#'@details Function \code{QDLOD_exp} works on the output of the function
#'\code{Repsd}. Function \code{QDLOD_exp} computes the limit of blank (LOB),
#'limit of detection (LOD) and limit of quantification (LOQ) intensities
#'of the sandwich assay. The results of LOB, LOD and LOQ intensities are
#'saved in the new slot lobData of the object of S4 Class \code{Qdata}. These intensities can be used to compute
#'the LOB, LOD and LOQ concentrations, based on the linear model equation.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Qdata
#'@examples
#'#To combine the replicates of sandwich assay
#'##To read the first file (.csv)
#'file_il_d_1 <- system.file("exData", "ImageJ_CRPi_IL6d_Qplex.csv", package = "Qplex")
#'data1 <- readQD(file = file_il_d_1, decp = ".")
#'## to read the second file (.csv)
#'file_il_i_1 <- system.file("exData", "ImageJ_CRPi_IL6i_Qplex.csv", package = "Qplex")
#'data2 <- readQD(file = file_il_i_1, decp = ".")
#'data <- ReadQD_combine(data1, data2)
#'data_rep_sand <- Repsd(data, calib = FALSE)
#'# 1) To compute the LOB, LOD and LOQ intensities by first
#'#method
#'lod_exp1 <- QDLOD_exp(data_rep_sand)
#'#To visualize all the results
#'lod_exp1
#'# To visualize the only the LOB, LOD and LOQ intensities
#'slot(lod_exp1,"lobData")
#'# 2) To compute the LOB, LOD and LOQ intensities by
#'#second method
#'lod_exp2 <- QDLOD_exp(data_rep_sand,LOB = TRUE)
#'#To visualize all the results
#'lod_exp2
#'# To visualize only the LOB, LOD and LOQ intensities
#'slot(lod_exp2,"lobData")
#'@export
setMethod("QDLOD_exp", signature = "Qdata", definition =

            function (Qdata, LOB = FALSE){
              sd_g <- slot(Qdata,"sdgData")
              mean_g <- slot(Qdata,"cgData")
              sd_r <- slot(Qdata,"sdrData")
              mean_r <- slot(Qdata,"crData")

              x_ni_g <- sd_g[1,"Green_NI"]
              x_ni_g2 <- sd_g[2,"Green_NI"]
              m_ni_g <- mean_g[1,"Green_NI"]
              x <- nrow(sd_r)
              x_ni_r <- sd_r[x,"Red_NI"]
              x_ni_r2 <- sd_r[(x-1),"Red_NI"]
              m_ni_r <- mean_r[x,"Red_NI"]

              if (LOB == FALSE){

                lod_ni_g <- m_ni_g + (3*x_ni_g)
                loq_ni_g <- m_ni_g + (10*x_ni_g)

                lod_ni_r <- m_ni_r + (3*x_ni_r)
                loq_ni_r <- m_ni_r + (10*x_ni_r)

                res_data <- as.data.frame(cbind(lod_ni_g,loq_ni_g,
                                                lod_ni_r, loq_ni_r))
              }

              else if (LOB == TRUE){

                lob_ni_g <- m_ni_g+(1.645*(x_ni_g))
                lod_ni_g <- lob_ni_g + (1.645*(x_ni_g2))
                loq_ni_g <- m_ni_g + (10*x_ni_g)

                lob_ni_r <- m_ni_r+(1.645*(x_ni_r))
                lod_ni_r <- lob_ni_r + (1.645*(x_ni_r2))
                loq_ni_r <- m_ni_r + (10*x_ni_r)

                res_data <- as.data.frame(cbind(lob_ni_g,lod_ni_g,loq_ni_g,
                                                lob_ni_r,lod_ni_r,loq_ni_r))
              }
              res <- new("Qdata",lobData = res_data)
              res
            })


