#'@name Repsd
#'@aliases Repsd
#'@title To combine the replicates, after normalisation of the intensity and compute the
#'standard deviations within replicates after normalisation of the
#'intensities
#'@description Function \code{Repsd} works on the output of the function
#'\code{readQD} in case of streptavidin assay. In case of sandwich assay,
#'function \code{Repsd} works on the output of the function \code{ReadQD_combine}. Function
#'\code{Repsd} combines replicates after normlaization of the intensity
#'and computes the standard deviations within replicates after normalization
#'of the intensities. It saves the resulting data in the form of the object of
#'S4 Class \code{Qdata}.
#'@param Qdata object of S4 Class \code{Qdata}, which is an output of function
#'\code{readQD} in case of streptavidin assay and an output of the function
#'\code{ReadQD_combine} in case of sandwich assay.
#'@param calib choose between TRUE or FALSE. TRUE is for streptavidin assay
#'and FALSE is for sandwich assay. Default is TRUE.
#'@return object of Class \code{"Qdata"} and save the results in different slots, which are
#'described under section details.
#'@details Allows the user to combine the replicates after normalization of
#'the intensity and computes the standard deviation within replicates
#'after normalization of the intensities. The results are saved in different
#'slots of the Class \code{Qdata}. The common slots for the streptavidin assay
#'and sandwich assay for the output of the function \code{Repsd}:
#'\code{nsgData} : normalized intensities of green QDs
#'\code{nsrData} : normalized intensities of red QDs
#'\code{cgData} : combined replicates of normalized intensities of green QDs
#'\code{sdgData} : standard deviations of normalized intensities of replicates
#'of green QDs
#'\code{crData} : combined replicates of normalized intensities of red QDs
#'\code{sdrData} : standard deviations of normalized intensities of replicates
#'of red QDs
#'function Repsd separates the concentration slots also. The concentration
#'slots for streptavidin and sandwich assays are unique.
#'Slots which are unique for streptavidin assay:
#'\code{Concentration} : The concentration of green and red QDs, used in the assay
#'Slots which are unique for sandwich assay:
#'\code{Concentration_CRP} : Concentration used in case of green QDs
#'\code{Concentration_IL6} : COncentration used in case of red QDs
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Qdata
#'@examples
#'##1) For streptavidin assay
#'file_cal_1 <- system.file("exData", "QD_calibration_Qplex_new.csv", package = "ReadqPCR")
#'data_stp <- readQD(file = file_cal_1, type = ".csv", decp = ",")
#'#to combine the replicates
#'data_rep_stp <- Repsd(data_stp)
#'#to visualise all resulting data
#'data_rep_stp
#'#to visualize the normalized green QD data
#'slot(data_rep_stp,"nsgData")
#'#to visualize the normalized red QD data
#'slot(data_rep_stp,"nsrData")
#'#to visualize the combined replicates of green QD data
#'slot(data_rep_stp,"cgData")
#'#to visualize the standard deviation of the green QD data
#'slot(data_rep_stp,"sdgData")
#'#to visualize the combined replicates of red QD data
#'slot(data_rep_stp,"crData")
#'#to visualize the standard deviation of the red QD data
#'slot(data_rep_stp,"sdrData")
#'## 2) for sandwich assay
#'##To read the first file (.csv)
#'file_il_d_1 <- system.file("exData", "ImageJ_CRPi_IL6d_Qplex.csv", package = "Qplex")
#'data1 <- readQD(file = file_il_d_1, decp = ".")
#'## to read the second file (.csv)
#'file_il_i_1 <- system.file("exData", "ImageJ_CRPi_IL6i_Qplex.csv", package = "Qplex")
#'data2 <- readQD(file = file_il_i_1, decp = ".")
#'## to combine the data1 and data2
#'data <- ReadQD_combine(data1, data2)
#'#to combine the replicates
#'data_rep_sand <- Repsd(data, calib = FALSE)
#'#to visualize all the data
#'data_rep_sand
#'#to visualize the normalized green QD data
#'slot(data_rep_sand,"nsgData")
#'#to visualize the normalized red QD data
#'slot(data_rep_sand,"nsrData")
#'#to visualize the combined replicates of green QD data
#'slot(data_rep_sand,"cgData")
#'#to visualize the standard deviation of the green QD data
#'slot(data_rep_sand,"sdgData")
#'#to visualize the combined replicates of red QD data
#'slot(data_rep_sand,"crData")
#'#to visualize the standard deviation of the red QD data
#'slot(data_rep_sand,"sdrData")
#'@export
setMethod("Repsd", signature = "Qdata", definition =

            function (Qdata, calib = TRUE){

              x <- slot(Qdata,"initialData")
              if (calib == TRUE){

                conc1 <- (x[,"Concentration"])
                x_conc1 <- !(is.na(conc1))
                conc <- conc1[x_conc1]
                replicate_g <- x[,"Replicate_1"]
                replicate_r <- x[,"Replicate_2"]
              }

              else if (calib == FALSE){

                conc1 <- (x[,"Conc_CRP_Green"])
                x_conc1 <- !(is.na(conc1))
                conc_CRP <- conc1[x_conc1]

                conc2 <- (x[,"Conc_2_IL6_Red"])
                x_conc2 <- !(is.na(conc2))
                conc_IL6 <- conc2[x_conc2]

                replicate_g <- x[,"Replicate"]
                replicate_r <- x[,"Replicate"]

              }

              else{
                warning("Please provide argument calib as TRUE or FALSE!")
              }

              gcl <- as.numeric(x[,"Green_cl"])
              gtl <- as.numeric(x[,"Green_tl"])
              ni_g <- gtl/gcl

              ns_g_rep <- as.data.frame(cbind(ni_g))
              colnames(ns_g_rep) <- c("Green_NI")
              ns_g_rep1 <- as.matrix(ns_g_rep)

              row.names(ns_g_rep1) <- replicate_g
              grep.row <- replicate_g
              grep.fac <- factor(grep.row, levels = unique(grep.row))
              grep.unique <- unique(grep.row)

              repMg <- matrix(NA, nrow = length(grep.unique), ncol(ns_g_rep1))
              dimnames(repMg) <- list(grep.unique, colnames(ns_g_rep1))

              for (i in 1:ncol(ns_g_rep1))

              {

                grep.col <- (ns_g_rep1[,i])

                grep.col.split <- lapply(split(grep.col,grep.fac), mean, na.rm = TRUE)

                repMg[,i] <- unlist(grep.col.split)

              }

              ##grep complete

              #for sd_g
              ns_g <- as.data.frame(cbind(gcl,gtl,ni_g))
              colnames(ns_g) <- c("Green_cl","Green_tl","Green_NI")
              ns_g1 <- as.matrix(ns_g)
              row.names(ns_g1) <- replicate_g
              resMg <- matrix(NA,nrow=length(grep.unique),ncol(ns_g1))
              dimnames(resMg) <- list(grep.unique, colnames(ns_g1))

              for (i in 1:ncol(ns_g1))

              {

                gres.col <- (ns_g1[,i])

                gres.col.split <- lapply(split(gres.col,grep.fac), sd, na.rm = TRUE)

                resMg[,i] <- unlist(gres.col.split)

              }
              ##sd_g complete

              ## rrep

              rcl <- as.numeric(x[,"Red_cl"])
              rtl <- as.numeric(x[,"Red_tl"])
              ni_r <- rtl/rcl

              ns_r_rep <- as.data.frame(cbind(ni_r))
              colnames(ns_r_rep) <- c("Red_NI")
              ns_r_rep1 <- as.matrix(ns_r_rep)

              row.names(ns_r_rep1) <- replicate_r
              rrep.row <- replicate_r
              rrep.fac <- factor(rrep.row, levels = unique(rrep.row))
              rrep.unique <- unique(rrep.row)

              repMr <- matrix(NA, nrow = length(rrep.unique), ncol(ns_r_rep1))
              dimnames(repMr) <- list(grep.unique, colnames(ns_r_rep1))

              for (i in 1:ncol(ns_r_rep1))

              {

                rrep.col <- (ns_r_rep1[,i])

                rrep.col.split <- lapply(split(rrep.col,rrep.fac), mean, na.rm = TRUE)

                repMr[,i] <- unlist(rrep.col.split)

              }

              ##rrep complete

              #for sd_r
              ns_r <- as.data.frame(cbind(rcl,rtl,ni_r))
              colnames(ns_r) <- c("Red_cl","Red_tl","Red_NI")
              ns_r1 <- as.matrix(ns_r)
              row.names(ns_r1) <- replicate_r
              resMr <- matrix(NA,nrow=length(rrep.unique),ncol(ns_r1))
              dimnames(resMr) <- list(rrep.unique, colnames(ns_r1))

              for (i in 1:ncol(ns_r1))

              {

                rres.col <- (ns_g1[,i])

                rres.col.split <- lapply(split(rres.col,rrep.fac), sd, na.rm = TRUE)

                resMr[,i] <- unlist(rres.col.split)

              }

              newdata_g <- as.data.frame(repMg)
              newdata_gres <- as.data.frame(resMg)
              newdata_r <- as.data.frame(repMr)
              newdata_rres <- as.data.frame(resMr)
              if (calib == TRUE){

                concn <- as.data.frame(conc)
                colnames(concn) <- "Concentration"

                resdata <- new("Qdata",initialData = x, nsgData = ns_g_rep,
                               nsrData= ns_r_rep, cgData = newdata_g,
                               sdgData = newdata_gres, crData = newdata_r,
                               sdrData = newdata_rres, Concentration = concn)

              }

              else if (calib == FALSE)
              {

                conc1 <- as.data.frame(conc_CRP)
                colnames(conc1) <- "Conc_CRP_Green"
                conc2 <- as.data.frame(conc_IL6)
                colnames(conc2) <- "Conc2_IL6_Red"
                resdata <- new("Qdata",initialData = x, nsgData = ns_g_rep,
                               nsrData= ns_r_rep, cgData = newdata_g,
                               sdgData = newdata_gres, crData = newdata_r,
                               sdrData = newdata_rres,
                               Concentration_CRP = conc1,
                               Concentration_IL6 = conc2)

              }

              else {
                warning("Please provide argument calib as TRUE or FALSE!")
              }
              resdata

            })
