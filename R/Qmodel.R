#'@name Qmodel
#'@aliases Qmodel
#'@title To develop the statistical linear models and graphs with R-square
#'values and linear model equation.
#'@description Function \code{Qmodel} works on the output of the function
#'\code{Repsd}. Function \code{Qmodel} is designed for the streptavidin assay.
#'It develops the statistical linear fit models of Normalized
#'Intensities vs Concentration, where it represents the results in the
#'form of plots, with embedded R-square value, linear model equation and
#'graph title in the plot.
#'@param Qdata object of S4 Class \code{Qdata}, which is an output of function
#'\code{Repsd}.
#'@param greenQD Choose between TRUE or FALSE. TRUE is for green QDs,
#'while FALSE is for red QDs. Default is TRUE.
#'@param graphtitle The title of the graph to be mentioned in the main of
#'the plot.
#'@param W a value to be provided for width. Default is 0.2
#'@param P a value to be provided for position_dodge. Default is 0.02
#'@return Plot with R-square value, linear model equation and graph title.
#'@details Allows the user to Function \code{Qmodel} works on the output of the function
#'\code{Repsd}. Function \code{Qmodel} is designed for the streptavidin assay.
#Function \code{Qmodel} develops the statistical linear fit models of Normalized
#'Intensities vs Concentration, where it represents the results in the
#'form of plot, with embedded R-square value, linear model equation and graph title.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Qdata
#'@examples
#'file_cal_1 <- system.file("exData", "QD_calibration_Qplex_new.csv", package = "ReadqPCR")
#'data_stp <- readQD(file = file_cal_1, type = ".csv", decp = ",")
#'#to combine the replicates
#'data_rep_stp <- Repsd(data_stp)
#'# 1) linear model fitting for green QD
#'plot_stp_g <- Qmodel(data_rep_stp)
#'# to visualize the plot of green QD
#'plot_stp_g
#'# 2) linear model fitting for red QD
#'plot_stp_r <- Qmodel(data_rep_stp, greenQD = FALSE,
#'graphtitle = "Red QD")
#'# to visualize the plot of red QD
#'plot_stp_r
#'@export
setMethod("Qmodel", signature = "Qdata", definition =

            function (Qdata, greenQD = TRUE, graphtitle = "Green QD",
                      W = 0.02, P = 0.02){
              crep_g <- slot(Qdata,"cgData")
              nidata_g <- crep_g[,"Green_NI"]

              crep_r <- slot(Qdata,"crData")
              nidata_r <- crep_r[,"Red_NI"]

              Concn <- slot(Qdata,"Concentration")
              Concn.new <- Concn[(1:nrow(crep_g)),]

              sd_g <- slot(Qdata,"sdgData")
              sd.ni_g <- sd_g[,"Green_NI"]

              sd_r <- slot(Qdata,"sdrData")
              sd.ni_r <- sd_r[,"Red_NI"]

              ni.new_g <- as.data.frame(cbind(Concn.new,nidata_g,sd.ni_g))
              names(ni.new_g) <- c("x","y","sd")

              ni.new_r <- as.data.frame(cbind(Concn.new,nidata_r,sd.ni_r))
              names(ni.new_r) <- c("x","y","sd")
              if (greenQD == TRUE){
                plot1 <- ggplot(data =ni.new_g, aes(x,y))+ ggtitle(graphtitle)+
                  geom_point(colour = "black", na.rm = TRUE)+
                  geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black")+
                  geom_errorbar(aes(ymin= y-sd, ymax=y+sd), width=W,
                                position=position_dodge(P))+
                  xlab("Concentration (nM)") +
                  ylab("N.I. (tl/cl) [a.u.]")+
                  stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label..,
                                                                sep = "~~~")),size = 10,
                               parse = TRUE)

              }

              else if (greenQD == FALSE){
                plot1 <- ggplot(data =ni.new_r, aes(x,y))+ ggtitle(graphtitle)+
                  geom_point(colour = "black", na.rm = TRUE)+
                  geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black")+
                  geom_errorbar(aes(ymin= y-sd, ymax=y+sd), width=W,
                                position=position_dodge(P))+
                  xlab("Concentration (nM)") +
                  ylab("N.I. (tl/cl) [a.u.]")+
                  stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label..,
                                                                sep = "~~~")),size = 10,
                               parse = TRUE)
              }

              else{
                warning("Please mention arguments appropriately!")
              }

              plot1

            })
