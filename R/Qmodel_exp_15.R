#'@name Qmodel_exp_15
#'@aliases Qmodel_exp_15
#'@title To develop the statistical linear models and graphs with R-square
#'values and linear model equation.
#'@description Function \code{Qmodel_exp_15} works on the output of the function
#'\code{Repsd}. Function \code{Qmodel_exp_15} is designed for the sandwich assay.
#'It develops the statistical linear fit models of Normalized
#'Intensities vs Concentration, in a range with one value lesser than
#'the complete assay range. Here, it is in the range of 0-15 nM, after
#'removing the 20 nM concentration and respective intensity. It represents
#'the results in the form of plots, with embedded R-square value,
#'linear model equation and graph title in the plot.
#'@param Qdata object of S4 Class \code{Qdata}, which is an output of function
#'\code{Repsd}.
#'@param greenQD Choose between TRUE or FALSE. TRUE is for green QDs,
#'while FALSE is for red QDs. Default is TRUE.
#'@param graphtitle The title of the graph to be mentioned in the main of
#'the plot.
#'@param W a value to be provided for width. Default is 0.2
#'@param P a value to be provided for position_dodge. Default is 0.02
#'@return Plot with R-square value, linear model equation and graph title.
#'@details Function \code{Qmodel_exp_15} works on the output of the function
#'\code{Repsd}. Function \code{Qmodel_exp_15} is designed for the sandwich assay.
#'It develops the statistical linear fit models of Normalized
#'Intensities vs Concentration, in a range with one value lesser than
#'the complete assay range. Here, it is in the range of 0-15 nM, after
#'removing the 20 nM concentration and respective intensity. It represents
#'the results in the form of plots, with embedded R-square value,
#'linear model equation and graph title in the plot.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Qdata
#'@examples
#'# Example is similar like Qmodel_exp
#'#'#To combine the replicates of sandwich assay
#'#To combine the replicates of sandwich assay
#'file_il_d_1 <- system.file("exData", "ImageJ_CRPi_IL6d.csv", package = "Qplex")
#'data1 <- readQD(file = file_il_d_1)
#'file_il_i_1 <- system.file("exData", "ImageJ_CRPi_IL6i.csv", package = "Qplex")
#'data2 <- readQD(file = file_il_i_1)
#'data <- ReadQD_combine(data1, data2)
#'data_rep_sand <- Repsd(data, calib = FALSE)#'file_il_d_1 <- system.file("exData", "ImageJ_CRPi_IL6d.csv", package = "Qplex")
#'data1 <- readQD(file = file_il_d_1)
#'file_il_i_1 <- system.file("exData", "ImageJ_CRPi_IL6i.csv", package = "Qplex")
#'data2 <- readQD(file = file_il_i_1)
#'data <- ReadQD_combine(data1, data2)
#'data_rep_sand <- Repsd(data, calib = FALSE)
#'# 1) linear fitting model for green QD
#'plot_sand_g <- Qmodel_exp(data_rep_sand)
#'# to visualize the plot
#'plot_sand_g
#'# 2) linear fitting model for red QD
#'plot_sand_r <- Qmodel_exp(data_rep_sand,
#'graphtitle = "Red QD")
#'# to visualize the plot
#'plot_sand_r
#'@export
setMethod("Qmodel_exp_15", signature = "Qdata", definition =

            function (Qdata, greenQD = TRUE, graphtitle = "Green QD",
                      W = 0.02, P = 0.02){
              crep_g <- slot(Qdata,"cgData")
              nidata_g <- crep_g[1:(nrow(crep_g)-1),"Green_NI"]

              crep_r <- slot(Qdata,"crData")
              nidata_r <- crep_r[(2:nrow(crep_r)),"Red_NI"]

              if (greenQD == TRUE){
                Concn_g <- slot(Qdata,"Concentration_CRP")
                Concn.new_g <- Concn_g[1:(nrow(crep_g)-1),]
              }

              else if (greenQD == FALSE){
                Concn_r <- slot(Qdata,"Concentration_IL6")
                Concn.new_r <- Concn_r[2:nrow(crep_r),]
              }
              else {
                warning("please provide appropriate arguments!")
              }


              sd_g <- slot(Qdata,"sdgData")
              sd.ni_g <- sd_g[1:(nrow(sd_g)-1),"Green_NI"]

              sd_r <- slot(Qdata,"sdrData")
              sd.ni_r <- sd_r[2:(nrow(sd_r)),"Red_NI"]

              if (greenQD == TRUE){

                ni.new_g <- as.data.frame(cbind(Concn.new_g,nidata_g,sd.ni_g))
                names(ni.new_g) <- c("x","y","sd")

              }

              else if (greenQD == FALSE){
                ni.new_r <- as.data.frame(cbind(Concn.new_r,nidata_r,sd.ni_r))
                names(ni.new_r) <- c("x","y","sd")
              }
              else {
                warning("Please provide appropriate arguments!")
              }

              if (greenQD == TRUE){
                plot1 <- ggplot(data =ni.new_g, aes(x,y))+ ggtitle(graphtitle)+
                  geom_point(colour = "black", na.rm = TRUE)+
                  geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black")+
                  geom_errorbar(aes(ymin= y-sd, ymax=y+sd), width=.2,
                                position=position_dodge(0.05))+
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
