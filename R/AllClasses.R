#'@title S4 Class Qdata to contain the data
#'@slot initialData contains the initial data, in the form of an object
#'of Class \code{data.frame}
#'@slot nsgData contains the normalized intensity data of green quantum dots
#'(QDs) in the form of an object of Class \code{data.frame}
#'@slot nsrData contains the normalized intensity data of red quantum dots
#'(QDs) in the form of an object of Class \code{data.frame}
#'@slot cgData contains the combined replicates of normalized intensity of
#'green quantum dots in the form of an object of Class \code{data.frame}
#'@slot sdgData contains the standard deviation of normalized intensity of
#'green QDs within the replicates in the form of an object of Class
#'\code{data.frame}
#'@slot crData contains the combined replicates of normalized intensity of
#'red QDs in the form of an object of Class \code{data.frame}
#'@slot sdrData contains the standard deviation of normalized intensity of
#'red QDs within the replicates in the form of an object of Class \code{data.frame}
#'@slot Concentration contains the concentration data of calibration in the
#'form of an object of Class \code{data.frame}
#'@slot Concentration_CRP contains the concentration data of CRP in the form
#'of an object of Class \code{data.frame}
#'@slot Concentration_IL6 contains the concentration data of IL6 in the form
#'of an object of Class \code{data.frame}
#'@slot CIData contains the confidence interval data of combined replicates
#'in the form of an object of Class \code{data.frame}
#'@slot corData contains the result of correlation of combined normalized
#'intensities of calibration with respect to their predicted values by
#'linear model in the form of an object of Class \code{data.frame}
#'@slot corData_exp contains the result of correlation of combined normalized
#'intensities with respect to their predicted values by linear model in the
#'form of an object of Class \code{data.frame} 
#'@slot lobData contains the result of normalized intensities values of limit
#'of blank (LOB), limit of detection (LOD) and limit of quantification (LOQ) in
#'the form of an object of Class \code{data.frame}
#'@exportClass
setClass("Qdata", contains = "data.frame", representation(initialData = "data.frame",
                                                          nsgData = "data.frame",
                                                          nsrData= "data.frame",
                                                          cgData = "data.frame",
                                                          sdgData = "data.frame",
                                                          crData = "data.frame",
                                                          sdrData = "data.frame",
                                                          Concentration = "data.frame",
                                                          Concentration_CRP = "data.frame",
                                                          Concentration_IL6 = "data.frame",
                                                          CIData = "data.frame",
                                                          corData = "data.frame",
                                                          corData_exp = "data.frame",
                                                          lobData = "data.frame"

                                                          ))