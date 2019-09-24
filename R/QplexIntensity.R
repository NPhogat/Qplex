#'#'@name QplexIntensity
#'@aliases QplexIntensity
#'@title To provide the intensity of the test line and control line, after
#'background processing.
#'@description Function \code{QplexIntensity} provides the intensity
#'of the control line and test line after background processing.
#'@param image Image to be read in.
#'@param fp Choose between TRUE or FALSE. TRUE is for intensity of the
#'first peak, which is the intensity of the control line, while FALSE is
#'for intensity of the second peak, which is the intensity of the test
#'line.
#'@param cut1 The first cut on the x-axis (index), from where the peak starts.
#'@param cut2 The second cut on the x-axis (index), where the peak ends.
#'@param icut The intensity for background propagation to remove noise.
#'@return The intensity for test line and control line after background
#'processing.
#'@details Allows the user to compute the intensity of the control
#'line and test line after background processing. Before applying function
#'\code{QplexIntensity}, the user should apply the functions \code{readIQplexplot}
#'to exactly know the starting index values and intensity value to process
#'the signal properly by the function \code{readIQcutprocess}. The values of
#'the parameters used while applying the function \code{readIQcutprocess} helps
#'to exactly know the intensity for background processing, index cuts for
#'control line and test line signal. These values of parameters of function
#'\code{readIQcutprocess} are used in function \code{QplexIntensity} to get the
#'intensity after proper background propagation.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Intensity
#'@examples
#'# To compute the intensity of first peak (control line)
#'QplexIntensity(image  = image_test, fp = TRUE,
#'cut1 =1140, cut2 = 1200, icut = 0.004)
#'To compute the intensity of second peak (test line)
#'QplexIntensity(image  = image_test, fp = FALSE,
#'cut1 =1290, cut2 = 1340, icut = 0.0035)
#'@export
QplexIntensity <- function(image, fp = TRUE, cut1 = 1, cut2 = 1, icut = 0.2){
  if ((missing(image))||(missing(fp))||(missing(cut1))||
      (missing(cut2))||(missing(icut))){
    stop("Please provide all the arguments appropriately!")
  }

  else if ((!(is.numeric(cut1))) || (!(is.numeric(cut2))) ||
           (!(is.numeric(icut)))){
    stop("Please provide numeric values for cut1 and cut2")
  }

  else{
    imageQ <- readImage(files = image)
    data1 <- imageQ@.Data
    im_min <- min(data1)
    im_cd <- colMeans(data1-im_min)
    if (fp == TRUE){
      Intensity1 <- im_cd[cut1:cut2]
      Intensity <- mean(Intensity1[Intensity1>icut])
      Intensity
    }

    else if (fp == FALSE){
      Intensity1 <- im_cd[cut1:cut2]
      Intensity <- mean(Intensity[Intensity1>icut])
      Intensity
    }
    else{
      warning("Provide right arguments for image, fp, cut1 and cut2!")
    }
  }
}
