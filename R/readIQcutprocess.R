#'@name readIQcutprocess
#'@aliases readIQcutprocess
#'@title To cut the test line and control line peaks, along with
#'background processing.
#'@description Function \code{readIQcutprocess} represents the separated
#'peaks of control line and test line in the form of the plot of intensity
#'vs index, after background processing.
#'@param image Image to be read in.
#'@param fp Choose between TRUE or FALSE. TRUE is for the first peak
#'of the signal, which is the peak of control line, while FALSE is for
#'the second peak of the signal, which is the peak of the test line.
#'@param cut1 The first cut on the x-axis (index), from where the peak starts.
#'@param cut2 The second cut on the x-axis (index), where the peak ends.
#'@param icut The intensity for background propagation to remove error.
#'@return The separated and background processed plots of the peaks of
#'control line and test line.
#'@details Allows the user to represent the separated peaks of control line
#'and test line in the form of the plot of intensity vs index, after
#'background processing. Before applying function \code{readIQcutprocess},
#'the user should apply the function \code{readIQplexplot} to exactly
#'know the starting index values and intensity value to process the
#'signal properly by the function \code{readIQcutprocess}. It helps
#'to exactly know the intensity for background processing, index cuts
#'for control line and test line signal. These values of parameters
#'are used in the QplexIntensity to get the intensity values of the
#'control line and test line.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Signal
#'@examples
#'#Attach the file through system.file and then apply the function
#'# To see the signal (plot) of first peak (control line)
#'#separately
#'readIQcutprocess(image  = image_test, fp = TRUE,
#'cut1 =1140, cut2 = 1200, icut = 0.004)
#'# To see the signal (plot) of second peak (test line)
#'# separately
#'readIQcutprocess(image  = image_test, fp = FALSE,
#'cut1 =1290, cut2 = 1340, icut = 0.0035)
#'@export
readIQcutprocess <- function(image, fp = TRUE, cut1 = 1, cut2 =1, icut = 0.02)
{
  if ((missing(image))||(missing(fp))||(missing(cut1))||
      (missing(cut2))||(missing(icut))){
    stop("Please provide all the arguments appropriately!")
  }

  else if ((!(is.numeric(cut1))) || (!(is.numeric(cut2))) ||
           (!(is.numeric(icut)))){
    stop("Please provide numeric values for cut1, cut2 and icut")
  }

  else{
    imageQ <- readImage(files = image)
    data1 <- imageQ@.Data
    im_min <- min(data1)
    im_cd <- colMeans(data1-im_min)

    if (fp == TRUE){
      Intensity1 <- im_cd[cut1:cut2]
      Intensity <- Intensity1[Intensity1>icut]
      plot(Intensity,xlab = "Index", ylab = "Intensity [arbitrary unit]",
           main = "Intensity vs Index")
    }

    else if (fp == FALSE){
      Intensity2 <- im_cd[cut1:cut2]
      Intensity <- Intensity2[Intensity2>icut]
      plot(Intensity,xlab = "Index", ylab = "Intensity [arbitrary unit]",
           main = "Intensity vs Index")
    }
    else{
      warning("Provide right arguments for image, fp, cut1 and cut2!")
    }
  }
}
