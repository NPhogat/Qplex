#'@name readIQplexplot
#'@aliases readIQplexplot
#'@title To display image or to display image data in the form of a signal
#'(plot of Intensity vs Index)
#'@description Function \code{readIQplexplot} either displays the image or image
#'data in the form of a signal, which is a plot of intensity vs concentration
#'@param Image Image to be read in
#'@return image signal in the form of plot of Intensity vs Index.
#'@details Allows the user either to read in the image and display
#'the image data in the form of a signal (a plot of intensity vs index).
#'The signal plot helps the user to understand that where the exact
#'signals of test line and cross line are present in the plot to
#'perform efficient background processing.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Image
#'@examples
#'# To read in the image and display in the form of signal plot
#'#attach the file of image through system.file and then read file.
#'#for example, let's say, the file attached is image1, then, to
#'# read image1 and show the plot code is as follows:
#'image_plot <- readIQplexplot(Image = image1)
#'# to display the image data in the form of plot (signal)
#'image_plot
#'@export
readIQplexplot <- function(Image, plot = "No"){
  imageQ <- readImage(files = Image)
  im_data <- imageQ@.Data
  im_min <- min(im_data)
  im_cd <- colMeans(im_data-im_min)
  w <- plot(im_cd, xlab = "Index", ylab = "Intensity [a.u.]",
            main = "Intensity vs Index")
  w
}
