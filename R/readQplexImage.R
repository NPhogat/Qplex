#'@name readQplexImage
#'@aliases readQplexImage
#'@title To display image
#'@description Function \code{readQplexImage} displays the image read in
#'@param Image Image to be read in
#'@return display image read in
#'@details Allows the user to display the image
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords Image
#'@examples
#'# To read in the image and display the image
#'#attach the file of image through system.file, if it is not in
#'#same directory and then read file.
#'#for example, let's say, the file attached is image1, then, to
#'# read image1 and display image1, the code is as follows:
#'image_dis <- readQplexImage(Image = image1)
#'# to display the image data in the form of plot (signal)
#'image_dis
#'@export
readQplexImage <- function(Image){
  imageQ <- readImage(files = Image)
  display(imageQ)
}