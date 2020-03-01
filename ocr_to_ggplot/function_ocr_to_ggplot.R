library(ggplot2) #version 3.1.0
library(data.table) #version 1.12.6
library(magick) # version 2.2
require(tesseract)

#' Return ggplot from ocr with overlay of boundingboxes indicating confidence
#'
#' @param filename string with magick-readable image
#'
#' @return ggplot object
#' @export
#'
#' @example ocr_and_bbox_ggplot("Download.png")
#' 
ocr_and_bbox_ggplot <- function(filename) {
  
  image <- image_read(filename)
  ocr_data <- image_ocr_data(image, language = "eng")
  
  setDT(ocr_data)
  ocr_data[, c("x_min", "y_min", "x_max", "y_max") := tstrsplit(bbox, ",", fixed=TRUE)]
  ocr_data[, x_min := as.numeric(x_min)]
  ocr_data[, x_max := as.numeric(x_max)]
  ocr_data[, y_min := as.numeric(y_min)]
  ocr_data[, y_max := as.numeric(y_max)]
  
  erg <- list()
  for (rownumber in 1:nrow(ocr_data)) {
    tmp <- ocr_data[rownumber, ]
    ocr_data[rownumber, ecken := list(list(c(tmp$x_min, tmp$y_min), 
                                           c(tmp$x_min, tmp$y_max), 
                                           c(tmp$x_max, tmp$y_max), 
                                           c(tmp$x_max, tmp$y_min)))]
    erg[[rownumber]] <- cbind(data.table(x = lapply(unlist(ocr_data[rownumber,]$ecken, recursive = FALSE), `[[`, 1) %>% unlist()),
                              data.table(y = lapply(unlist(ocr_data[rownumber,]$ecken, recursive = FALSE), `[[`, 2) %>% unlist()),
                              tmp[, .(confidence)])
    erg[[rownumber]][ , id := rownumber]
  }
  erg <- rbindlist(erg)
  
  myplot <- image_ggplot(image = image) #using geom_polygon because geom_rect was not possible because of error
  p <- myplot + 
    geom_polygon(
      data = erg, 
      aes(x = x, y = y, group = id, fill = confidence), 
      color="red", size = .1, alpha = .5
    )
  
  return(p)
}
