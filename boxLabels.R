# Boxes
library(baRcodeR)
require(tidyverse)
# -20 Freezer in the lab  == 1 

box_labs <- function(freezer,
                     n_levels,
                     n_boxes, 
                     repetitions, 
                     optional_text = ""){
  freezer <- stringr::str_pad(freezer, width = 2,pad = "0", side = "left")
  n_levels <- stringr::str_pad(n_levels, width = 2,pad = "0", side = "left")
  n_boxes <- stringr::str_pad(n_boxes, width = 2,pad = "0", side = "left")
  
  lapply(n_levels,function(level){
    paste0(optional_text,"F",freezer,".","R",level,".B",sapply(n_boxes,rep,repetitions))
  }) %>% unlist()
}

labs <- box_labs(freezer = 6,
                 n_levels = 1:30,
                 n_boxes = 1:20, 
                 repetitions = 2, 
                 optional_text = "")


custom_create_PDF(user=FALSE,
                  Labels = labs,
                  name = 'boxLabels',
                  type = 'matrix',
                  ErrCorr = 'M',
                  Fsz = 10,
                  Across = T,
                  ERows = 0,
                  ECols = 0,
                  trunc = F,
                  numrow = 7,
                  numcol = 4,
                  page_width = 8.27,
                  page_height = 11.69,
                  width_margin = 0.1,
                  height_margin = 0.0,
                  label_width = NA,
                  label_height = NA,
                  x_space = 0,
                  y_space = 0.5)


