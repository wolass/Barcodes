# episLabeling
require(tidyverse)
epis_labs <- function(sample_ns,
         prefix,
         date,
         repetitions){
  lapply(sample_ns,function(n){
    rep(paste0(prefix,"\n",date,".",n
               ),repetitions) 
  })%>% 
    unlist()
  
}

epis_labs(51:(51+53),
          "BIO-AD1",
          "20200129",
          5)


custom_create_PDF(user=FALSE,
                  Labels = epis_labs(48:100,
                                     "BIO-AD",
                                     "2020",
                                     3),
                  name = 'LabelsEpis',
                  type = 'matrix',
                  ErrCorr = 'M',
                  Fsz = 4,
                  Across = T,
                  ERows = 0,
                  ECols = 0,
                  trunc = F,
                  numrow = 30,
                  numcol = 10,
                  page_width = 8.27,
                  page_height = 11.69,
                  width_margin = 0.35,
                  height_margin = 0.48,
                  label_width = NA,
                  label_height = NA,
                  x_space = 0,
                  y_space = 0.5)

### MAde for the 270 labels page!
custom_create_PDF(user=FALSE,
                  Labels = epis_labs(51:(51+53),
                                     "BIO-AD",
                                     5,1),
                  name = 'LabelsOut',
                  type = 'matrix',
                  ErrCorr = 'M',
                  Fsz = 4,
                  Across = T,
                  ERows = 0,
                  ECols = 0,
                  trunc = F,
                  numrow = 27,
                  numcol = 10,
                  page_width = 8.27,
                  page_height = 11.69,
                  width_margin = 0.15,
                  height_margin = 0.55,
                  label_width = NA,
                  label_height = NA,
                  x_space = 0,
                  y_space = 0.5)
