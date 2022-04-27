generate_labels_per_visit_LCRY_saliva <- function(proj, 
                                            patient,
                                            visit_nr,
                                            visit_type,
                                            date){
  p <- stringr::str_pad(patient, width = 3,pad = "0", side = "left")
  p <-paste0(substring(proj,1,1),p)
  #v <- as.character(visit_nr) # levels 10, 11, 12, 20, 30, 40, 41, 42
  static <- paste0("FOOD@\n",p,".",ifelse(visit_type=="scheduled","V","U"),visit_nr,".")
  ##### V1 ####
  samples <- c(rep("08",10),
               rep("33",9),
               rep("34",9),
               rep("35",9) 
  )
  o <- paste0(static,samples,"\n",date) 
  o <- gsub(x = o,  pattern=".bla.*",replacement = "- - -")
  return(data.frame(label = o))
}