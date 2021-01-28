generate_excel <- function(proj, 
                           patient,
                           visit_nr,
                           visit_type,
                           date){
  
  p <- stringr::str_pad(patient, width = 3,pad = "0", side = "left")
  p <-paste0(substring(proj,1,1),p)
  #v <- as.character(visit_nr) # levels 10, 11, 12, 20, 30, 40, 41, 42
  static <- paste0(p,".",ifelse(visit_type=="scheduled","V","U"),visit_nr,".")
  samples <- c(rep("15",10),  
               rep("16",10)  
  )
  o <- paste0(static,samples)
  oo<-data.frame(a=rep("food@",length(o)),
                 b = o,
                 c = rep(c("BAT+unstim",
                           "BAT+aIgE",
                           "BAT+All",
                           rep("InflCircs",3),
                           "Bcells",
                           "Tcells",
                           rep("PBMC",2)),2),
                 d = format(date,"%Y"))
  return(oo)
}