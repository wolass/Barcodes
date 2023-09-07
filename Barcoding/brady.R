generate_labels_per_visit_brady <- function(proj, 
                                            patient,
                                            visit_nr,
                                            visit_type,
                                            date){
  p <- stringr::str_pad(patient, width = 3,pad = "0", side = "left")
  p <-paste0(substring(proj,1,1),p)
  #v <- as.character(visit_nr) # levels 10, 11, 12, 20, 30, 40, 41, 42
  static <- paste0("FOOD@\n",p,".",ifelse(visit_type=="scheduled","V","U"),visit_nr,".")
  #### V1 ####
  if(visit_nr==1&(proj=="Adult"|proj=="Oesterreich")){
    samples <- c(rep("15",6),
                 rep("16",6))
  } else  if(visit_nr==1&proj=="Child"){
    samples <- c(rep("15",3))
  } else if(visit_nr==1&proj=="Infant"){
    samples <- c(rep("15",3))
  } else if(visit_nr==1&proj=="Healthy"){
    samples <- c(rep("15",6))
    ########V 2 / 3 ######
  } else if(visit_nr%in%c(2,3)&(proj=="Adult"|proj=="Oesterreich")){
    samples <- c(rep("bla",1))
  } else if(visit_nr%in%c(2,3)&proj=="Child"){
    samples <- c(rep("bla",1))
  } else if(visit_nr%in%c(2,3)&proj=="Infant"){
    samples <- c(rep("bla",1))
    ##### V4 ####
  } else if(visit_nr==4&(proj=="Adult"|proj=="Oesterreich")){
    samples <- c(rep("15",6),
                 rep("16",6))
  } else if(visit_nr==4&proj=="Child"){
    samples <- c(rep("15",3))
  } else if(visit_nr==4&proj=="Infant"){
    samples <- c(rep("15",3))
    ##### V5 #####
  } else if(visit_nr==5&proj=="Infant"){
    samples <- c(rep("bla",1))
    
  }
  o <- paste0(static,samples,"\n",date) 
  o <- gsub(x = o,  pattern=".bla.*",replacement = "- - -")
  return(data.frame(label = o))
}
