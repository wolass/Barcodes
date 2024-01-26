generate_labels_per_visit_LCRY <- function(proj, 
                                           patient,
                                           visit_nr,
                                           visit_type,
                                           date){
  p <- stringr::str_pad(patient, width = 3,pad = "0", side = "left")
  p <-paste0(substring(proj,1,1),p)
  #v <- as.character(visit_nr) # levels 10, 11, 12, 20, 30, 40, 41, 42
  static <- paste0("FOOD@\n",p,".",ifelse(visit_type=="scheduled","V","U"),visit_nr,".")
  ##### V1 ####
  if(visit_type == "scheduled"){
    if(visit_nr==1&(proj=="Adult"|proj=="Oesterreich")){
    samples <- c(rep("02",40),
                 rep("04",2),
                 rep("05",2),
                 rep("06",2),
                 rep("07",2),
                 rep("08",10),
                 rep("10",2),
                 rep("11",2),
                 rep("12",2),
                 rep("17",5),
                 rep("18",5),
                 rep("21",2))
  } else if(visit_nr==1&proj=="Child"){
    samples <- c(rep("02",20),
                 rep("04",2),
                 rep("05",2),
                 rep("06",2),
                 rep("07",2),
                 rep("08",5),
                 rep("10",2),
                 rep("11",2),
                 rep("12",2),
                 rep("17",4),
                 rep("21",2))
  } else if(visit_nr==1&proj=="Infant"){
    samples <- c(rep("02",15),
                 rep("04",2),
                 rep("05",2),
                 rep("06",2),
                 rep("07",2),
                 rep("08",6),
                 rep("09",4),
                 rep("10",2),
                 rep("11",2),
                 rep("12",2),
                 rep("17",3),
                 rep("20",15),
                 rep("21",2),
                 rep("22",2))
  } else if(visit_nr==1&proj=="Healthy"){
    samples <- c(rep("01",4),
                 rep("02",60),
                 rep("03",9),
                 rep("06",2),
                 rep("32",2),
                 rep("07",2),
                 rep("21",2),
                 rep("08",10),
                 rep("10",2),
                 rep("11",2),
                 rep("17",5))
    ##### V 2 / 3 ######
  }else if(visit_nr%in%c(2,3)&(proj=="Adult"|proj=="Oesterreich")){
    samples <- c(rep("02",30),
                 rep("04",2),
                 rep("06",2),
                 rep("07",2),
                 rep("08",10),
                 rep("21",2))
  } else if(visit_nr%in%c(2,3)&proj=="Child"){
    samples <- c(rep("04",2),
                 rep("06",2),
                 rep("07",2),
                 rep("08",5),
                 rep("21",2))
  } else if(visit_nr%in%c(2,3,4)&proj=="Infant"){
    samples <- c(rep("04",2),
                 rep("06",2),
                 rep("08",6),
                 rep("09",4),
                 rep("10",2),
                 rep("11",2),
                 rep("12",2),
                 rep("22",2))
    ###### V4 ####
  } else if(visit_nr==4&(proj=="Adult"|proj=="Oesterreich")){
    samples <- c(rep("02",40),
                 rep("04",2),
                 rep("06",2),
                 rep("08",10),
                 rep("10",2),
                 rep("11",2),
                 rep("12",2),
                 rep("17",5),
                 rep("18",5))
  } else if(visit_nr==4&proj=="Child"){
    samples <- c(rep("02",20),
                 rep("04",2),
                 rep("06",2),
                 rep("08",6),
                 rep("10",2),
                 rep("11",2),
                 rep("12",2),
                 rep("17",5))
  } else if(visit_nr==5&proj=="Infant"){
    samples <- c(rep("02",20),
                 rep("04",2),
                 rep("06",2),
                 rep("08",6),
                 rep("09",4),
                 rep("10",2),
                 rep("11",2),
                 rep("12",2),
                 rep("17",3),
                 rep("20",15),
                 rep("22",2))
    ###### V5 #####
  } else if(visit_nr==6&proj=="Infant"){
    samples <- c(rep("02",20))
  } else if(proj=="Healthy Children"){
    samples <- c(rep("06",2),
                 rep("07",2),
                 rep("10",2),
                 rep("11",2),
                 rep("21",2),
                 rep("08",5),
                 rep("09",1) # here added Saliva for Vali FOOD
    )
  }
    
  } else {
    if((proj=="Adult"|proj=="Oesterreich")){
      samples <- c(rep("02",40),
                   rep("04",2),
                   rep("05",2),
                   rep("06",2),
                   rep("07",2),
                   rep("08",6),
                   rep("10",2),
                   rep("11",2),
                   rep("12",2),
                   rep("13",40),
                   rep("17",5),
                   rep("18",5),
                   rep("21",2))
    } else  if(proj=="Child"){
      samples <- c(rep("02",20),
                   rep("04",2),
                   rep("05",2),
                   rep("06",2),
                   rep("07",2),
                   rep("08",2),
                   rep("10",2),
                   rep("11",2),
                   rep("12",2),
                   rep("17",4),
                   rep("21",2))
    } else if(proj=="Infant"){
      samples <- c(rep("02",15),
                   rep("04",2),
                   rep("05",2),
                   rep("06",2),
                   rep("07",2),
                   rep("08",6),
                   rep("09",4),
                   rep("10",2),
                   rep("11",2),
                   rep("12",2),
                   rep("17",3),
                   rep("20",15),
                   rep("22",1))
    }
  }
  o <- paste0(static,samples,"\n",date) 
  o <- gsub(x = o,  pattern=".bla.*",replacement = "- - -")
  return(data.frame(label = o))
}