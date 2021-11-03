generate_labels_per_visit_herma2 <- function(proj, 
                                            patient,
                                            visit_nr,
                                            visit_type,
                                            date){
  p <- stringr::str_pad(patient, width = 3,pad = "0", side = "left")
  p <-paste0(substring(proj,1,1),p)
  #v <- as.character(visit_nr) # levels 10, 11, 12, 20, 30, 40, 41, 42
  static <- paste0("FOOD@\n",p,".",ifelse(visit_type=="scheduled","V","U"),visit_nr,".")
  #### V1 ####
  if(visit_type == "scheduled"){
    if(visit_nr==1&proj=="Adult"){
    samples <- c(rep("25",6),
                 rep("26",6),
                 rep("27",6))
  } else  if(visit_nr==1&proj=="Child"){
    samples <- c(rep("02",4),
                 rep("03",4),
                 rep("04",1),
                 rep("05",1),
                 rep("06",1),
                 rep("07",1),
                 rep("08",5),
                 rep("10",1),
                 rep("11",1),
                 rep("12",1),
                 rep("13",2),
                 rep("21",1))
  } else if(visit_nr==1&proj=="Infant"){
    samples <- c(rep("02",5),
                 rep("03",3),
                 rep("04",2),
                 rep("05",2),
                 rep("06",2),
                 rep("07",2),
                 rep("08",3),
                 rep("09",3),
                 rep("10",2),
                 rep("11",2),
                 rep("12",2),
                 rep("20",5),
                 rep("21",2),
                 rep("22",1))
    ########V 2 / 3 ######
  } else if(visit_nr%in%c(2,3)&proj=="Adult"){
    samples <- c(rep("bla",1))
  } else if(visit_nr%in%c(2,3)&proj=="Child"){
    samples <- c(rep("04",1),
                 rep("06",2),
                 rep("08",3))
  } else if(visit_nr%in%c(2,3,4)&proj=="Infant"){
    samples <- c(rep("04",2),
                 rep("06",2),
                 rep("08",3),
                 rep("09",3),
                 rep("10",2),
                 rep("11",2),
                 rep("12",2),
                 rep("22",1))
    ##### V4 ####
  } else if(visit_nr==4&proj=="Adult"){
    samples <- c(rep("25",6),
                 rep("26",6),
                 rep("27",6))
  } else if(visit_nr==4&proj=="Child"){
    samples <- c(rep("02",3),
                 rep("03",4),
                 rep("04",1),
                 rep("05",1),
                 rep("06",1),
                 rep("08",3),
                 rep("10",1),
                 rep("11",1),
                 rep("12",1),
                 rep("13",2)
    )
  } else if(visit_nr==5&proj=="Infant"){
    samples <- c(rep("01",2),
                 rep("02",5),
                 rep("03",3),
                 rep("04",2),
                 rep("06",2),
                 rep("08",3),
                 rep("09",3),
                 rep("10",2),
                 rep("11",2),
                 rep("12",2),
                 rep("20",5),
                 rep("22",1))
    ##### V5 #####
  } else if(visit_nr==6&proj=="Infant"){
    samples <- c(rep("02",5),
                 rep("03",5))
    
  }
  } else {
  ##### Unscheduled ######  
    if(proj=="Adult"){
      samples <- c(rep("03",5),
                   rep("14", 5))
    } else  if(proj=="Child"){
      samples <- c(rep("02",3),
                   rep("03",4),
                   rep("04",1),
                   rep("05",1),
                   rep("06",1),
                   rep("07",1),
                   rep("08",3),
                   rep("10",1),
                   rep("11",1),
                   rep("12",1),
                   rep("21",1))
    } else if(proj=="Infant"){
      samples <- c(rep("02",5),
                   rep("03",3),
                   rep("04",2),
                   rep("05",2),
                   rep("06",2),
                   rep("07",2),
                   rep("08",3),
                   rep("10",2),
                   rep("11",2),
                   rep("12",2),
                   rep("21",2),
                   rep("22",1))
    } 
  }
  o <- paste0(static,samples,"\n",date) 
  o <- gsub(x = o,  pattern=".bla.*",replacement = "- - -")
  return(data.frame(label = o))
}
