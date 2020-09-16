#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("shinydashboard")


#### Packages #### 
## app.R ##
library(shinydashboard)
#install.packages("baRcodeR")
library(baRcodeR)
pacman::p_load(writexl)


#### Functions ####
source(file = "custom_create_PDF_sub.R")
source(file = "custom_create_PDF_sub2.R")

source(file = "custom_qrcode_make.R")
source(file = "custom_qrcode_make2.R")
generate_labels_per_visit <- function(proj, 
                                      patient,
                                      visit_nr,
                                      visit_type,
                                      date){
  p <- stringr::str_pad(patient, width = 3,pad = "0", side = "left")
  p <-paste0(substring(proj,1,1),p)
  #v <- as.character(visit_nr) # levels 10, 11, 12, 20, 30, 40, 41, 42
  static <- paste0("FOOD@\n",p,".",ifelse(visit_type=="scheduled","V","U"),visit_nr,".")
  if(visit_nr==1){
    samples <- c(rep("01",2),  # EDTA
                 rep("bla",18),
                 rep("02",50), # serum
                 rep("bla",10),
                 rep("03",20), # heparin
                 rep("04",4),  # dust bed
                 rep("05",4),  # dust livingroom
                 rep("06",4),  # stool
                 rep("07",4),  # stool dog
                 rep("08",8),  # saliva swab
                 if(proj=="Infant"){
                   rep("09",4)
                 } else {
                   rep("bla",4)
                 },  # saliva microbiome
                 rep("bla",12),
                 rep("10",4),  # microbiome skin location 1
                 rep("11",4),  # microbiome skin location 2
                 rep("12",4), # microbiome loc 3 
                                #serum after reaction started
                 rep("13",25),  # heparin 7 days post reaction
                 rep("bla",13),
                 rep("14",10),#see descr
                 rep("bla", 10),
                 rep("17", 5),
                 rep("18", 5)
    )
  } else if(visit_nr %in% c(2,3)){
    samples <- c(#rep("01",2),  # EDTA
      rep("02",50), # serum
      rep("bla",10),
      rep("04",4),  # dust bed
      #rep("05",4),  # dust livingroom
      if(proj!="Adult"){
        rep("06",4)  # stool
      } else {
        rep("bla",4)
      },
      #rep("07",4),  # stool dog
      rep("08",8),  # saliva swab
      if(proj!="Adult"){
        c(rep("09",4),
      #} else {
      #  rep("bla",4)
      #},  # saliva microbiome
      rep("bla",12),
      rep("10",4),  # microbiome skin location 1
      rep("11",4),  # microbiome skin location 2
      rep("12",4)) # loc3
      }#rep("13",15)  # heparin 7 days post reaction
    )
  } else if(visit_nr ==4){
    samples <- c(
      rep("01",2),  # EDTA
      rep("bla",8),
      rep("02",50), # serum
      rep("bla",10),
      rep("03",20), # heparin
      rep("04",4),  # dust bed
      #rep("05",4),  # dust livingroom
      rep("06",4),  # stool
      #rep("07",4),  # stool dog
      rep("08",8),  # saliva swab
      if(proj=="Infant"){
        rep("09",4)
      } else {
        rep("bla",4)
      },  # saliva microbiome
      rep("bla",12),
      rep("10",4),  # microbiome skin location 1
      rep("11",4),  # microbiome skin location 2
      rep("12",4), # serum after reaction started
      rep("13",25),  # heparin 7 days post reaction
      rep("bla",13),
      rep("14",10),#see descr
      rep("bla", 10),
      rep("17", 5),
      rep("18", 5),
      if(proj=="Adult"){
        rep("19", 10)
      }
    )
  }else if(visit_nr ==5){
    samples <- c(
      rep("02",50),
      rep("03",20))
  }
  if(visit_type=="unscheduled"){
    samples <- c(
      rep("01",2),  # EDTA
      rep("bla",8),
      rep("02",50), # serum
      rep("bla",10),
      rep("03",20), # heparin
      rep("04",4),  # dust bed
      rep("05",4),  # dust livingroom
      rep("06",4),  # stool
      rep("07",4),  # stool dog
      rep("08",8),  # saliva swab
      if(proj=="Infant"){
        rep("09",4)
      } else {
        rep("bla",4)
      },  # saliva microbiome
      rep("bla",12),
      rep("10",4),  # microbiome skin location 1
      rep("11",4),  # microbiome skin location 2
      rep("12",4), # serum after reaction started
      rep("13",25),  # heparin 7 days post reaction
      rep("bla",13),
      rep("14",10),#see descr
      rep("bla", 10),
      rep("17", 5),
      rep("18", 5)
    )
  }
  o <- paste0(static,samples,"\n",date) 
  o <- gsub(x = o,  pattern=".*bla.*",replacement = "- - -")
  return(data.frame(label = o))
}

generate_labels_per_visit2 <- function(proj, 
                                      patient,
                                      visit_nr,
                                      visit_type,
                                      date){
  p <- stringr::str_pad(patient, width = 3,pad = "0", side = "left")
  p <-paste0(substring(proj,1,1),p)
  #v <- as.character(visit_nr) # levels 10, 11, 12, 20, 30, 40, 41, 42
  static <- paste0("FOOD@\n",p,".",ifelse(visit_type=="scheduled","V","U"),visit_nr,".")
  if(visit_nr==1){
    samples <- c(rep("01",2),  # EDTA
                 rep("02",20), # serum
                 rep("03",6), # heparin
                 rep("04",2),  # dust bed
                 rep("05",2),  # dust livingroom
                 rep("06",2),  # stool
                 rep("07",2),  # stool dog
                 rep("08",4),  # saliva swab
                 rep("10",2),  # microbiome skin location 1
                 rep("11",2),  # microbiome skin location 2
                 rep("12",2), # microbiome loc 3 
                 #serum after reaction started
                 rep("17", 5)
    )
  } else if(visit_nr %in% c(2,3)){
    samples <- c(#rep("01",2),  # EDTA
      rep("02",20), # serum
      rep("04",2),  # dust bed
      rep("08",4),  # saliva swab
    )
  } else if(visit_nr ==4){
    samples <- c(
      rep("01",2),  # EDTA
      rep("bla",8),
      rep("02",50), # serum
      rep("bla",10),
      rep("03",20), # heparin
      rep("04",4),  # dust bed
      #rep("05",4),  # dust livingroom
      rep("06",4),  # stool
      #rep("07",4),  # stool dog
      rep("08",8),  # saliva swab
      if(proj=="Infant"){
        rep("09",4)
      } else {
        rep("bla",4)
      },  # saliva microbiome
      rep("bla",12),
      rep("10",4),  # microbiome skin location 1
      rep("11",4),  # microbiome skin location 2
      rep("12",4), # serum after reaction started
      rep("13",25),  # heparin 7 days post reaction
      rep("bla",13),
      rep("14",10),#see descr
      rep("bla", 10),
      rep("17", 5),
      rep("18", 5),
      if(proj=="Adult"){
        rep("19", 10)
      }
    )
  }else if(visit_nr ==5){
    samples <- c(
      rep("02",50),
      rep("03",20))
  }
  if(visit_type=="unscheduled"){
    samples <- c(
      rep("01",2),  # EDTA
      rep("bla",8),
      rep("02",50), # serum
      rep("bla",10),
      rep("03",20), # heparin
      rep("04",4),  # dust bed
      rep("05",4),  # dust livingroom
      rep("06",4),  # stool
      rep("07",4),  # stool dog
      rep("08",8),  # saliva swab
      if(proj=="Infant"){
        rep("09",4)
      } else {
        rep("bla",4)
      },  # saliva microbiome
      rep("bla",12),
      rep("10",4),  # microbiome skin location 1
      rep("11",4),  # microbiome skin location 2
      rep("12",4), # serum after reaction started
      rep("13",25),  # heparin 7 days post reaction
      rep("bla",13),
      rep("14",10),#see descr
      rep("bla", 10),
      rep("17", 5),
      rep("18", 5)
    )
  }
  o <- paste0(static,samples,"\n",date) 
  o <- gsub(x = o,  pattern=".*bla.*",replacement = "- - -")
  return(data.frame(label = o))
}

generate_labels_per_visit3 <- function(proj, 
                                       patient,
                                       visit_nr,
                                       visit_type,
                                       date){
  p <- stringr::str_pad(patient, width = 3,pad = "0", side = "left")
  p <-paste0(substring(proj,1,1),p)
  #v <- as.character(visit_nr) # levels 10, 11, 12, 20, 30, 40, 41, 42
  static <- paste0("FOOD@\n",p,".",ifelse(visit_type=="scheduled","V","U"),visit_nr,".")
  if(visit_nr==1 & proj == "Adult"){
    DF <- c(
      rep("03",5), # heparin,
      rep("14",5)
    )
    LCRY <- c(
      rep("02",40),
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
      rep("18",5)
    )
    Brady <- c(
      rep("15",6),
      rep("16",6)
    )
  } else if(visit_nr==1 & proj == "Child"){
    DF <- c(
      rep("01",2),
      rep("03",6)
    )
    LCRY <- c(
      rep("02",20),
      rep("04",2),
      rep("05",2),
      rep("06",2),
      rep("07",2),
      rep("08",5),
      rep("10",2),
      rep("11",2),
      rep("12",2),
      rep("17",4)
    )
    Brady <- c(
      rep("15",3)
    )
  } else if(visit_nr==1 & proj == "Infant"){
    DF <- c(
      rep("03",6)
    )
    LCRY <- c(
      rep("02",15),
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
      rep("20",15)
    )
    Brady <- c(
      rep("15",3)
    )
  } else if(visit_nr%in%c(2,3) & proj == "Adult"){
    DF <- c(
      rep("00",1)
    )
    LCRY <- c(
      rep("02",30),
      rep("04",2),
      rep("08",6)
      )
    Brady <- c(
      rep("00",1)
    )
  } else if(visit_nr%in%c(2,3) & proj == "Child"){
    DF <- c(
      rep("00",1)
    )
    LCRY <- c(
      rep("04",2),
      rep("08",5)
    )
    Brady <- c(
      rep("00",1)
    )
  } else if(visit_nr%in%c(2,3) & proj == "Infant"){
    DF <- c(
      rep("00",1)
    )
    LCRY <- c(
      rep("04",2),
      rep("06",2),
      rep("08",6),
      rep("09",4),
      rep("10",2),
      rep("11",2),
      rep("12",2)
    )
    Brady <- c(
      rep("00",1)
    )
  } else if(visit_nr==4 & proj == "Adult"){
    DF <- c(
      rep("03",5),
      rep("14",5),
      rep("19",5),
    )
    LCRY <- c(
      rep("02",40),
      rep("04",2),
      rep("06",2),
      rep("08",6),
      rep("10",2),
      rep("11",2),
      rep("12",2),
      rep("13",40),
      rep("17",5),
      rep("18",5)
    )
    Brady <- c(
      rep("15",6),
      rep("16",6)
    )
  } else if(visit_nr==4 & proj == "Child"){
    DF <- c(
      rep("01",2),
      rep("03",5)
    )
    LCRY <- c(
      rep("02",20),
      rep("04",2),
      rep("06",2),
      rep("08",6),
      rep("10",2),
      rep("11",2),
      rep("12",2),
      rep("17",5)
    )
    Brady <- c(
      rep("15",3)
    )
  } else if(visit_nr==4 & proj == "Infant"){
    DF <- c(
      rep("01",2),
      rep("03",5)
    )
    LCRY <- c(
      rep("02",20),
      rep("04",2),
      rep("06",2),
      rep("08",6),
      rep("10",2),
      rep("11",2),
      rep("12",2),
      rep("17",3),
      rep("20",15)
    )
    Brady <- c(
      rep("15",3)
    )
  } else if(visit_nr==5 & proj == "Infant"){
    DF <- c(
      rep("03",5)
    )
    LCRY <- c(
      rep("02",20)
    )
    Brady <- c(
      rep("00",1)
    )
  }
  o <- data.frame(label = c(DF,LCRY,Brady),
                  paper = c(rep("DF",length(DF)),
                            rep("LCRY",length(LCRY)),
                            rep("Brady",length(Brady))))
  o$label <- paste0(static,o$label,"\n",date) 
  return(o)
}



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




#### ui #### 

ui <- dashboardPage(
  dashboardHeader(title = "QR code Labels generator for clinical samples"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(h1("KFO339 Labelling System"),
          p("This app will help you generate labels for clinical samples within the KFO339 project."),
          p("Note that patients are divided into three age groups corresponding to different clinical samples"),
          p("Please provide the patient's number (screening number), visit number and if the visit is planned or unscheduled."),
            p ("You may find the paper for printing the labels:"), 
          a("here under this link",href= "https://www.bueromarkt-ag.de/universaletiketten_herma_4212_movables_weiss,p-4212,l-google-prd,pd-b2c.html?gclid=EAIaIQobChMI8oq1y4uB6AIVkh0YCh3CzgbVEAQYASABEgId4_D_BwE"))
    ),
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
      h1("Patient and Visit details"),
      box(selectizeInput("project", label = "Patient age group",choices = c("Infant","Child","Adult")),
             textInput("patient_n", label = "Patient number",value = "1"),
          selectizeInput("paper", label = "Paper type",choices = c("DeepF-LCRY","deep freeze: 4388","movables: 10000")),
             #width = 3
          ),
      box(radioButtons("visit_type", label = "Type of visit",choices = c("scheduled","unscheduled")),
             #width = 3,
          selectizeInput("v_n", label = "Visit number",choices = c("1","2","3","4","5")),
          dateInput("visit_date", label = "Visit date", value = Sys.Date())
      )
    )
    ),
    fluidRow(
      box(
        h1("Print Files"),
        p("Here You can download the files for printing the labels. Deep Freeze are cheap paper labels, Brady are for the Thermal printer for snap freezing, and LCRY are vinyl label for -80 deg C"),
        p("You can also specify from which line we should start printing to save paper."),
        numericInput("startLineDF", label = "What line to start on deep freeze paper?",1, min =1, max = 27),
        numericInput("startLineLCRY",label = "What line to start on LCRY paper?", 1, min =1, max = 17),
        downloadButton("downloadLabels", "Deep Freeze"),
        downloadButton("downloadExcel","Brady"),
        downloadButton("downloadLCRY","LCRY")
        )
      ),
    fluidRow(
      box(
        h1("Reference for coding numbers"),
        p("For reference the sample types and their corresponding IDs are listed below:"),
        p("01 - EDTA samples for diff. BB "),
        p("02 - serum before the reaction "),
        p("03 - heparin before the reaction "),
        p("04 - dust from the bedroom "),
        p("05 - dust from the living room "),
        p("06 - stool from the patient "),
        p("07 - stool from the patient's dog "),
        p("08 - saliva for cytokines "),
        p("09 - saliva/oral microbiome "),
        p("10 - skin swab in location 1 "),
        p("11 - skin swab in location 2 "),
        p("12 - skin swab in location 3 "),
        p("13 - serum post reaction "),
        p("14 - heparin 7 days post reaction "),
        p("15 - PBMCs_03  (Thermo Labels - Brady)"), #X 10
        p("16 - PBMCs_14  (Thermo Labels - Brady)"), #x 10
        p("17 - Plasma_03"), #x 5
        p("18 - Plasma_14"),
        p("20 - Serum from mothers")#x5
      )
    )
  )
)


#### Server #### 

server <- function(input, output) {
  
  #observeEvent(input$generateB, {
  #  cat("button pressed")
   
   # cat(list.files())
  #  output$test <-  reactive({
  #    cat("testing")
  #    any(list.files() == "LabelsOut.pdf")
  #  })
  #})
  
 
  #outputOptions(output, "test", suspendWhenHidden = FALSE)
  
  
  output$downloadLabels <- downloadHandler(
    filename = function(){
      paste0("labels-",input$patient_n,".pdf")
    },
    content = function(file){
      labels_pat1 <- generate_labels_per_visit(
      proj = input$project,
      patient = input$patient_n,
      visit_nr = input$v_n,
      visit_type = input$visit_type,
      date = format(input$visit_date,format="%d.%m.%y"))
  #show the generated results. 
      #cat(paste0("\nFirst Label = ",as.character(labels_pat1[1,]),
      #           "\nLast Label = ",as.character(labels_pat1[nrow(labels_pat1),])))
      if(input$paper=="movables: 10000"){
        custom_create_PDF_sub(user=FALSE,
                              Labels = labels_pat1[,],
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
      } else if(input$paper =="deep freeze: 4388"){
        custom_create_PDF_sub(user=FALSE,
                              Labels = labels_pat1[,],
                              name = 'LabelsOut',
                              type = 'matrix',
                              ErrCorr = 'M',
                              Fsz = 4,
                              Across = T,
                              ERows = 0,
                              ECols = 0,
                              trunc = F,
                              numrow = 26,
                              numcol = 10,
                              page_width = 8.27,
                              page_height = 11.5,
                              width_margin = 0.375,
                              height_margin = 0.4,
                              label_width = NA,
                              label_height = NA,
                              x_space = 0,
                              y_space = 0.5)
      }else if(input$paper =="LCRY-2380"){
        custom_create_PDF_sub2(user=FALSE,
                              Labels = generate_labels_per_visit2(
                                proj = input$project,
                                patient = input$patient_n,
                                visit_nr = input$v_n,
                                visit_type = input$visit_type,
                                date = format(input$visit_date,format="%d.%m.%y")),
                              name = 'LabelsOut',
                              type = 'matrix',
                              ErrCorr = 'M',
                              Fsz = 4,
                              Across = T,
                              ERows = 0,
                              ECols = 0,
                              trunc = F,
                              numrow = 17,
                              numcol = 7,
                              page_width = 8.5,
                              page_height = 10.9,
                              width_margin = 0.6,
                              height_margin = 0.28,
                              label_width = 0.8,
                              label_height = 0.4,
                              x_space = 0,
                              y_space = 0.5)
      } else if(input$paper =="DeepF-LCRY"){
        gls <- generate_labels_per_visit3(
          proj = input$project,
          patient = input$patient_n,
          visit_nr = input$v_n,
          visit_type = input$visit_type,
          date = format(input$visit_date,format="%d.%m.%y"))
        custom_create_PDF_sub(user=FALSE,
                              Labels = gls$label[gls$paper =="DF"],
                              name = 'LabelsOut',
                              type = 'matrix',
                              ErrCorr = 'M',
                              Fsz = 4,
                              Across = T,
                              ERows = input$startLineDF-1,
                              ECols = 0,
                              trunc = F,
                              numrow = 26,
                              numcol = 10,
                              page_width = 8.27,
                              page_height = 11.5,
                              width_margin = 0.375,
                              height_margin = 0.4,
                              label_width = NA,
                              label_height = NA,
                              x_space = 0,
                              y_space = 0.5)
      }
      
      cat(list.files())
      file.copy("LabelsOut.pdf",file)
    }
  )

  
    #### Download Excels ####
  output$downloadExcel <- downloadHandler(
    filename = function(){
      paste0("cryo-labels-",input$patient_n,"-",input$v_n,".xlsx")
    },
    content = function(file){
      gls <- generate_labels_per_visit3(
        proj = input$project,
        patient = input$patient_n,
        visit_nr = input$v_n,
        visit_type = input$visit_type,
        date = format(input$visit_date,format="%d.%m.%y"))
      labels_ex <- data.frame(project = "food@",
                              sample = substr(start = 7,
                                              stop = 16,
                                              gls$label[gls$paper=="Brady"]),
                              year = format(input$visit_date,format="%Y"))
      #show the generated results. 
      #cat(paste0("\nFirst Label = ",as.character(labels_ex[1,]),
      #           "\nLast Label = ",as.character(labels_ex[nrow(labels_ex),])))
      writexl::write_xlsx(labels_ex,"excel.xlsx",col_names = F)
      cat(list.files())
      file.copy("excel.xlsx",file)
    }
  )

  ### DownloadLCRY####
  output$downloadLCRY <- downloadHandler(
    filename = function(){
      paste0("labels-",input$patient_n,".pdf")
    },
    content = function(file){
        gls <- generate_labels_per_visit3(
          proj = input$project,
          patient = input$patient_n,
          visit_nr = input$v_n,
          visit_type = input$visit_type,
          date = format(input$visit_date,format="%d.%m.%y"))
        custom_create_PDF_sub2(user=FALSE,
                               Labels = gls$label[gls$paper=="LCRY"],
                               name = 'LabelsOut',
                               type = 'matrix',
                               ErrCorr = 'M',
                               Fsz = 4,
                               Across = T,
                               ERows = input$startLineLCRY-1,
                               ECols = 0,
                               trunc = F,
                               numrow = 17,
                               numcol = 7,
                               page_width = 8.5,
                               page_height = 10.9,
                               width_margin = 0.6,
                               height_margin = 0.28,
                               label_width = 0.8,
                               label_height = 0.4,
                               x_space = 0,
                               y_space = 0.5)
      
      cat(list.files())
      file.copy("LabelsOut.pdf",file)
    }
  )
  
  
    
}

shinyApp(ui, server)
