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
source(file = "herma.R")
source(file = "LCRY.R")
source(file = "brady.R")
source(file = "herma2.R")# Time course extra
source(file = "LCRY2.R")# Time course extra
source(file = "LCRY3.R")# Time course extra2!
source(file = "LCRY_saliva.R")# saliva only



#### ui #### 

ui <- dashboardPage(
  dashboardHeader(title = "QR code Labels generator for clinical samples"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(h1("KFO339 Labelling System"),
          p("This app will help you generate labels for clinical samples within the KFO339 project."),
            p("Note that patients are divided into three age groups corresponding to different clinical projects."),
            p("Please provide the patient's number (e.g. A003, I008, C014), visit number and if the visit is planned or unscheduled."),
          p("Please note that the patients from the Austrian study have a designation starting with an O, please choose Oesterreich in patients age group in order to generate labels for this cohort."),
          hr(),
          p ("You may find the paper for printing the labels:"), 
          p(a("HERMA",href="https://www.herma.de/buero-zuhause/produkt/haftetiketten-a4-movables-10001/")),
          p(a("LCRY-2380 (Cryo Babies)", href = "https://www.carlroth.com/de/de/etiketten/kryo-etiketten-auf-dem-bogen-weiss/p/x547.1")))
    ),
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
      h1("Patient and Visit details"),
      box(selectizeInput("project", label = "Cohort (former age group)",choices = c("Infant","Child","Adult","Healthy","Oesterreich","Healthy Children")),
             textInput("patient_n", label = "Patient number",value = "1"),
          #selectizeInput("paper", label = "Paper type",choices = c("LCRY-2380","HERMA")),
             #width = 3
          ),
      box(radioButtons("visit_type", label = "Type of visit",choices = c("scheduled","unscheduled")),
             #width = 3,
          selectizeInput("v_n", label = "Visit number",choices = c("1","2","3","4","5","6")),
          dateInput("visit_date", label = "Visit date", value = Sys.Date())
      )
    )
    ),
    fluidRow(
      box(
        h1("Print Files"),
        p("Here You can download the files for printing the labels. HERMA are cheap paper labels, LCRY-2380 (CryoBabies) are vinyl label for -80 deg C"),
        p("You can also specify from which line we should start printing to save paper."),
        numericInput("startLineHERMA", label = "What line to start on HERMA paper?",1, min =1, max = 27),
        numericInput("startLineLCRY",label = "What line to start on LCRY-2380 paper?", 1, min =1, max = 17),
        p("NOTE: Please use CHROME BROWSER to print the labels and set the paper type to A4 and scaling to 100%. Unfortunately Windows printer setings have different margin widths and do not allow to print the file in the same format. By using chrome in all of our prints we are able to make it a bit more uniform. Recommended Printer is the KYOCERA ECOSYS P2040dn"),
        downloadButton("downloadLabels", "Herma"),
        downloadButton("downloadLCRY","LCRY-2380"),
        downloadButton("downloadExcel", label = "Brady printer file"),
        p("EXTRA LABELS: These print files are for Time course labels"),
        downloadButton("downloadLabels2", "Herma"),
        downloadButton("downloadLCRY2","LCRY-2380"),
        downloadButton("downloadLCRY3","LCRY-2380-Follow up"),
        p("Additional labels for the saliva samples only, retrospective visits"),
        downloadButton("downloadLCRY_saliva","LCRY-2380 Saliva only")
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
        p("20 - Serum from mothers"),
        p("21 - skin of the patient's dog"),#x5
        p("22 - fresh stool patient"),
       p("23 - serum after 30’ of allergen contact"),
       p("24 - serum after 90’ of allergen contact"),
       p("25 - heparin 14±1 days post reaction"),      
       p("26 - heparin 21±1 days post reaction"),
       p("27 - heparin 28±1 days post reaction"),       
       p("28 - serum 7d post reaction"),
       p("29 - serum 14d post reaction"),
       p("30 - serum 21d post reaction"),
       p("31 - serum 28d post reaction"),
       p("32 - stool OMNImet from the patient"),
       p("33 - saliva after 30’ of allergen contact"),
       p("34 - saliva after 90’ of allergen contact"),
       p("35 - saliva post reaction")
      )
    )
  )
)


#### Server #### 

server <- function(input, output) {
  
##### Download Herma #####  
  output$downloadLabels <- downloadHandler(
    filename = function(){
      paste0("Herma-labels-",input$patient_n,".pdf")
    },
    content = function(file){
      labels_pat1 <- generate_labels_per_visit_herma(
      proj = input$project,
      patient = input$patient_n,
      visit_nr = input$v_n,
      visit_type = input$visit_type,
      date = format(input$visit_date,format="%d.%m.%y"))
   #### Herma paper definition ####
        custom_create_PDF_sub(user=FALSE,
                              Labels = labels_pat1[,],
                              name = 'LabelsOut',
                              type = 'matrix',
                              ErrCorr = 'M',
                              Fsz = 4,
                              Across = T,
                              ERows = input$startLineHERMA-1,
                              ECols = 0,
                              trunc = F,
                              numrow = 27,
                              numcol = 7,
                              page_width = 8.27,
                              page_height = 11.625,
                              width_margin = 0.33,
                              height_margin = 0.5314,
                              label_width = 1,
                              label_height = 0.385,
                              x_space = 0,
                              y_space = 0.5,
                              npc_y = 0.32) # This sets the position of the sublabel under qr code
      cat(list.files())
      file.copy("LabelsOut.pdf",file)
    }
  )

  
    #### Download Excels ####
  output$downloadExcel <- downloadHandler(
    filename = function(){
      paste0("Brady-labels-",input$patient_n,"-",input$v_n,".xlsx")
    },
    content = function(file){
      gls <- generate_labels_per_visit_brady(
        proj = input$project,
        patient = input$patient_n,
        visit_nr = input$v_n,
        visit_type = input$visit_type,
        date = format(input$visit_date,format="%d.%m.%y"))
      labels_ex <- data.frame(project = "food@",
                              sample = substr(gls$label,start = 7,16),
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
      paste0("LCRY-labels-",input$patient_n,".pdf")
    },
    content = function(file){
        gls <- generate_labels_per_visit_LCRY(
          proj = input$project,
          patient = input$patient_n,
          visit_nr = input$v_n,
          visit_type = input$visit_type,
          date = format(input$visit_date,format="%d.%m.%y"))
    #####LCRY Paper Definition ####
            custom_create_PDF_sub2(user=FALSE,
                               Labels = gls$label,
                               name = 'LabelsOut',
                               type = 'matrix',
                               ErrCorr = 'L',
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
  
  ############## Time course labels ##############
  ##### Download Herma #####  
  output$downloadLabels2 <- downloadHandler(
    filename = function(){
      paste0("Herma-labels-",input$patient_n,".pdf")
    },
    content = function(file){
      labels_pat1 <- generate_labels_per_visit_herma2(
        proj = input$project,
        patient = input$patient_n,
        visit_nr = input$v_n,
        visit_type = input$visit_type,
        date = format(input$visit_date,format="%d.%m.%y"))
      #### Herma paper definition ####
      custom_create_PDF_sub(user=FALSE,
                            Labels = labels_pat1[,],
                            name = 'LabelsOut',
                            type = 'matrix',
                            ErrCorr = 'L',
                            Fsz = 4,
                            Across = T,
                            ERows = input$startLineHERMA-1,
                            ECols = 0,
                            trunc = F,
                            numrow = 27,
                            numcol = 7,
                            page_width = 8.27,
                            page_height = 11.625,
                            width_margin = 0.33,
                            height_margin = 0.5314,
                            label_width = 1,
                            label_height = 0.385,
                            x_space = 0,
                            y_space = 0.5,
                            npc_y = 0.32) # This sets the position of the sublabel under qr code
      cat(list.files())
      file.copy("LabelsOut.pdf",file)
    }
  )
  ### DownloadLCRY####
  output$downloadLCRY2 <- downloadHandler(
    filename = function(){
      paste0("LCRY-labels-",input$patient_n,".pdf")
    },
    content = function(file){
      gls <- generate_labels_per_visit_LCRY2(
        proj = input$project,
        patient = input$patient_n,
        visit_nr = input$v_n,
        visit_type = input$visit_type,
        date = format(input$visit_date,format="%d.%m.%y"))
      #####LCRY Paper Definition ####
      custom_create_PDF_sub2(user=FALSE,
                             Labels = gls$label,
                             name = 'LabelsOut',
                             type = 'matrix',
                             ErrCorr = 'L',
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
    ### DownloadLCRY only 1-day time course####
    output$downloadLCRY3 <- downloadHandler(
      filename = function(){
        paste0("LCRY-labels-",input$patient_n,".pdf")
      },
      content = function(file){
        gls <- generate_labels_per_visit_LCRY3(
          proj = input$project,
          patient = input$patient_n,
          visit_nr = input$v_n,
          visit_type = input$visit_type,
          date = format(input$visit_date,format="%d.%m.%y"))
        #####LCRY Paper Definition ####
        custom_create_PDF_sub2(user=FALSE,
                               Labels = gls$label,
                               name = 'LabelsOut',
                               type = 'matrix',
                               ErrCorr = 'L',
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
  ########## DownloadLCRY only saliva####
    output$downloadLCRY_saliva <- downloadHandler(
      filename = function(){
        paste0("LCRY-saliva-",input$patient_n,".pdf")
      },
      content = function(file){
        gls <- generate_labels_per_visit_LCRY_saliva(
          proj = input$project,
          patient = input$patient_n,
          visit_nr = input$v_n,
          visit_type = input$visit_type,
          date = format(input$visit_date,format="%d.%m.%y"))
        #####LCRY Paper Definition ####
        custom_create_PDF_sub2(user=FALSE,
                               Labels = gls$label,
                               name = 'LabelsOut',
                               type = 'matrix',
                               ErrCorr = 'L',
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
