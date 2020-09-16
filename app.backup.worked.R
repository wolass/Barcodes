#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("shinydashboard")

## app.R ##
library(shinydashboard)

library(baRcodeR)
generate_labels_per_visit <- function(patient,
                                      visit_nr){
  p <- stringr::str_pad(patient,width = 3,pad = "0", side = "left")
  #v <- as.character(visit_nr) # levels 10, 11, 12, 20, 30, 40, 41, 42
  static <- paste0(p,"-",visit_nr,"-")
  if(visit_nr==1){
    samples <- c(rep("01",2),  # EDTA
                 rep("02",20), # serum
                 rep("03",15), # heparin
                 rep("04",2),  # dust bed
                 rep("05",2),  # dust livingroom
                 rep("06",2),  # stool
                 rep("07",2),  # stool dog
                 rep("08",7),  # saliva swab
                 rep("09",2),  # saliva microbiome
                 rep("10",3),  # microbiome skin location 1
                 rep("11",3),  # microbiome skin location 2
                 rep("12",20), # serum after reaction started
                 rep("13",10)  # heparin 7 days post reaction
    )
  } else if(visit_nr %in% c(2,3)){
    samples <- c(#rep("01",2),  # EDTA
      rep("02",20), # serum
      #rep("03",15), # heparin
      rep("04",2),  # dust bed
      rep("05",2),  # dust livingroom
      rep("06",2),  # stool
      rep("07",2),  # stool dog
      rep("08",7),  # saliva swab
      rep("09",2),  # saliva microbiome
      rep("10",2),  # microbiome skin location 1
      rep("11",2)  # microbiome skin location 2
      #rep("12",20), # serum after reaction started
      #rep("13",15)  # heparin 7 days post reaction
    )
  } else if(visit_nr ==4){
    samples <- c(
      rep("01",2),  # EDTA
      rep("02",20), # serum
      rep("03",15), # heparin
      rep("04",2),  # dust bed
      rep("05",2),  # dust livingroom
      rep("06",2),  # stool
      rep("07",2),  # stool dog
      rep("08",7),  # saliva swab
      rep("09",2),  # saliva microbiome
      rep("10",2),  # microbiome skin location 1
      rep("11",2),  # microbiome skin location 2
      rep("12",15), # serum after reaction started
      rep("13",10),  # heparin 7 days post reaction
      rep("14",9)  # heparin post reaction
    )
  }
  o <- paste0(static,samples)
  return(data.frame(label = o))
}


ui <- dashboardPage(
  dashboardHeader(title = "QR code Labels generator from clinical samples"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(textInput("patient_n", label = "Patient number",value = "1"))
    ),
    fluidRow(
      box(actionButton("generateB", "Generate labels")),
      box(downloadButton("downloadLabels", "Download"))
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$generateB, {
    cat("button pressed")
    labels_pat1 <-
      rbind(generate_labels_per_visit(patient= input$patient_n,visit_nr = 1),
            generate_labels_per_visit(patient= input$patient_n,visit_nr = 2),
            generate_labels_per_visit(patient= input$patient_n,visit_nr = 3),
            generate_labels_per_visit(patient= input$patient_n,visit_nr = 4))
    cat(paste0("\nFirst Label = ",as.character(labels_pat1[1,]),
               "\nLast Label = ",as.character(labels_pat1[nrow(labels_pat1),])))
    custom_create_PDF(user=FALSE,
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
                      width_margin = 0.25,
                      height_margin = 0.5,
                      label_width = NA,
                      label_height = NA,
                      x_space = 0,
                      y_space = 0.5)
    cat(list.files())
  })
  
  output$downloadLabels <- downloadHandler(
    filename = function(){
      paste0("labels-",input$patient_n,".pdf")
    },
    content = function(file){
      file.copy("LabelsOut.pdf",file)
    }
  )
  
}

shinyApp(ui, server)
