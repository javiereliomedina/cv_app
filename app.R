library(shiny)
library(shinyFiles)
library(bslib)

source("render_cv.R")

ui <- fluidPage(
  
  titlePanel("Build an academic CV from excel file"),
  
  p("In my process of learning Shiny I have created this app for building an
    academic CV from an excel file. It uses the CV template of",
    a("pagedown", href = "https://github.com/rstudio/pagedown.git"),
    "and it is inspired on the",
    a("datadrivencv", href = "https://github.com/nstrayer/datadrivencv"), 
    "package."
  ),
  
  
  sidebarPanel(
    
    h1("Download excel template"), 
    
    p("You would need to create an excel file with all the information you would
      like to put in your CV. The file should have six sheets:"),
    
    p("- ", strong("contact_info:"),"your contact information."),
    
    p("- ", strong("text_blocks:"), "text with a summary of your CV."),
    
    p("- ", strong("entries_data:"), "information about your education,
      employments and teaching activities."),
    
    p("- ", strong("software:"), "software skills."),
    
    p("- ", strong("publications:"), "list of articles you have written."),
    
    p("- ", strong("languages:"), "language skills."),
    
    br(),
    
    p("You may download a template in the following link:"),  
    
    downloadButton("downloadData", "Download")
    
  ),
  
  mainPanel( 
    
    h1("Create CV"),
    
    column(4, 
           
           uiOutput("download_cv"),
           
           textInput("name", "Add your name"),
           
           fileInput("upload", "Upload an excel file with your data", accept = c(".xlsx")),
           
           p("You would also need to select the folder where your CV will be saved."),
           shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE),
           
           br(),
           br(),
           p("Now you can build your CV as pdf, it should be something similar to mine!!"),
           
           actionButton("build_cv", "Build CV")  
           
    ),
    
    column(8, 
           
           img(src = "img_cv.png", height = 600, width = 550)
           
    )
    
  )
  
)

server <- function(input, output, session) {
  
  name <- eventReactive(input$build_cv, {
    req(input$name)
  })
  
  data <- eventReactive(input$build_cv, {
    req(input$upload)
  })
  
  roots <- c(wd ='C:')
  
  shinyDirChoose(input, 'folder', roots = roots, filetypes=c('', 'txt'))
  
  output$download_cv <- renderUI(
    render_cv(name_input = name(),
              path_input = input$upload$datapath,
              path_output = parseDirPath(roots, input$folder)
    )
  )
  
  # Downloadable excel template for CV inputs ----
  url_template <- "https://github.com/javiereliomedina/cv_app/blob/main/CV_data.xlsx?raw=true"
  
  output$downloadData <- downloadHandler(
    filename <- "cv_data_template.xlsx",
    content <- function(file) {
      httr::GET(url_template, httr::write_disk(path = file))
    }
  )
  
}

shinyApp(ui, server)
