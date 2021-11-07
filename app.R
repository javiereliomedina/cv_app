library(shiny)
library(shinyFiles)

source("render_cv.R")

ui <- fluidPage(
  
  downloadButton("downloadData", "Download excel template"),
  
  textInput("name", "Add your name"),
  
  fileInput("upload", "Upload CV data", accept = c(".xlsx")),
  
  shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE),
  
  actionButton("build_cv", "Build CV"),
  
  uiOutput("download_cv")
  
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
