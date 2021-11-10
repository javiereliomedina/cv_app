library(shiny)
library(shinyFiles)
library(bslib)
library(promises)
library(shinybusy)

source("render_cv.r")

ui <- fluidPage(
  
  titlePanel("Build an academic CV"),
  
  p("In my process of learning Shiny I have created this app for building an
    academic CV from an excel file. It uses the CV template of",
    a("pagedown,", href = "https://github.com/rstudio/pagedown.git"),
    "and it is inspired on the",
    a("datadrivencv", href = "https://github.com/nstrayer/datadrivencv"),
    "package and a demo shiny app developed by",
    a("Romain Lesur.", href = "https://github.com/RLesur/chrome_print_shiny.git"),
    "If you use it, please let me know what you think on my",
    a("twitter", href = "https://twitter.com/Elio_Javi"),
    "or",
    a("linkedin", href = "https://www.linkedin.com/in/javiereliomedina/"),
    "pages."
  ),
  
  sidebarPanel(
    
    h1("Download Excel template"), 
    
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
           
           fileInput("upload", "Upload the excel file with your data", accept = c(".xlsx")),
           
           p("Now you can build your CV. If everything works fine, you would get
             a message indicating that the PDF has been generated and a download
             button would appear to save it. It should be something similar to mine!!"
           ),
           
           actionButton("buildPDF", "Build PDF document"),
           uiOutput("downloadBtn")  
           
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
  
  # output$download_cv <- renderUI(
  #   render_cv(name_input = name(),
  #             path_input = input$upload$datapath)
  # )
  
  # Downloadable excel template for CV inputs ----
  url_template <- "https://github.com/javiereliomedina/cv_app/blob/main/CV_data.xlsx?raw=true"
  
  output$downloadData <- downloadHandler(
    filename <- "cv_data_template.xlsx",
    content <- function(file) {
      httr::GET(url_template, httr::write_disk(path = file))
    }
  )
  
  # Build CV ----
  observeEvent(input$buildPDF, {
    output$downloadBtn <- renderUI({
      name_input <- input$name
      path_input <- input$upload$datapath
      # add a spinner which blocks the UI
      show_modal_spinner()
      # launch the PDF file generation
      pagedown::chrome_print(
        rmarkdown::render("cv.rmd", params = list(cv_name = name_input,
                                                  data_path = path_input)),
        output = tempfile(fileext = ".pdf"),
        extra_args = chrome_extra_args(),
        verbose = 1,
        async = TRUE # returns a promise
      )$then(
        onFulfilled = function(value) {
          showNotification(
            paste("PDF file successfully generated"),
            type = "message"
          )
          output$downloadPDF <- downloadHandler(
            filename = function() {
              "cv.pdf"
            },
            content = function(file) {
              file.copy(value, file)
            },
            contentType = "application/pdf"
          )
          # return a download button
          downloadButton("downloadPDF", paste("Download", "CV"))
        },
        onRejected = function(error) {
          showNotification(
            error$message,
            duration = NULL,
            type = "error"
          )
          HTML("")
        }
      )$finally(remove_modal_spinner)
    })
  })
  
  observeEvent("cv", {
    output$downloadBtn <- renderUI(HTML(""))
  })

  
  
  
}

shinyApp(ui, server)
