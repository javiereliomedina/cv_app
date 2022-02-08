library(shiny)
library(shinyFiles)
library(bslib)
library(promises)
library(shinybusy)
library(shinythemes)

source("render_cv.r")

# UI ----
ui <- fluidPage(
  
  #shinythemes::themeSelector(),
  theme = shinytheme("cerulean"),
  
  titlePanel("Build an academic CV"),
  
  p("I have created this app for building an
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
    
    column(6, 
           
           uiOutput("download_cv"),
           
           textInput("name", "Add your name"),
           
           fileInput("upload", "Upload the excel file with your data", accept = c(".xlsx")),
           
           p("Select the sections you would like to add into your CV"), 
           
           fluidRow(
             
             column(
               width = 5,
               checkboxInput("summary", "CV summary", TRUE),
               checkboxInput("software", "Software", TRUE),
               checkboxInput("languages", "Languages", TRUE),
               checkboxInput("education", "Education", TRUE),
               checkboxInput("employment", "Employment", TRUE)
             ),
             
             column(
               width = 5,
               checkboxInput("teaching", "Teaching", TRUE),
               checkboxInput("publication", "Publications", TRUE),
               checkboxInput("packages", "Software development", TRUE),
               checkboxInput("apps", "Apps development", TRUE)
             )
           ), 
           
           p("Now you can build your CV. If everything works fine, you would get
             a message indicating that the PDF has been generated and a download
             button would appear to save it. It should be something similar to mine!!"
           ),
           
           actionButton("buildPDF", "Build PDF document"),
           uiOutput("downloadBtn")
          
    ),
    
    column(6, 
           
           htmlOutput("pdfviewer")
           
    )
    
  )
  
)

# Server ----
server <- function(input, output, session) {
  
  name <- eventReactive(input$build_cv, {
    req(input$name)
  })
  
  data <- eventReactive(input$build_cv, {
    req(input$upload)
  })
  
  eval_text <- eventReactive(input$build_cv, { req(input$summary) })
  eval_sof  <- eventReactive(input$build_cv, { req(input$software) })
  eval_lan  <- eventReactive(input$build_cv, { req(input$languages) })
  eval_edu  <- eventReactive(input$build_cv, { req(input$education) })
  eval_emp  <- eventReactive(input$build_cv, { req(input$employment) })
  eval_tea  <- eventReactive(input$build_cv, { req(input$teaching) })
  eval_pub  <- eventReactive(input$build_cv, { req(input$publication) })
  eval_pck  <- eventReactive(input$build_cv, { req(input$packages) })
  eval_app  <- eventReactive(input$build_cv, { req(input$apps) })
  
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
      eval_text  <- input$summary
      eval_sof   <- input$software
      eval_lan   <- input$languages
      eval_edu   <- input$education
      eval_emp   <- input$employment
      eval_tea   <- input$teaching
      eval_pub   <- input$publication
      eval_pck   <- input$packages
      eval_app   <- input$apps
      # add a spinner which blocks the UI
      show_modal_spinner()
      # launch the PDF file generation
      render_cv(name_input = name_input,
                path_input = path_input,
                eval_text = eval_text,
                eval_sof = eval_sof,
                eval_lan = eval_lan,
                eval_edu = eval_edu,
                eval_emp = eval_emp,
                eval_tea = eval_tea,
                eval_pub = eval_pub,
                eval_pck = eval_pck,
                eval_app = eval_app
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

  # Show my CV ----
  output$pdfviewer <- renderUI({
    tags$iframe(style = 'height: 550px; width: 400px;',
                src = "cv.pdf")
  })
  
}

# App ----
shinyApp(ui, server)

