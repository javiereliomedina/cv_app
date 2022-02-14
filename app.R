library(shiny)
library(shinyFiles)
library(bslib)
library(promises)
library(shinybusy)

source("render_cv.r")

# link to Excel data
url_template <- "https://github.com/javiereliomedina/cv_app/blob/main/CV_data.xlsx?raw=true"

# UI ----
ui <- fluidPage(
  
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("cerulean"),
  
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
    
    p("- ", strong("text_blocks:"), "text with a summary of your CV and your main interest."),
    
    p("- ", strong("entries_data:"), "information about your education,
      employments, and training and teaching activities."),
    
    p("- ", strong("software:"), "software skills."),
    
    p("- ", strong("languages:"), "language skills."),
    
    p("- ", strong("publications:"), "list of articles you have written."),
    
    p("- ", strong("packages:"), "list of software packages (e.g. R, Python) you have developed."),
    
    p("- ", strong("apps:"), "list of software apps (e.g. Shiny) you have developed."),
    
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
    
    column(
      width = 6,
      htmlOutput("pdfviewer")
    )
    
  )
  
)

# Server ----
server <- function(input, output, session) {
  
  name <- eventReactive(input$buildPDF, { req(input$name) })
  # eval_text <- eventReactive(input$buildPDF, { req(input$summary) })
  # eval_sof  <- eventReactive(input$buildPDF, { req(input$software) })
  # eval_lan  <- eventReactive(input$buildPDF, { req(input$languages) })
  # eval_edu  <- eventReactive(input$buildPDF, { req(input$education) })
  # eval_emp  <- eventReactive(input$buildPDF, { req(input$employment) })
  # eval_tea  <- eventReactive(input$buildPDF, { req(input$teaching) })
  # eval_pub  <- eventReactive(input$buildPDF, { req(input$publication) })
  # eval_pck  <- eventReactive(input$buildPDF, { req(input$packages) })
  # eval_app  <- eventReactive(input$buildPDF, { req(input$apps) })
  
## Downloadable excel template for CV inputs ----
  
  output$downloadData <- downloadHandler(
    filename <- "cv_data_template.xlsx",
    content <- function(file) {
      httr::GET(url_template, httr::write_disk(path = file))
    }
  )
  
## Build CV ----
  observeEvent(input$buildPDF, {
    output$downloadBtn <- renderUI({

      # add a spinner which blocks the UI
      show_modal_spinner(spin = "fading-circle", color = "#98c1d9")
      
      # launch the PDF file generation
      render_cv(
        name_input = name(),
        path_input = input$upload$datapath,
        eval_text = input$summary,
        eval_sof = input$software,
        eval_lan = input$languages,
        eval_edu = input$education,
        eval_emp = input$employment,
        eval_tea = input$teaching,
        eval_pub = input$publication,
        eval_pck = input$packages,
        eval_app = input$apps
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

## Show my CV ----
  output$pdfviewer <- renderUI({
    tags$iframe(style = 'height: 550px; width: 400px;', src = "my_cv.pdf")
  })
  
}

# App ----
shinyApp(ui, server)

