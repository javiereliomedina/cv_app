library(shiny)
library(shinyFiles)
library(bslib)
library(promises)
library(shinybusy)
library(tidyverse)

source("render_cv.r")
source("cv_long_printing_functions.r")

# link to Excel data
url_template <- "https://github.com/javiereliomedina/cv_app/blob/main/CV_data.xlsx?raw=true"

# UI ----
ui <- fluidPage(
  
  add_busy_spinner(spin = "fading-circle",
                   color = "#98c1d9",
                   position = "full-page"),
  theme = shinythemes::shinytheme("cerulean"),
  
  tabsetPanel(
    
    # App introduction ----
    
    tabPanel("Intro",
             
             titlePanel("Introduction"),
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 h1("Download Excel template"),
                 
                 p("You would need to create an excel file with all the information you would
      like to put in your CV. The file should have six sheets:"),
      
      p("- ", strong("contact_info:"),"your contact information."),
      
      p("- ", strong("text_blocks:"), "text with a summary of your CV and your main interest."),
      
      p("- ", strong("entries_data:"), "information about your education,
          employments, training and teaching activities, grants, awards, disemination."),
      
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
        
        p("I have created this app for building academic CVs from an excel file.
          The idea is to save all our profesional information in only one file,
          which we can use later to get various CVs depending on our needs
          (e.g. short vs. full academic CV). It uses CV templates from",
        a("pagedown", href = "https://github.com/rstudio/pagedown.git"),
        "and",
        a("vitae,", href = "https://github.com/mitchelloharawild/vitae.git"),
        "and it is inspired on the",
        a("datadrivencv", href = "https://github.com/nstrayer/datadrivencv"),
        "package and a demo shiny app developed by",
        a("Romain Lesur.", href = "https://github.com/RLesur/chrome_print_shiny.git"),
        "If you use it, please let me know what you think on my",
        a("twitter", href = "https://twitter.com/Elio_Javi"),
        "or",
        a("linkedin", href = "https://www.linkedin.com/in/javiereliomedina/"),
        "pages."),
        p("There are two panels with diferent templates depending on the CV you would like
        to generate."),
        tags$li(strong("2-pages CV:"), "the template is optimised for a 2 pages document 
              and it may be more interesting if you apply for industrial jobs."),
        tags$li(strong("Full CV:"), "an extended academic CV with a list of publications."),
        br(),
        p("Both templates, however, use the same excel file for importing the data,
          which I think is very handy!!.
          You may find the excel template in",
          em("Download Excel template."))
        
      ),
      
      position = "right"
      
             )
      
    ),
    
    # Short CV (2-pages) ----
    tabPanel("2-pages CV",
             
             titlePanel("Short academic CV"),
             
             sidebarLayout(
             
               sidebarPanel(
                 
                 h1("Input data"),
                 textInput("name", "Add your name"),
                 fileInput("upload", "Upload the excel file with your data", accept = c(".xlsx"))
                 
               ),
               mainPanel(
                 
                 column(
                   width = 6,
                   
                   uiOutput("download_cv"),
                   
                   p(strong("Select the sections you would like to add into your CV")), 
                   
                   fluidRow(
                     column(
                       width = 4,
                       checkboxInput("summary", "CV summary", TRUE),
                       checkboxInput("software", "Software", TRUE),
                       checkboxInput("languages", "Languages", TRUE),
                       checkboxInput("education", "Education", TRUE),
                       checkboxInput("employment", "Employment", TRUE)
                     ),
                     column(
                       width = 4,
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
             ),
    
    # Full CV ----
    tabPanel("Full CV",
             
             titlePanel("Full academic CV"),
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 h1("Input data"),
                 textInput("name_long", "Add your name"),
                 textInput("surname_long", "Add your surname"),
                 fileInput("upload_long", "Upload the excel file with your data", accept = c(".xlsx")),
                 p(strong("Add 3 keywords describing you or your research interest")),
                 textInput("key_1", ""),
                 textInput("key_2", ""),
                 textInput("key_3", "")
                 
               ),
               
               mainPanel(
                 
                 column(6,
                        
                        uiOutput("download_cv_long"),
                        
                        p(strong("Select the sections that applied to you:")), 
                        
                        fluidRow(
                          column(
                            width = 5,
                            checkboxInput("summary_long", "CV summary", TRUE),
                            checkboxInput("education_long", "Education", TRUE),
                            checkboxInput("employment_long", "Experience", TRUE),
                            checkboxInput("inv_position_long", "Invited Positions", TRUE),
                            checkboxInput("membership_long", "Professional memberships", TRUE),
                            checkboxInput("service_long", "Professional services", TRUE),
                            checkboxInput("project_long", "Funding projects", TRUE)
                            
                          ),
                          column(
                            width = 5,
                            checkboxInput("award_long", "Honours and awards", TRUE),
                            checkboxInput("teaching_long", "Lectures/Seminars", TRUE),
                            checkboxInput("supervision_long", "Supervision", TRUE),
                            checkboxInput("ext_sensor_long", "External sensor", TRUE),
                            checkboxInput("article_long", "Peer reviewed articles", TRUE),
                            checkboxInput("book_long", "Books and book chapters", TRUE)
                          )
                        ), 
                        
                        p("Now you can build your CV. If everything works fine, you would get
             a PDF with your full academic CV. It should be something similar to mine!!"
                        ),
             
             downloadButton("buildPDF_long", "Build PDF document")
             
                 ),
             
             
             
             column(6, htmlOutput("pdfviewer_long"))
             
               )
             
             )
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
# Downloadable excel template for CV inputs ----
  output$downloadData <- downloadHandler(
    filename <- "cv_data_template.xlsx",
    content <- function(file) {
      httr::GET(url_template, httr::write_disk(path = file))
    }
  )
  
# Short CV ----
  name_react <- reactive({ input$name }) %>% bindEvent(input$buildPDF) 
  summary_react <- reactive({ input$summary }) %>% bindEvent(input$buildPDF)
  software_react <- reactive({ input$software }) %>% bindEvent(input$buildPDF)
  languages_react <- reactive({ input$languages }) %>% bindEvent(input$buildPDF)
  education_react <- reactive({ input$education }) %>% bindEvent(input$buildPDF)
  employment_react <- reactive({ input$employment }) %>% bindEvent(input$buildPDF)
  teaching_react <- reactive({ input$teaching }) %>% bindEvent(input$buildPDF)
  publication_react <- reactive({ input$publication }) %>% bindEvent(input$buildPDF)
  packages_react <- reactive({ input$packages }) %>% bindEvent(input$buildPDF)
  apps_react <- reactive({ input$apps }) %>% bindEvent(input$buildPDF)
  
  ### Build CV
  
  observeEvent(input$buildPDF, {
    
    output$downloadBtn <- renderUI({

      # launch the PDF file generation
      render_cv_short(

      # launch the PDF file generation
      render_cv(
        name_input = name_react(),
        path_input = input$upload$datapath,
        eval_text = summary_react(),
        eval_sof = software_react(),
        eval_lan = languages_react(),
        eval_edu = education_react(),
        eval_emp = employment_react(),
        eval_tea = teaching_react(),
        eval_pub = publication_react(),
        eval_pck = packages_react(),
        eval_app = apps_react()
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
          downloadButton("downloadPDF", "Download CV")
        },
        onRejected = function(error) {
          showNotification(
            error$message,
            duration = NULL,
            type = "error"
          )
          HTML("")
        }
      )
      
    )
    
  })
    
  })
  
  observeEvent("cv", {
    
    output$downloadBtn <- renderUI(HTML(""))
    
  })
  
  ### Show my CV 
  output$pdfviewer <- renderUI({
    tags$iframe(style = 'height: 550px; width: 400px;', src = "cv_short.pdf")
  })
  
# Full CV  ----
   
  # Get contact informations
  cv <- reactive({ create_cv_object(input$upload_long$datapath) })
  
  output$buildPDF_long <- downloadHandler(
    filename = "cv_full.pdf", 
    content = function(file) {
      
      # Temporary directory
      tempCV <- file.path(tempdir(), "cv_long.Rmd")
      file.copy("cv_long.Rmd", tempCV, overwrite = TRUE)
      tempPrint <- file.path(tempdir(), "cv_long_printing_functions.r")
      file.copy("cv_long_printing_functions.r", tempPrint, overwrite = TRUE )
      
      # Knit the document
      output <- output <- rmarkdown::render(
        input = tempCV,
        params = list(cv_name = input$name_long,
                      cv_surname = input$surname_long,
                      key_1 = input$key_1,
                      key_2 = input$key_2,
                      key_3 = input$key_3,
                      data_path = input$upload_long$datapath,
                      summary_long = input$summary_long,
                      education_long = input$education_long,
                      employment_long = input$employment_long,
                      inv_position_long = input$inv_position_long,
                      membership_long = input$membership_long,
                      service_long = input$service_long,  
                      project_long = input$project_long,
                      award_long = input$award_long,
                      teaching_long = input$teaching_long,
                      supervision_long = input$supervision_long,
                      ext_sensor_long = input$ext_sensor_long,
                      article_long = input$article_long,
                      book_long = input$book_long,
                      email = pull(filter(cv()$contact_info, loc == "email"), user),
                      website = pull(filter(cv()$contact_info, loc == "website"), user), 
                      github =  pull(filter(cv()$contact_info, loc == "github"), user), 
                      linkedin = pull(filter(cv()$contact_info, loc == "linkedin"), user),
                      twitter = pull(filter(cv()$contact_info, loc == "twitter"), user) 
                      )
        )
      file.copy(output, file)  
      })
  
  ### Show long CV
  output$pdfviewer_long <- renderUI({
    tags$iframe(style = 'height: 550px; width: 400px;', src = "cv_long.pdf")
  })
  
}

# App ----
shinyApp(ui, server)
