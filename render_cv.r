# Print cv as pdf

render_cv <- function(name_input, path_input, path_output) {
  
  input <- rmarkdown::render("cv.rmd",
                             params = list(cv_name = name_input,
                                           data_path = path_input))
  pagedown::chrome_print(input = input,
                         output = paste(path_output, "cv.pdf", sep = "/"),
                         async = TRUE)
}


