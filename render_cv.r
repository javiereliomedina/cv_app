# Print cv as pdf

render_cv <- function(name_input, path_input) {
  
  input <- rmarkdown::render("cv.rmd",
                             params = list(cv_name = name_input,
                                           data_path = path_input))
  pagedown::chrome_print(input = input,
                         output = paste0("cv-", Sys.Date(), ".pdf"),
                         async = TRUE)
}


#' Return Chrome CLI arguments
#'
#' This is a helper function which returns arguments to be passed to Chrome.
#' This function tests whether the code is running on shinyapps and returns the
#' appropriate Chrome extra arguments.
#'
#' @param default_args Arguments to be used in any circumstances.
#'
#' @return A character vector with CLI arguments to be passed to Chrome.
chrome_extra_args <- function(default_args = c("--disable-gpu")) {
  args <- default_args
  # Test whether we are in a shinyapps container
  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    args <- c(args,
              "--no-sandbox", # required because we are in a container
              "--disable-dev-shm-usage") # in case of low available memory
  }
  args
}
