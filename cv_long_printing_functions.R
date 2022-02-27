#' @description Print bold text in the pdf
bold_text <- function(x) {x <- str_c("\\textbf{", x, "}")}

#' Format table for CV output ----
#' @param df Data frame with the sesction info 

cv_table <-  function(df) {
  df %>% 
    knitr::kable(col.names = NULL,
                 escape = FALSE,
                 align = 1L,
                 longtable = TRUE,
                 format = "latex"
    ) %>%
    kableExtra::kable_styling(full_width = F) %>%
    kableExtra::column_spec(1, color = "darkgray", width = "3.5em") %>%
    kableExtra::column_spec(2, width = "40.5em")
  
}

#' Create a CV_Printer object.
create_cv_object <-  function(data_path) {
  
  # Read data from excel
  cv <- data_path %>% 
    readxl::excel_sheets() %>% 
    purrr::set_names() %>% 
    purrr::map(readxl::read_excel, skip = 1, path = data_path)
  
  cv
  
}

#' @description Take a position data frame and the section id desired and prints the section to markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
print_section <- function(cv, section_id) {
  
  cv$entries_data %>% 
    filter(section == section_id) %>% 
    select(start, end, everything()) %>% 
    select(-section, -in_resume) %>% 
    relocate(loc, .after = last_col()) %>% 
    mutate(end = str_sub(end, -2), 
           title = bold_text(title)) %>% 
    unite(year, start, end, sep = "-", na.rm = TRUE) %>% 
    janitor::remove_empty(which = "cols") %>% 
    unite(details, 2:last_col(), sep = ", ", na.rm = TRUE) %>%
    mutate(details = paste0(details, ".")) %>% 
    # Note sure why I get an error with one-row tables, add empty a row with values
    bind_rows(tibble(year = "", details = " ")) %>%
    arrange(desc(year)) %>% 
    cv_table()
  
}

#' @description Prints publications into markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
#' @param surname Bold surname in the pdf
print_publications <- function(cv, pub_type, surname){
  
  cv$publications %>%
    filter(type == pub_type) %>%
    mutate(authors = gsub(surname, paste0("\\\\textbf{", surname, "}"), authors),
           doi = paste0("\\textcolor{blue}{\\url{", doi, "}}", sep = "")) %>% 
    unite(details, authors, title, journal, doi, sep = ", ") %>% 
    select(year, details) %>%
    mutate(details = paste0(details, ".")) %>% 
    arrange(desc(year)) %>%
    cv_table()
  
}

#' @description Prints out text block identified by a given label.
#' @param label ID of the text block to print as encoded in `label` column of `text_blocks` table.
print_text_block <- function(cv, label){
  text_block <- dplyr::filter(cv$text_blocks, loc == label) %>%
    dplyr::pull(text) %>% 
    cat()
}

#' @description Print research interest

print_reserch_intrerest <- function(cv){
  
  cv$text_blocks %>% 
    dplyr::filter(loc == "research_interests") %>% 
    stringr::str_split("; ") %>% 
    purrr::pluck(2) %>% 
    paste("  -", .) %>% 
    cat(., sep = "\n")
  
}



  

