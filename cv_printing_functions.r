# This file contains all the code needed to parse and print various sections of your CV
# from data. Feel free to tweak it as you desire!


#' Create a CV_Printer object.
create_cv_object <-  function(data_path) {
  
  # Read data from excel
  cv <- data_path %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map(read_excel, skip = 1, path = data_path)
  
  
  extract_year <- function(dates){
    date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
    date_year[is.na(date_year)] <- lubridate::year(lubridate::ymd(Sys.Date())) + 10
    
    date_year
  }
  
  parse_dates <- function(dates){
    
    date_month <- stringr::str_extract(dates, "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})")
    date_month[is.na(date_month)] <- "1"
    
    paste("1", date_month, extract_year(dates), sep = "-") %>%
      lubridate::dmy()
  }
  
  # Clean up entries dataframe to format we need it for printing
  cv$entries_data %<>%
    tidyr::unite(
      tidyr::starts_with('description'),
      col = "description_bullets",
      sep = "\n- ",
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      description_bullets = ifelse(description_bullets != "", paste0("- ", description_bullets), ""),
      start = ifelse(start == "NULL", NA, start),
      end = ifelse(end == "NULL", NA, end),
      start_year = extract_year(start),
      end_year = extract_year(end),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start  & no_end  ~ "N/A",
        no_start  & has_end ~ as.character(end),
        has_start & no_end  ~ paste("Current", "-", start),
        TRUE                ~ paste(end, "-", start)
      )
    ) %>%
    dplyr::arrange(desc(parse_dates(end))) 
  
  cv
  
}


#' @description Take a position data frame and the section id desired and prints the section to markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
print_section <- function(cv, section_id){
  
  glue_template <- "
### {title}

{institution}

{loc}
  
{timeline}

{description_bullets}
\n\n\n"
  
  section_data <- dplyr::filter(cv$entries_data,
                                section == section_id,
                                in_resume == TRUE) %>% 
    dplyr::arrange(desc(end_year))
  
  print(glue::glue_data(section_data, glue_template))
  
  invisible(cv)
  
}

#' @description Prints publications into markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
print_publications <- function(cv){
  
  glue_template <- "
### {title}

{journal}

N/A

{year}

<i class='fa fa-link'></i> {doi}

"
  
  dplyr::filter(cv$publications,
                in_resume == TRUE) %>% 
    dplyr::arrange(desc(year))
  
  print(glue::glue_data(publications, glue_template))
  
  invisible(cv)
  
}

#' @description Prints languages.
print_languages <- function(cv){
  
  glue_template <- "

**{language}**: {level}.

"
  
  invisible(cv)
  
}

#' @description Prints packages
print_packages <- function(cv){
  
  glue_template <- "

**{package}**: {description}, {link}.

"
  
  print(glue::glue_data(cv$package, glue_template))
  
  invisible(cv)
  
}

#' @description Prints apps
print_apps <- function(cv){
  
  glue_template <- "

**{app}**: {description}, {link}.

"
  
  print(glue::glue_data(cv$apps, glue_template))
  
  invisible(cv)
  
}

#' @description Prints out text block identified by a given label.
#' @param label ID of the text block to print as encoded in `label` column of `text_blocks` table.
print_text_block <- function(cv, label){
  text_block <- dplyr::filter(cv$text_blocks, loc == label) %>%
    dplyr::pull(text) %>% 
    cat()
}

#' @description Construct a bar chart of skills
#' @param out_of The relative maximum for skills. Used to set what a fully filled in skill bar is.
print_skill_bars <- function(cv, 
                             out_of = 5, 
                             bar_color = "#969696",
                             bar_background = "#d9d9d9", 
                             glue_template = "default"){
  
  if(glue_template == "default"){
    glue_template <- "
<div
  class = 'skill-bar'
  style = \"background:linear-gradient(to right,
                                      {bar_color} {width_percent}%,
                                      {bar_background} {width_percent}% 100%)\"
>{skill}</div>"
  }
  
  cv$software %>%
    dplyr::mutate(width_percent = round(100*as.numeric(level)/out_of)) %>%
    glue::glue_data(glue_template) %>%
    print()
  
  invisible(cv)
}


#' @description List of all links in document labelled by their superscript integer.
print_links <- function(cv) {
  n_links <- length(cv$links)
  if (n_links > 0) {
    cat("
Links {data-icon=link}
--------------------------------------------------------------------------------

<br>

")
    
    purrr::walk2(cv$links, 1:n_links, function(link, index) {
      print(glue::glue('{index}. {link}'))
    })
  }
  
  invisible(cv)
}



#' @description Contact information section with icons
print_contact_info <- function(cv){
  glue::glue_data(cv$contact_info, 
                  "- <i class='fa fa-{icon}'></i> {contact}") %>%
    print()
  
  invisible(cv)
}
