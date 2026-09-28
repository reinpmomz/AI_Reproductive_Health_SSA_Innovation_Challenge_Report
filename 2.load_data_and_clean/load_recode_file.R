library(dplyr)
library(readxl)
library(tibble)
library(stringr)

working_directory

## Reading the recode file sheet

recode_file <- read_excel_allsheets("./2.load_data_and_clean/hash_hackathon_recode_file.xlsx")

study_details <- recode_file[["study"]]

application_rename_vars_df <- recode_file[["application_rename_vars"]] #df for renaming variable labels
information_webinar_rename_vars_df <- recode_file[["information_webinar_rename_vars"]] #df for renaming variable labels
capacity_building_rename_vars_df <- recode_file[["capacity_building_rename_vars"]] #df for renaming variable labels
african_countries <- recode_file[["african_countries"]]
country_language <- recode_file[["country_language"]]
needs_assessment_organization <- recode_file[["needs_assessment_organization"]]
needs_assessment_occupation <- recode_file[["needs_assessment_occupation"]]
needs_assessment_solution <- recode_file[["needs_assessment_solution"]]

## Creating a named vector to quickly assign variable names and labels
rename_vars_df <- sapply(ls(pattern = "_rename_vars_df$"), function(x){
  nn <- x
  df_new <- get(x)
  
  out <- df_new #%>%
    #dplyr::mutate(new_label = stringr::str_to_sentence(new_label))
  
}, simplify=FALSE)


new_var_names <-  sapply(names(rename_vars_df), function(x){ 
  out <- rename_vars_df[[x]] %>%
  dplyr::select(new_variable, new_names_janitor) %>%
  tibble::deframe()
  
}, simplify=FALSE)

new_labels <-  sapply(names(rename_vars_df), function(x){ 
  out <- rename_vars_df[[x]] %>%
  dplyr::select(new_variable, new_label) %>%
  tibble::deframe()
  
}, simplify=FALSE)

## Creating a named vector to quickly assign africa regions and language (English, French, Portuguese)
new_african_regions <- dplyr::bind_rows(african_countries %>%
                                          dplyr::select(region, country)
                                        , african_countries %>%
                                          dplyr::select(region, country_other_name) %>%
                                          dplyr::rename(country = country_other_name)
                                        ) %>%
  dplyr::distinct(country, .keep_all = TRUE) %>%
  tibble::deframe()
  
new_language <- country_language %>%
  dplyr::select(language, country) %>%
  tibble::deframe()

new_organization <- needs_assessment_organization %>%
  dplyr::select(organization_institution_new, organization_institution) %>%
  tibble::deframe()

new_occupation <- needs_assessment_occupation %>%
  dplyr::mutate(current_occupation = stringr::str_to_upper(current_occupation)) %>%
  dplyr::select(current_occupation_new, current_occupation) %>%
  tibble::deframe()

new_solution <- needs_assessment_solution %>%
  dplyr::select(type_of_solution_to_develop_new, type_of_solution_to_develop) %>%
  tibble::deframe()
