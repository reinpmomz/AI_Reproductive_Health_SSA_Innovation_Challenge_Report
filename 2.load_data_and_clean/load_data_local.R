library(dplyr)
library(haven)
library(janitor)
library(tidyr)
library(writexl)
library(labelled)

working_directory

## Reading data from local folder

data_files <- list.files(path = data_Dir, pattern = NULL,  full.names = F)

df_list <- sapply(data_files, function(x){
  nn <- x
  
  data_subDir <- file.path(data_Dir, nn)
  #read excel files
    
  df_raw <- read_excel_allsheets(data_subDir)
  
}, simplify=FALSE)


### Application 
df_raw_application <- df_list[["application_AI Innovation challenge.xlsx"]][["contact_person"]] %>%
  janitor::clean_names() %>%
  dplyr::mutate(dplyr::across(dplyr::where(haven::is.labelled), ~ haven::as_factor(.x)
                              ) #converts only labelled columns to factors
                )

df_raw_information_webinar <- df_list[["application_AI Innovation challenge.xlsx"]][["information_webinar"]] %>%
  janitor::clean_names() %>%
  dplyr::mutate(dplyr::across(dplyr::where(haven::is.labelled), ~ haven::as_factor(.x)
                              ) #converts only labelled columns to factors
                )

### Capacity Builing
df_raw_capacity_building <- df_list[["Capacity Building Needs Assessment Survey (Responses).xlsx"]][["Form Responses 1"]] %>%
  janitor::clean_names() %>%
  dplyr::mutate(dplyr::across(dplyr::where(haven::is.labelled), ~ haven::as_factor(.x)
                              ) #converts only labelled columns to factors
                )

### Selection Criteia

df_raw_initial_criteria <- df_list[["application_selection_SOI_Rubric_Evaluation.xlsx"]][["final_application"]] %>%
  janitor::clean_names() %>%
  dplyr::mutate(dplyr::across(dplyr::where(haven::is.labelled), ~ haven::as_factor(.x)
                              ) #converts only labelled columns to factors
                ) %>%
  dplyr::filter(status_initial_screening %in% c("Passed", "Failed")
                )

df_raw_soi_criteria <- df_list[["application_selection_SOI_Rubric_Evaluation.xlsx"]][["statement_of_interest"]] %>%
  janitor::clean_names() %>%
  dplyr::mutate(dplyr::across(dplyr::where(haven::is.labelled), ~ haven::as_factor(.x)
                              ) #converts only labelled columns to factors
                )

df_raw_soi_scoring_scale <- df_list[["application_selection_SOI_Rubric_Evaluation.xlsx"]][["soi_scoring_scale"]]


df_raw_soi_evaluation_criteria <- df_list[["application_selection_SOI_Rubric_Evaluation.xlsx"]][["soi_evaluation_criteria"]]


df_raw_soi_weight_scoring_summary <- df_list[["application_selection_SOI_Rubric_Evaluation.xlsx"]][["soi_weight_scoring_summary"]]


df_raw_soi_reviewers <- df_list[["application_selection_SOI_Rubric_Evaluation.xlsx"]][["soi_reviewers"]] %>%
  tidyr::drop_na(person)


df_raw_soi_final_teams <- df_list[["application_selection_SOI_Rubric_Evaluation.xlsx"]][["confirmed_teams"]] 

### Evaluation

df_raw_submission_teams <- df_list[["submission_prototype.xlsx"]][["confirmed_teams"]]

df_raw_evaluators_list <- df_list[["submission_prototype.xlsx"]][["evaluators_list"]]

df_raw_evaluation_scores <- df_list[["submission_prototype.xlsx"]][["evaluation_scores"]]

df_raw_final_evaluation_list <- df_list[["submission_prototype.xlsx"]][["final_evaluation_list"]]

df_raw_evaluation_scoring_scale <- df_list[["submission_prototype.xlsx"]][["evaluation_scoring_scale"]]

df_raw_evaluation_criteria <- df_list[["submission_prototype.xlsx"]][["evaluation_criteria"]]

df_raw_evaluation_weight_scoring <- df_list[["submission_prototype.xlsx"]][["evaluation_weight_scoring"]]

df_raw_submission_prototype_criteria <- df_list[["submission_prototype.xlsx"]][["submission_prototype_comments"]]

## creating data dictionary

raw_attribute_application <- base::as.data.frame(labelled::look_for(df_raw_application, labels = TRUE, values = TRUE))

raw_attribute_information_webinar <- base::as.data.frame(labelled::generate_dictionary(df_raw_information_webinar
                                                                                       , labels = TRUE
                                                                                       , values = TRUE
                                                                                       )
                                                         )

raw_attribute_capacity_building <- base::as.data.frame(labelled::look_for(df_raw_capacity_building
                                                                          , labels = TRUE
                                                                          , values = TRUE
                                                                          )
                                                       )


## Save raw dictionary

writexl::write_xlsx(list( application_attribute = raw_attribute_application
                          , information_webinar_attribute = raw_attribute_information_webinar
                          , capacity_building_attribute = raw_attribute_capacity_building
                          ),
                    path = base::file.path(output_Dir, paste0("raw_attributes_dictionary.xlsx") )
                    )
