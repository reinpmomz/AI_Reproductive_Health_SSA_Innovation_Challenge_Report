library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(labelled)
library(tidyr)
library(tibble) 
library(forcats) 

## Clean dataset 

df_clean_information_webinar <- df_raw_information_webinar %>%
  dplyr::rename(any_of(new_var_names[["information_webinar_rename_vars_df"]]) #rename variable names
                ) %>%
  dplyr::filter(category %in% c("Participant")) %>%
  dplyr::filter(application_done %in% c("no")) %>%
  labelled::set_variable_labels(!!!new_labels[["information_webinar_rename_vars_df"]][names(new_labels[["information_webinar_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
                                )

  
