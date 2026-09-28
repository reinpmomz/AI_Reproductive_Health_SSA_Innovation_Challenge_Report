library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(labelled)
library(tidyr)
library(tibble) 
library(forcats) 

## Clean dataset - All applications

df_clean_application <- df_raw_application %>%
  dplyr::rename(any_of(new_var_names[["application_rename_vars_df"]]) #rename variable names
                ) %>%
  dplyr::mutate(country_of_citizenship = stringr::str_trim(country_of_citizenship)
                , country_of_citizenship = stringr::str_to_title(country_of_citizenship)
                , email_address = stringr::str_to_lower(email_address)
                , email_address = stringr::str_trim(email_address)
                , team_contact_person_name = stringr::str_to_upper(team_contact_person_name)
                , team_contact_person_name = stringr::str_squish(team_contact_person_name)
                , application_date = as.Date(lubridate::mdy_hms(application_date))
                , capacity_building_email_date = as.Date(lubridate::mdy_hm(capacity_building_email_date))
                , gender = stringr::str_trim(gender)
                , across(c(gender, country_of_citizenship, team, registered_webinar,innovation_challenge_track,
                           capacity_building_email_sent, capacity_building_email_received), ~as.factor(.x)
                         )
                , country_of_citizenship = forcats::fct_collapse(country_of_citizenship
                                                                , "Benin" = c("Bénin", "Beninese") # collapse levels
                                                                , "Cameroon" = c("Cameroun") # collapse levels
                                                                , "Democratic Republic of Congo" = c("Drc"
                                                                                                     , "Republique Democratique Du Congo"
                                                                                                     , "Democratic Republic Of Congo"
                                                                                                     , "République Démocratique Du Congo"
                                                                                                     , "The Democratic Republic Of Congo"
                                                                                                     ) # collapse levels
                                                                , "Ethiopia" = c("Ethiopian") # collapse levels
                                                                , "Nigeria" = c("Nigerian") # collapse levels
                                                                , "Kenya" = c("Kenyan") # collapse levels
                                                                , "South Africa" = c("South African") # collapse levels
                                                                , "Chad" = c("Tchad") # collapse levels
                                                                , "Tanzania" = c("Tanzanian"
                                                                                 , "Tanzania, United Republic Of"
                                                                                 , "India (Working As Assistant Professor At Iitm Zanzibar, Tanzania)"
                                                                                 ) # collapse levels
                                                                , "Uganda" = c("Ugandan") # collapse levels
                                                                )
                , country_of_citizenship = as.factor(as.character(country_of_citizenship))
                , innovation_challenge_track = forcats::fct_recode(innovation_challenge_track
                                                                   , "Track I: Access to Comprehensive\n Early Pregnancy Loss Care" = "Track I: Access to Comprehensive Early Pregnancy Loss Care"
                                                                   , "Track II: Empowering Informed\n Decision-Making for Contraception" = "Track II: Empowering Informed Decision-Making for Contraception"
                                                                   )
                ) %>%
  dplyr::group_by(application_no) %>%
  dplyr::add_count(application_date, name = "no_of_team_members_cat") %>%
  dplyr::add_count(application_date, name = "no_of_team_members") %>%
  dplyr::add_count(application_date, wt = (gender == "Female"), name = "no_of_females") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(no_of_team_members_cat = as.factor(no_of_team_members_cat)
                , region = forcats::fct_recode(country_of_citizenship, !!!new_african_regions)
                , language = forcats::fct_recode(country_of_citizenship, !!!new_language)
                ) %>%
  labelled::set_variable_labels(no_of_team_members = 'Total number of team members'
                                , no_of_team_members_cat = 'Total Number Team members (Grouped)'
                                , no_of_females = "Total number of females in team"
                                , region = "Regions of Africa"
                                , language = "Language of communication"
                                ) %>% 
  labelled::set_variable_labels(!!!new_labels[["application_rename_vars_df"]][names(new_labels[["application_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
                                )

## Clean dataset - All unique applications
df_clean_application_unique <- df_clean_application %>%
  dplyr::filter(team %in% c("lead")
                ) %>%
  dplyr::mutate(across(where(is.factor),  ~forcats::fct_drop(.x )
                       ) #drop unused factor levels
                )

## Clean dataset - All applications less excluded duplicate applications/non-african countries
df_clean_application_final <- df_clean_application %>%
  dplyr::filter(is.na(application_remarks)) %>%
  dplyr::mutate(across(where(is.factor),  ~forcats::fct_drop(.x )
                       ) #drop unused factor levels
                )

## Clean dataset - Final unique applications
df_clean_application_unique_final <- df_clean_application_final %>%
  dplyr::filter(team %in% c("lead")
                ) %>%
  dplyr::mutate(across(where(is.factor),  ~forcats::fct_drop(.x )
                       ) #drop unused factor levels
                )
  

### Saving the data
writexl::write_xlsx(list(clean_application_final = df_clean_application_final %>%
                           dplyr::select(application_no, application_date, gender, country_of_citizenship,
                                         application_remarks, team, discipline, innovation_challenge_track,
                                         no_of_team_members, no_of_females, region)
                         , clean_application_unique_final = df_clean_application_unique_final %>%
                           dplyr::select(application_no, application_date, country_of_citizenship,
                                         application_remarks, innovation_challenge_track,
                                         no_of_team_members, no_of_females, region)
                         ),
                    path = base::file.path(output_Dir, paste0("application_data_clean_final.xlsx") )
                    )
