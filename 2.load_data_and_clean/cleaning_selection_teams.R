library(dplyr)

## Clean dataset - Final selected team members

df_clean_application_selection_teams <- df_clean_application_final %>%
  dplyr::filter(application_no %in% df_raw_soi_final_teams$application_no) %>%
  dplyr::left_join(df_raw_soi_final_teams %>% dplyr::select(application_no, confirmed)
                   , by = c("application_no")
                   )


## Clean dataset - Capacity building with only Final selected team members

df_clean_capacity_building_final_selection_teams <- df_clean_capacity_building_final %>%
  dplyr::filter(email_address %in% df_clean_application_selection_teams$email_address
                 | full_name %in% df_clean_application_selection_teams$team_contact_person_name
                # "aabiathar@gmail.com"    "ametepig@gmail.com"     "marthafful07@gmail.com"
                )  %>%
  dplyr::mutate(across(where(is.factor),  ~forcats::fct_drop(.x )
                       ) #drop unused factor levels
                )

# draft <- df_clean_capacity_building_final %>%
#   dplyr::filter(email_address %in% df_clean_application_selection_teams$email_address
#                 )
# 
# df_clean_capacity_building_final_selection_teams$email_address[!df_clean_capacity_building_final_selection_teams$email_address %in% draft$email_address]
