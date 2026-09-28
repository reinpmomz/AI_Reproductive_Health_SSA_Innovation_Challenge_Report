library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(labelled)
library(tidyr)
library(tibble) 
library(forcats) 

## Clean dataset 

df_clean_capacity_building <- df_raw_capacity_building %>%
  dplyr::rename(any_of(new_var_names[["capacity_building_rename_vars_df"]]) #rename variable names
                ) %>%
  dplyr::mutate(country_of_citizenship = stringr::str_trim(country_of_citizenship)
                , country_of_citizenship = stringr::str_to_title(country_of_citizenship)
                , email_address = stringr::str_to_lower(email_address)
                , email_address = stringr::str_trim(email_address)
                , full_name = stringr::str_to_upper(full_name)
                , full_name = stringr::str_squish(full_name)
                , across(c(preferred_language_of_communication, gender, country_of_citizenship
                           , highest_level_of_education), ~as.factor(.x)
                         )
                , gender = forcats::fct_collapse(gender
                                                , "Female" = c("Féminin", "female", "FEMALE", "F", "Woman"
                                                               , "Feamle", "Felinin", "Feminin", "FEMME"
                                                               , "Femme") # collapse levels
                                                , "Male" = c("MALE", "Masculin", "Make", "Homme", "M", "Shadrak"
                                                             , "male", "Masculino") # collapse levels
                                                )
                , country_of_citizenship = forcats::fct_collapse(country_of_citizenship
                                                                 , "Benin" = c("Bénin", "Beninesse") # collapse levels
                                                                 , "Burkina Faso" = c("Burkinabé") # collapse levels
                                                                 , "Cameroon" = c("Cameroun", "Cameroonian"
                                                                                  , "Cmaeroon") # collapse levels
                                                                 , "Chad" = c("Tchad") # collapse levels
                                                                 , "Democratic Republic of Congo" = c("Democratic Republic Of The Congo"
                                                                                                      , "Democratic Republic Of Congo"
                                                                                                      , "République Démocratique Du Congo"
                                                                                                      , "Rdc") # collapse levels
                                                                 , "Ethiopia" = c("Ethiopian") # collapse levels
                                                                 , "Ghana" = c("Ghanaian") # collapse levels
                                                                 , "Guinea" = c("Guinée", "République De Guinée") # collapse levels
                                                                 , "Nigeria" = c("Nigerian") # collapse levels
                                                                 , "Kenya" = c("Kenyan", "Kenha", "Kenga") # collapse levels
                                                                 , "Malawi" = c("Malawian", "Malama") # collapse levels
                                                                 , "Mozambique" = c("Moçambique") # collapse levels
                                                                 , "Somalia" = c("Somali", "Somaliland") # collapse levels
                                                                 , "Tanzania" = c("Tanzanian", "Tanzania, United Republic Of"
                                                                                  , "United Republic Of Tanzania") # collapse levels
                                                                 , "Uganda" = c("Ugandan") # collapse levels
                                                                 , "Zimbabwe" = c("Zimbabwean") # collapse levels
                                                                 )
                , highest_level_of_education = forcats::fct_collapse(highest_level_of_education
                                                                     , "Post graduate Diploma" = c("Diploma", "Ordinary Diploma"
                                                                                                   , "ORDINARY DIPLOMA"
                                                                                                   , "National Diploma"
                                                                                                   , "HND"
                                                                                                   , "Diploma in IT / Cybersecurity") # collapse levels
                                                                     , "High School" = c("High school cert", "High School Certificate"
                                                                                         , "High School Diploma"
                                                                                         , "Senior Secondary School") # collapse levels
                                                                     , "A levels" = c("Advance level"
                                                                                      , "Advanced Level"
                                                                                      , "Advanced level and Ordinary level") # collapse levels
                                                                     , "Bachelors" = c("Doctor of medicine", "Undergraduate"
                                                                                       , "Second-year bachelor's student"
                                                                                       , "Bachelor of Medicine and Surgery"
                                                                                       , "Currently pursuing Bachelor's degree (3rd year)"
                                                                                       , "Doctor of Medicine"
                                                                                       , "Doctor of Medicine (MD)"
                                                                                       , "Enrolled at a university"
                                                                                       , "Registered Nurse"
                                                                                       , "MBChB student"
                                                                                       , "Medical Student (5th Year)"
                                                                                       , "Medical"
                                                                                       , "5e année de Médecine"
                                                                                       , "Baccalauréat"
                                                                                       , "Getting my bachelors"
                                                                                       , "Students of a bachelor's degree") # collapse levels
                                                                     , "Masters" = c("Masters of Nursing Science ( MNSc.) in view"
                                                                                 ) # collapse levels
                                                                     , "PhD" = c("Fellowship", "PhD Candidate in Public Health"
                                                                                 , "Fellowship of the Medical College in Public Health"
                                                                                 ) # collapse levels
                                                                     , "Professional Certificate" = c("12 Grade with professional certifications"
                                                                                                      , "Cert", "Certificate" 
                                                                                                      , "Certificate in midwifery"
                                                                                                      , "FWACP"
                                                                                                      , "NCICT"
                                                                                                      , "OND"
                                                                                                      , "Certificate in software engineering") # collapse levels
                                                                     )
                , age = gsub("May 19th 1998", "", age) # ikugbayigbej@yahoo.com
                , age = readr::parse_number(age)
                , age = if_else(age %in% c(2), NA, age) #Naemecynthia@gmail.com
                , age_group = if_else(age < 25 , "18-24",
                                      if_else(age < 35 , "25-34",
                                              if_else(age < 50 , "35-49",
                                                      if_else(age < 65 , "50-64", "65 and above"
                                                              )
                                                      )
                                              )
                                      )
                , age_group = factor(age_group, levels = c( "18-24", "25-34", "35-49", "50-64", "65 and above")
                                              ) #gsub("N:A|N/A", NA, organization_institution)
                , region = forcats::fct_recode(country_of_citizenship, !!!new_african_regions)
                , organization_institution = ifelse(organization_institution == "N:A", NA_character_,
                                                    ifelse(organization_institution == "N/A", NA_character_,
                                                           ifelse(organization_institution == "NA", NA_character_,
                                                                  ifelse(organization_institution == "Not applicable", NA_character_,
                                                                         organization_institution
                                                                         )
                                                                  )
                                                           )
                                                    )
                , current_occupation = stringr::str_to_upper(current_occupation)
                , current_occupation = forcats::fct_recode(current_occupation, !!!new_occupation)
                , primary_area_of_expertise = ifelse( primary_area_of_expertise == "Data science /AI /Machine learning, Product management,"
                                                      , "Data science /AI /Machine learning, Product management", primary_area_of_expertise)
                , primary_area_of_expertise = ifelse( primary_area_of_expertise == "Frontend Development", "UX/UI design", 
                                                      ifelse( primary_area_of_expertise == "Medical Doctor", "Public health", 
                                                              ifelse( primary_area_of_expertise == "Data collection, analysis and presentation", "Research", 
                                                                      ifelse( primary_area_of_expertise == "Information Technology", "Software development",
                                                                              primary_area_of_expertise
                                                                              )
                                                                      )
                                                              )
                                                      )
                , primary_area_of_expertise = gsub("Maternal, Sexual and Reproductive Health \\(MSRH\\)"
                                                   , "Maternal,Sexual and Reproductive Health (MSRH)", primary_area_of_expertise)
                , primary_area_of_expertise = gsub(", Actuarial science|, Frontend Development|, 3D design and printing"
                                                   , "", primary_area_of_expertise)
                , primary_area_of_expertise = gsub(", Procurement|, Medical engineering|, Obstetric sciences"
                                                   , "", primary_area_of_expertise)
                , primary_area_of_expertise = gsub(", Health Data|, Information Technology|, General Doctor"
                                                   , "", primary_area_of_expertise) 
                , primary_area_of_expertise = gsub(", Bioinformatics|, Biomedical engineering|, Data and data systems"
                                                   , "", primary_area_of_expertise)
                , primary_area_of_expertise = gsub(", Primary Health Care|, Immunology-Virology|, Biomedical Engineering"
                                                   , "", primary_area_of_expertise) 
                , primary_area_of_expertise = gsub(", Education Technology|, Mathematical Modeling|, Networking"
                                                   , "", primary_area_of_expertise)
                , primary_area_of_expertise = gsub(", Medical Doctor|, Busines management|, General Nursing"
                                                   , "", primary_area_of_expertise) 
                , primary_area_of_expertise = gsub(", Gender-Based Violence \\(GBV\\), Reproductive Justice, Community Health"
                                                   , "", primary_area_of_expertise)
                , primary_area_of_expertise = gsub(", Data collection, analysis and presentation|, Clinical Data Management, Project Management"
                                                   , "", primary_area_of_expertise) 
                , primary_area_of_expertise = gsub(", Statistical and Mathematical modelling|, Digital Marketing / Communications / Entrepreneurship"
                                                   , "", primary_area_of_expertise) 
                , primary_area_of_expertise = gsub(", Proect management monitoring and evaluation|, Elaboration de stratégie de marketing"
                                                   , "", primary_area_of_expertise)
                , primary_area_of_expertise = gsub(", HealthTech Product Architecture|, Digital health / Health innovation / No-code automation"
                                                   , "", primary_area_of_expertise) 
                , which_areas_would_you_like_training_support = gsub(", Deployment of solutions in low-connectivity environments \\."
                                                                     , "", which_areas_would_you_like_training_support)
                , which_areas_would_you_like_training_support = gsub(", Team working with people from a different field and also knowing more about contraceptives since it is the topic we selected."
                                                                     , "", which_areas_would_you_like_training_support )
                , which_areas_would_you_like_training_support = gsub(", Marketing, online collaboration|, Project management|, Health education program design"
                                                                     , "", which_areas_would_you_like_training_support )
                , which_areas_would_you_like_training_support = gsub(", Gender Data storytelling|, Mobile deployment|, Data Analysis"
                                                                     , "", which_areas_would_you_like_training_support )
                , which_areas_would_you_like_training_support = gsub(", Regulatory pathways for digital health tools, Implementation in low-resource settings and Data governance and clinical validation"
                                                                     , "", which_areas_would_you_like_training_support )
                , which_areas_would_you_like_training_support = gsub(", Health data literacy|, Securing AI"
                                                                     , "", which_areas_would_you_like_training_support )
                , which_areas_would_you_like_training_support = gsub("Grant writing/fundraising,"
                                                                     , "Grant writing/fundraising", which_areas_would_you_like_training_support )
                , type_of_solution_to_develop = ifelse( type_of_solution_to_develop == "We are yet to finalize on the solution we are going to work. A couple of solutions are still on the table",
                                                        NA_character_, type_of_solution_to_develop)
                , type_of_solution_to_develop = forcats::fct_recode(type_of_solution_to_develop, !!!new_solution)
                ) %>%
  dplyr::filter(!country_of_citizenship %in% c("India", "United States")) %>%
  dplyr::mutate(across(where(is.factor),  ~forcats::fct_drop(.x )
                       ) #drop unused factor levels
                ) %>%
  labelled::set_variable_labels( #creating labels for new variables
    age_group = "Age group (Years)"
    , region = "Regions of Africa"
    ) %>%
  labelled::set_variable_labels(!!!new_labels[["capacity_building_rename_vars_df"]][names(new_labels[["capacity_building_rename_vars_df"]]) %in% names(.)]
                                #labeling variables from data dictionary
                                )

df_clean_capacity_building_final <- multiSeparate(df = df_clean_capacity_building
                                                  , vars = c("primary_area_of_expertise"
                                                             , "which_areas_would_you_like_training_support")
                                                  , pattern = ", "
                                                  , drop_ns = TRUE
                                                  )
