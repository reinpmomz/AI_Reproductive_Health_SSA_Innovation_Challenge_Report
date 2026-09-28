################################################################################
### Restart R
#.rs.restartR()

### Start with a clean environment by removing objects in workspace
rm(list=ls())

### Setting work directory
working_directory <- base::setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#working_directory <- base::setwd(".")

### Load Rdata
Rdata_files <- list.files(path = working_directory, pattern = "*.RData", full.names = T)

if ( length(Rdata_files) >0) {
  invisible(lapply(Rdata_files,load,.GlobalEnv))
} else {
  paste(c(".RData files", "do not exist"), collapse = " ")
}

### Install required packages
source("./1.setup/requirements.R")

### helper/customized functions
source("./1.setup/helperfuns_read_excel_sheets.R")
source("./1.setup/helperfuns_gt_summary_themes.R")
source("./1.setup/helperfuns_table_summary_categorical.R")
source("./1.setup/helperfuns_table_summary_continous.R")
source("./1.setup/helperfuns_ggplot_themes.R")
source("./1.setup/helperfuns_simple_pie_chart.R")
source("./1.setup/helperfuns_simple_plots.R")
source("./1.setup/helperfuns_separate_multiple_response_columns.R")
source("./1.setup/helperfuns_separate_multiple_response_columns_plots.R")
source("./1.setup/helperfuns_stack_plots.R")
source("./1.setup/helperfuns_multiple_response_plots.R")

################################################################################

### Load data 
source("./2.load_data_and_clean/load_data_local.R")

### Load recode file
source("./2.load_data_and_clean/load_recode_file.R")

### Data cleaning
source("./2.load_data_and_clean/cleaning_applications.R")
source("./2.load_data_and_clean/cleaning_information_webinar.R")
source("./2.load_data_and_clean/cleaning_capacity_building.R")
source("./2.load_data_and_clean/cleaning_selection_teams.R")

################################################################################

### Generate Application and selection report
rmarkdown::render(input = "./3.report/application_report.Rmd",
                  output_dir = output_Dir,
                  #output_file = paste0("application_report_",Sys.Date(), ".docx"),
                  output_format = rmarkdown::word_document(toc = TRUE,
                                                           toc_depth = 2,
                                                           number_sections = TRUE,
                                                           fig_width = 8, #10
                                                           fig_height = 6, #8
                                                           reference_docx = "default",
                                                           highlight = "default",
                                                           fig_caption = TRUE,
                                                           keep_md = FALSE
                                                           )
                  )

### Generate Needs assessment report - Selection teams
rmarkdown::render(input = "./3.report/capacity_building_report_selection_teams.Rmd",
                  output_dir = output_Dir,
                  #output_file = paste0("capacity_building_report_selection_teams_",Sys.Date(), ".docx"),
                  output_format = rmarkdown::word_document(toc = TRUE,
                                                           toc_depth = 3,
                                                           number_sections = TRUE,
                                                           fig_width = 8, #10
                                                           fig_height = 6, #8
                                                           reference_docx = "default",
                                                           highlight = "default",
                                                           fig_caption = TRUE,
                                                           keep_md = FALSE
                                                           )
                  )

### Generate Needs assessment report - All
rmarkdown::render(input = "./3.report/capacity_building_report_all.Rmd",
                  output_dir = output_Dir,
                  #output_file = paste0("capacity_building_report_all_",Sys.Date(), ".docx"),
                  output_format = rmarkdown::word_document(toc = TRUE,
                                                           toc_depth = 3,
                                                           number_sections = TRUE,
                                                           fig_width = 8, #10
                                                           fig_height = 6, #8
                                                           reference_docx = "default",
                                                           highlight = "default",
                                                           fig_caption = TRUE,
                                                           keep_md = FALSE
                                                           )
                  )

### Evaluation and Awards
rmarkdown::render(input = "./3.report/evaluation_and_awards.Rmd",
                  output_dir = output_Dir,
                  #output_file = paste0("evaluation_and_awards_",Sys.Date(), ".docx"),
                  output_format = rmarkdown::word_document(toc = TRUE,
                                                           toc_depth = 3,
                                                           number_sections = TRUE,
                                                           fig_width = 8, #10
                                                           fig_height = 6, #8
                                                           reference_docx = "default",
                                                           highlight = "default",
                                                           fig_caption = TRUE,
                                                           keep_md = FALSE
                                                           )
                  )


################################################################################

## Save workspace at the end without working directory path

save(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("working_directory", "mainDir", "subDir_data", "data_Dir",
                                                              "subDir_output", "output_Dir","Rdata_files"
                                                              )],
     file = "hash_hackathon.RData",
     envir = .GlobalEnv #parent.frame()
     )

################################################################################

## Run all files in Rstudio
source("main.R")

################################################################################

