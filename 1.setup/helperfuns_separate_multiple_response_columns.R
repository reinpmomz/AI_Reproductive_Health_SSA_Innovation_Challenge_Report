library(dplyr)
library(tidyr)

## Multiple selects separate
multiSeparate <- function(df, vars, pattern, remove = FALSE, drop_ns = FALSE){
  for (var in vars){
    nselected <- paste0(var, "_nselected")
    df <- (df
           %>% rename(temp_multi = var)
           %>% mutate(temp_nselected = sapply(
             regmatches(temp_multi, gregexpr(pattern, temp_multi))
             , length
           ) + 1
           )
    )
    # Max number of new variables to create
    maxselected <- max(pull(df, temp_nselected))
    
    # Create the new variable
    df <- (df
           %>% separate(
             temp_multi,
             into = paste0(rep(var, maxselected), "_", 1:maxselected),
             sep = pattern,
             remove = FALSE,
             convert = TRUE
           )
           %>% rename(
             !!var := temp_multi,
             !!nselected := temp_nselected
           )
           #%>% separate_("temp_multi"
           #              , c(paste0(rep(var, maxselected), "_", 1:maxselected))
           #              , sep = pattern
           #              , remove = FALSE
           #              , convert = TRUE
           #)
           #%>% rename_(.dots = setNames(c("temp_multi", "temp_nselected"), c(var, nselected)))
    )
    if(drop_ns){
      df <- select(df, -c(grep("_nselected$", colnames(df), value = TRUE)))
    }
    if (remove) {
      df = (df
            %>% select(-all_of(var))
      )
    }
  }
  return(df)
}


