library(dplyr)
library(forcats)
library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
library(labelled)
library(stringr)


### Multiple responses separate stack bar graphs
multires_sep_stack_plot <- 
  function(df, variable, reverse_xaxis=FALSE, reverse_fill = FALSE, rotate_axis=TRUE, percent_yaxis=TRUE,
           y_axis_breaks = 10, title_label="", text_label = TRUE, text_angle = 0, text_label_size = 4,
           bar_width = 0.8, legend_label = "Response", expand_yaxis = c(0.01,0.05), nrow_legend = 1, ncol_legend = NULL,
           x_axis_label_wrap_width = 20, title_label_wrap_width = 35, percent_text_accuracy = 0.1) {
    
    dictionary <- as.data.frame(labelled::generate_dictionary(df, labels = TRUE, values = TRUE)
                                ) %>%
      dplyr::rename(name = label)
    
    df <- (df %>%
             dplyr::select(all_of(variable)) %>%
             dplyr::mutate(across(where(is.factor), as.character)) %>% 
             tidyr::pivot_longer(cols = everything(),
                                 names_to = "var",
                                 values_to = "value") %>%
             tidyr::drop_na(value) %>%
             dplyr::left_join(dictionary %>% 
                                dplyr::select(variable, name),
                              by = c("var" = "variable")
                              ) %>%
             dplyr::group_by(name) %>%
             dplyr::mutate(total = n()) %>%
             dplyr::ungroup() %>%
             dplyr::group_by(name, value, total) %>%
             dplyr::summarise(count = n(), .groups = 'drop') %>%
             dplyr::mutate(p = scales::percent(count/total, accuracy = percent_text_accuracy)) %>%
             dplyr::arrange(desc(count), name, value) %>%
             dplyr::mutate(across(c(name, value), ~ forcats::as_factor(.x))
                           )
           )
    
    p <- if (reverse_xaxis == TRUE) {
      if (reverse_fill == TRUE) {
        ggplot(df, aes(x= forcats::fct_rev(name), y=count,
                       fill=forcats::fct_rev(forcats::fct_reorder(value, count)))) +
          guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
      } else {
        ggplot(df, aes(x= forcats::fct_rev(name), y=count,
                       fill=forcats::fct_reorder(value, count))) +
          guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
      }
    } else {
      if (reverse_fill == TRUE) {
        ggplot(df, aes(x= name, y=count, fill=forcats::fct_rev(forcats::fct_reorder(value, count)))) +
          guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
      } else {
        ggplot(df, aes(x= name, y=count, fill=forcats::fct_reorder(value, count))) +
          guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
      }
    }
    
    p1 <- if (rotate_axis == TRUE) { 
      p + coord_flip()
    } else {
      p
    }
    
    p3 <- if (percent_yaxis == TRUE) {
      p2 <- if (text_label == TRUE) {
        p1 +
          geom_bar(position = "fill", stat = "identity",width=bar_width) + 
          geom_text(aes(label= p),
                    position = position_fill(vjust=0.5), color="black", angle = text_angle,
                    size=text_label_size, fontface = "bold") +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
      } else {
        p1 +
          geom_bar(position = "fill", stat = "identity",width=bar_width) + 
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
      }
      p2
    } else {
      p2 <- if (text_label == TRUE) {
        p1 +
          geom_bar(stat = "identity",width=bar_width) + 
          geom_text(aes(label= count),
                    position = position_stack(vjust=0.5), color="black", angle = text_angle,
                    size=text_label_size, fontface = "bold") +
          scale_y_continuous(n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
      } else {
        p1 +
          geom_bar(stat = "identity",width=bar_width) + 
          scale_y_continuous(n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
      }
      p2
      
    }
    
    p4 <- p3 +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
      labs(x=NULL, y=NULL, fill = legend_label, title = stringr::str_wrap(title_label, width = title_label_wrap_width))
    
    print(p4)
  }


### Multiple responses separate simple bar graphs
multires_sep_count_plot <- 
  function(df, variable, reverse_xaxis=TRUE, rotate_axis=TRUE, percent_yaxis=FALSE,
           y_axis_breaks = 10, title_label="", text_label = TRUE, text_label_size = 4, percent_text_accuracy = 0.1,
           expand_yaxis = c(0.01,0.06), x_axis_label_wrap_width = 20, title_label_wrap_width = 35,
           bar_width = 0.8, y_axis_limits = c(NULL, NULL), bar_fill_colour = NULL) {
    df <- (df %>%
             dplyr::select(all_of(variable)) %>%
             dplyr::mutate(across(where(is.factor), as.character)) %>% 
             tidyr::pivot_longer(cols = everything(),
                                 names_to = "variable",
                                 values_to = "name"
                                 ) %>%
             tidyr::drop_na(name) %>%
             dplyr::group_by(variable) %>%
             dplyr::add_count(name = "variable_count") %>%
             dplyr::ungroup() %>%
             dplyr::mutate(total = max(variable_count)) %>%
             dplyr::group_by(name, total) %>%
             dplyr::summarise(count = n(), .groups = 'drop') %>%
             dplyr::mutate(prop = round(count/total, 3),
                           perc = scales::percent(prop, accuracy = percent_text_accuracy)) %>%
             dplyr::arrange(desc(count), name) %>%
             dplyr::mutate(across(c(name), ~ forcats::as_factor(.x))
                           )
           )
    
    p <- if (reverse_xaxis == TRUE) {
      if (percent_yaxis == TRUE) {
        ggplot(df, aes(y = prop, x=forcats::fct_rev(name))) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                             expand = expansion(mult = expand_yaxis))
      } else {
        ggplot(df, aes(y = count, x=forcats::fct_rev(name))) +
          scale_y_continuous( n.breaks = y_axis_breaks, limits = y_axis_limits,
                              expand = expansion(mult = expand_yaxis))
      }
    } else {
      if (percent_yaxis == TRUE) {
        ggplot(df, aes(y = prop, x=name)) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                             expand = expansion(mult = expand_yaxis))
      } else {
        ggplot(df, aes(y = count, x=name)) +
          scale_y_continuous( n.breaks = y_axis_breaks, limits = y_axis_limits,
                              expand = expansion(mult = expand_yaxis))
      }
    }
    
    p1 <- if (is.null(bar_fill_colour)) {
      p +
        geom_bar(position = "dodge", stat = "identity", width=bar_width)
      } else {
        p +
          geom_bar(position = "dodge", stat = "identity", width=bar_width, fill = bar_fill_colour)
        }
    
    p3 <- if (rotate_axis == TRUE) {
      p2 <- if (text_label == TRUE) {
        p1 +
          geom_text(aes(label= paste0(count," (", perc,")" )), vjust=0.4, hjust=-0.05,
                    color="black", size=text_label_size) + 
          coord_flip()
      } else {
        p1 + 
          coord_flip()
      }
      p2
    } else {
      p2 <- if (text_label == TRUE) {
        p1 +
          geom_text(aes(label= paste0(count," (", perc,")" )), vjust=-0.3, hjust=0.5,
                    color="black", size=text_label_size)
      } else {
        p1 
      }
      p2
    }
    
    p4 <- p3 +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
      labs(x=NULL, y=NULL, fill = NULL, title = stringr::str_wrap(title_label, width = title_label_wrap_width)) +
      guides(fill = "none")
    
    print(p4)
    
  }
