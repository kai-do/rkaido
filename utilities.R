library(devtools)
library(roxygen2)

devtools::create("tools")

min_max <- function(x, ...) {
  return((x - min(x, ...)) /(max(x, ...) - min(x, ...)))
}

min_max_between <- function(x, min = NULL, max = NULL) {
  if (is.null(min) || is.null(max)) {
    min <- min(x, ...)
    max <- max(x, ...)
  }
  return((x - min) /(max - min))
}

sd_pop <- function(x) {
  sd(x)*sqrt((length(x)-1)/length(x))
}

norm_prob <- function(n) {
  rnorm_data <- min_max(rnorm(n))
  abs(rnorm_data / sum(rnorm_data))
}

clean_columns <- function(list) {
  tolower(str_replace_all(list, "[.\\- ]+", "_"))
}

min_max_between <- function(x, min, max) {
  f <- function(x, min, max) x*(min(x)/max(x))*(max/min)
  sapply(x, f)
}


install_packages <- function(r, py){
  r_inst <- c()
  
  if(length(r) > 0) {
    for(i in r){
      if(!require(i, character.only = TRUE)){
        install.packages(i , dependencies = TRUE)
        require(i , character.only = TRUE)
        r_inst <- c(r_inst, i)
        
      } else {
        require(i , character.only = TRUE)
      }
    }
    cat("\n### R Packages #######################################################\n")
    if (!is.null(r_inst)) cat("\nR Packages Installed: ", paste0(r_inst, collapse = ", "), "\n")
    if (!is.null(r)) cat("\nR Packages Loaded: ", paste0(r, collapse = ", "), "\n\n")
  }
  
  i <- NULL
  if(!is.null(py)) {
    cat("\n### Python Packages ##################################################\n")
    cat("\nAttemping Install on Python Packages: ", paste0(py, collapse = ", "), "...\n\n")
    for(i in py){
      py_install(i)
    }
    
  }
}


title_columns <- function(list, capitalize_unknown = FALSE) {
  clean_list <- str_replace_all(list, "[._]+", " ")
  cased_list <- c()
  if(capitalize_unknown) {
    for(i in 1:length(clean_list)) {
      split <- str_split(clean_list[i], " ", simplify = TRUE)
      for (j in 1:length(split)) {
        if(nrow(define(split[j])) > 0) {
          split[j] <- str_to_title(split[j])
        } else {
          split[j] <- str_to_upper(split[j])
        }
      }
      cased_list[i] <- paste(split, collapse = " ")
    }
    return(paste(cased_list))
  } else {
    return(str_to_title(clean_list))
  }
}

sum_and_proportion_of_total <- function(x, total) {
  sum_x <- sum(x, na.rm = TRUE)
  prop_x <- format(round(sum(x, na.rm = TRUE)/total, digits = 4), scientific = FALSE)
  if(prop_x == 0) {
    paste0(sum_x, " (<", prop_x, ")", collapse = "")
  } else {
    paste0(sum_x, " (", prop_x, ")", collapse = "")
  }
}

format_int_for_text <- function(x) {
  format(round(as.integer(max(x)), 1), nsmall=0, big.mark=",")
}

format_freq_for_text <- function(x) {
  round(as.numeric(x), digits = 4)
}

format_percent_for_text <- function(x) {
  label_percent(accuracy = 0.01)(as.numeric(x))
}

simplify_df <- function(df, x_var, group_name = NULL) {
  if(is.null(group_name)) { 
    df <- df %>%
      select(.data[[x_var]])
  } else {
    df <- df %>%
      select(.data[[group_name]],
             .data[[x_var]])
  } 
  return(df)
}


create_stat_df <- function(df, x_var, group_name = NULL, include_combined_stats = TRUE, quantile_type = 1) {
  
  df <- simplify_df(df = df, x_var = x_var, group_name = group_name)
  
  if(is.null(group_name) | include_combined_stats) {
    combined_stats_df <- data.frame(group = paste0("all_groups"),
                                    df %>%
                                      summarise(sd_s = sd(.data[[x_var]], na.rm = TRUE),
                                                sd_p = sd_pop(.data[[x_var]]),
                                                mean = mean(.data[[x_var]], na.rm = TRUE),
                                                minimum = min(.data[[x_var]], na.rm = TRUE),
                                                q1 = quantile(.data[[x_var]], 0.25, type = quantile_type, na.rm = TRUE),
                                                median = median(.data[[x_var]], na.rm = TRUE),
                                                q3 = quantile(.data[[x_var]], 0.75, type = quantile_type, na.rm = TRUE),
                                                iqr = q3 - q1,
                                                lqb = q1 - 1.5 * iqr,
                                                uqb = q3 + 1.5 * iqr,
                                                maximum = max(.data[[x_var]], na.rm = TRUE),
                                                n_x = n(),
                                                sum_x = sum(.data[[x_var]], na.rm = TRUE),
                                                n_outliers = sum(case_when(between(.data[[x_var]], lqb, uqb) ~ 0,
                                                                           TRUE ~ 1), na.rm = TRUE)
                                      )
    ) 
  }
  
  if(is.null(group_name)) {
    return(combined_stats_df)
  }
  
  if(!is.null(group_name)) {
    stat_df <- df %>%
      group_by(.data[[group_name]]) %>%
      summarise(sd_s = sd(.data[[x_var]], na.rm = TRUE),
                sd_p = sd_pop(.data[[x_var]]),
                mean = mean(.data[[x_var]], na.rm = TRUE),
                minimum = min(.data[[x_var]], na.rm = TRUE),
                q1 = quantile(.data[[x_var]], 0.25, type = quantile_type, na.rm = TRUE),
                median = median(.data[[x_var]], na.rm = TRUE),
                q3 = quantile(.data[[x_var]], 0.75, type = quantile_type, na.rm = TRUE),
                iqr = q3 - q1,
                lqb = q1 - 1.5 * iqr,
                uqb = q3 + 1.5 * iqr,
                maximum = max(.data[[x_var]], na.rm = TRUE),
                n_x = n(),
                sum_x = sum(.data[[x_var]], na.rm = TRUE),
                n_outliers = sum(case_when(between(.data[[x_var]], lqb, uqb) ~ 0,
                                           TRUE ~ 1), na.rm = TRUE)
      )
  }
  
  if(!is.null(group_name) & !include_combined_stats) {
    return(stat_df) 
  }
  
  if(!is.null(group_name) & include_combined_stats) {
    combined_stats_df <- combined_stats_df %>%
      rename_with(~group_name, group) 
    stat_df <- bind_rows(stat_df, combined_stats_df)
    return(stat_df)
  }
}


create_binned_df <- function(df, x_var, group_name = NULL, bin_width = 5, use_qb_cutoff = FALSE, quantile_type = 1) {
  
  df <- simplify_df(df = df, x_var = x_var, group_name = group_name)
  
  stat_df <- create_stat_df(df, x_var = x_var, group_name = group_name, include_combined_stats = FALSE, quantile_type = quantile_type)
  
  if(use_qb_cutoff) {
    if(is.null(group_name)) {
      bin_df <- df %>% filter(between(.data[[x_var]], plyr::round_any(min(stat_df$lqb), bin_width, f = floor), plyr::round_any(max(stat_df$uqb), bin_width, f = ceiling)))
    } else {
      bin_df <- inner_join(stat_df, df) %>% filter(between(.data[[x_var]], lqb, uqb)) 
    }
  } else {
    bin_df <- df
  }
  
  
  
  bin_df <- bin_df %>%
    mutate(bin = cut(x = .data[[x_var]], 
                     breaks = seq(min(.data[[x_var]]), max(.data[[x_var]]) + bin_width, bin_width), 
                     #labels = paste0(seq(min(.data[[x_var]]), max(.data[[x_var]]), bin_width), "-", 
                     #                (seq(min(.data[[x_var]]), max(.data[[x_var]]), bin_width)) + bin_width - 1), 
                     right = FALSE))
  
  if(is.null(group_name)) {
    bin_df <- bin_df %>%
      group_by(bin) %>%
      summarize(count = n())
  } else {
    bin_df <- bin_df %>%
      group_by(bin, .data[[group_name]]) %>%
      summarize(count = n())
  }
  return(bin_df)
}


#ggplot(national_fire_departments_df %>% filter(dept_type == "Volunteer", group_name = NULL, between(n_ff, volunteer_lqb, volunteer_uqb)), aes(n_ff, n_ff)) 


plot_discrete_histogram <- function(df, x_var, group_name = NULL, bin_width = 5, use_qb_cutoff = FALSE, quantile_type = 1) {
  
  df <- simplify_df(df = df, x_var = x_var, group_name = group_name)
  
  if(bin_width == 1) {
    df <- df
    group_classes <- FALSE
  } else if(bin_width > 1) {
    bin_df <- create_binned_df(df = df, x_var = x_var, group_name = group_name, bin_width = bin_width, use_qb_cutoff = use_qb_cutoff, quantile_type = quantile_type)
    #group_classes <- df %>% data.frame() %>% select(.data[[group_name]]) %>% distinct()
  } else {
    stop("bin_width must be an integer greater than 0") 
  }
  
  if(!is.null(group_name)) {
    group_classes <- bin_df %>% data.frame() %>% transmute(group = as.character(.data[[group_name]])) %>% unique() %>% c()
    group_classes <- group_classes$group
  } else {
    group_classes <- c(1) 
  }
  
  plots <- list()
  
  for(i in 1:length(group_classes)) {
    
    if(!is.null(group_name)) {
      plot_df <- bin_df %>% filter(.data[[group_name]] == group_classes[i])
      summary_stats_df <- create_stat_df(df, x_var = x_var, group_name = group_name, include_combined_stats = FALSE) %>% filter(.data[[group_name]] == group_classes[i])
    } else {
      plot_df <- bin_df
      summary_stats_df <- create_stat_df(df, x_var = x_var, group_name = group_name, include_combined_stats = FALSE)
    }
    
    min <- summary_stats_df %>% select(minimum) %>% format_freq_for_text()
    lqb <- summary_stats_df %>% select(lqb) %>% format_freq_for_text()
    uqb <- summary_stats_df %>% select(uqb) %>% format_freq_for_text()
    max <- summary_stats_df %>% select(maximum) %>% format_freq_for_text()
    n_outliers <- summary_stats_df %>% select(n_outliers) %>% format_int_for_text()
    n_count <- summary_stats_df %>% select(n_x) %>% as.integer()
    max_group_count <- plot_df %>% data.frame() %>% select(count) %>% max()
    bin_count <- plot_df %>% nrow()
    
    if(use_qb_cutoff) {
      min_x_plot_scale <- max(c(min, lqb))
      max_x_plot_scale <- min(c(max, uqb))
    } else {
      min_x_plot_scale <- min
      max_x_plot_scale <- max
    }
    
    scale_to_x_axis <- function(x) {
      rescale(x, from = c(min_x_plot_scale, max_x_plot_scale), to = c(0.5, bin_count + 0.5))
    }
    
    x_axis_scale_factor <- max_x_plot_scale/bin_count
    
    plots[[i]] <- ggplot(plot_df, aes(bin, count)) +
      geom_bar(stat = "identity", width = 1, alpha = 0.5) +
      {if(!use_qb_cutoff) geom_vline(xintercept = scale_to_x_axis(lqb), color = "blue", alpha = 0.5)} +
      {if(!use_qb_cutoff) geom_vline(xintercept = scale_to_x_axis(uqb), color = "blue", alpha = 0.5)} +
      {if(!use_qb_cutoff) geom_label_repel(data = summary_stats_df, aes(label = paste0("LQB\n", round(lqb, digits = 1)), x = scale_to_x_axis(lqb), y = max_group_count), color = "black", alpha = 0.5, box.padding = 0.5)} +
      {if(!use_qb_cutoff) geom_label_repel(data = summary_stats_df, aes(label = paste0("UQB\n", round(uqb, digits = 1)), x = scale_to_x_axis(uqb), y = max_group_count), color = "black", alpha = 0.5, box.padding = 0.5)} +
      geom_vline(data = summary_stats_df, aes(xintercept = scale_to_x_axis(q1)), color = "blue", alpha = 0.5) +
      geom_vline(data = summary_stats_df, aes(xintercept = scale_to_x_axis(median)), color = "yellow", alpha = 0.5) +
      geom_vline(data = summary_stats_df, aes(xintercept = scale_to_x_axis(mean)), color = "orange", alpha = 0.5) +
      geom_vline(data = summary_stats_df, aes(xintercept = scale_to_x_axis(q3)), color = "blue", alpha = 0.5) +
      geom_label_repel(data = summary_stats_df,          
                       aes(label = paste0("Q1\n", round(q1, digits = 1)), x = scale_to_x_axis(q1), y = max_group_count/2),          
                       color = "black", alpha = 0.5, box.padding = 0.5) + 
      geom_label_repel(data = summary_stats_df,          
                       aes(label = paste0("Median\n", round(median, digits = 1)), x = scale_to_x_axis(median), y = max_group_count/2),          
                       color = "black", alpha = 0.5, box.padding = 0.5) +
      geom_label_repel(data = summary_stats_df,          
                       aes(label = paste0("Mean\n", round(mean, digits = 1)), x = scale_to_x_axis(mean), y = max_group_count/2),          
                       color = "black", alpha = 0.5, box.padding = 0.5) +
      geom_label_repel(data = summary_stats_df,          
                       aes(label = paste0("Q3\n", round(q3, digits = 1)), x = scale_to_x_axis(q3), y = max_group_count/2),          
                       color = "black", alpha = 0.5, box.padding = 0.5) +
      #scale_y_continuous(breaks = seq(0, max_group_count, plyr::round_any(max_group_count %/% 10, 10, f = ceiling)), 
      #                   limits = c(0, max_group_count)) +
      {if(use_qb_cutoff) labs(subtitle = paste0("Range between LQB = ", lqb, " and UQB = ", uqb, "\nOutliers not shown of n (", n_count, ") = ", n_outliers))}
  }
  
  if(!is.null(group_name)) {
    names(plots) <- group_classes
    return(plots)
  } else {
    return(plots[[1]])
  }
}
