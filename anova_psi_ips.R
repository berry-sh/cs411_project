suppressPackageStartupMessages({
  library(tidyverse) # Give ggplot, read_delim, tidyr, etc.
  library(janitor) # Gives tabyl
  library(gplots) # Gives plotmeans
  library(ggstatsplot) # Gives pretty plotting
  library(car) # Gives Anova
  library(Hmisc) # Gives mean_cl_boot
  library(dplyr) # Gives join, mutate, filter, etc.
  library(gridExtra) # Gives grid.arrange
  library(patchwork) # Gives wrap_plots
})
options(width=200) # set the display width to 200 characters

load_questionnaire_data <- function(file_path) {
  # Read CSV
  df <- read.csv(file_path, check.names = FALSE)
  
  # Drop unwanted columns
  df <- df %>% select(
    -c(
      Surname,
      `First name`,
      State,
      `Started on`,
      Completed,
      `Grade/10.00`,
      `Time taken`,
    )
  )
  
  # Rename columns
  df <- df %>%
    rename(
      email = `Email address`,
      gender = `Response 1`,
      age = `Response 2`,
      academic_level = `Response 3`,
      academic_field = `Response 4`,
      parents_education = `Response 5`,
      sport = `Response 6`
    )
  
  return(df)
}

load_pretest_data <- function(file_path) {
  # Read CSV
  df <- read.csv(file_path, check.names = FALSE)
  
  # Keep and rename specific columns
  df <- df %>%
    select(
      email = `Email address`,
      time_pretest = `Time taken`,
      grade_pretest = `Grade/9.00`
    )
  
  return(df)
}

load_posttest1_data <- function(file_path) {
  # Read CSV
  df <- read.csv(file_path, check.names = FALSE)
  
  # Keep and rename specific columns
  df <- df %>%
    select(
      email = `Email address`,
      time_posttest = `Time taken`,
      grade_posttest = `Grade/9.00`
    )
  
  return(df)
}

load_posttest2_data <- function(file_path) {
  # Read CSV
  df <- read.csv(file_path, check.names = FALSE)
  
  # Drop unwanted columns
  df <- df %>% select(
    -c(
      Surname,
      `First name`,
      State,
      `Started on`,
      Completed,
      `Grade/7.00`,
      `Time taken`
    )
  )
  
  # Rename columns
  df <- df %>%
    rename(
      email = `Email address`,
      comfort_before       = `Response 1`,
      comfort_after        = `Response 2`,
      comfort_explanation  = `Response 3`,
      confidence           = `Response 4`,
      perceived_learning   = `Response 5`,
      learning_explanation = `Response 6`,
      comments             = `Response 7`
    )
  
  return(df)
}

load_psactv_data <- function(file_path) {
  # Read CSV
  df <- read.csv(file_path, check.names = FALSE)
  # Keep and rename specific columns
  df <- df %>%
    select(
      email = `email`,
      correct = `Correct`,
      algorithmic = `Algorithmic`,
      graph_used = `Graph_used`
    )

  return(df)
}

convert_time_to_seconds <- function(time_str) {
  mins <- as.numeric(str_extract(time_str, "\\d+(?=\\s*mins?)"))
  secs <- as.numeric(str_extract(time_str, "\\d+(?=\\s*secs?)"))
  mins[is.na(mins)] <- 0
  secs[is.na(secs)] <- 0
  total_seconds <- mins * 60 + secs
  return(total_seconds)
}


# Calculate the relative learning gain
calculate_rlg <- function(df, pre_col = "grade_pretest", post_col = "grade_posttest", max_score = 9) {
  df = df %>%
    mutate(
      !!pre_col := as.numeric(.data[[pre_col]]),
      !!post_col := as.numeric(.data[[post_col]]),
      rel_learning_gain = round((.data[[post_col]] - .data[[pre_col]]) / (max_score - .data[[pre_col]]), 2)
    )
  return(df)
}


plot_dual_pie_with_shared_legend <- function(data, group_var, target_var, fill_label, title_ips, title_psi) {
  # Ensure column names are quosures for tidy evaluation
  group_var <- enquo(group_var)
  target_var <- enquo(target_var)

  # IPS plot
  plot_ips <- data %>%
    filter(!!group_var == "IPS") %>%
    count(!!target_var) %>%
    mutate(percent = paste0(round(n / sum(n) * 100), "%")) %>%
    ggplot(aes(x = "", y = n, fill = !!target_var)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = percent), position = position_stack(vjust = 0.5), size = 8) +
    labs(title = title_ips, fill = fill_label) +
    theme_void() +
    theme(
      plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 23),
      legend.text = element_text(size = 21)
    )

  # PSI plot (no legend)
  plot_psi <- data %>%
    filter(!!group_var == "PSI") %>%
    count(!!target_var) %>%
    mutate(percent = paste0(round(n / sum(n) * 100), "%")) %>%
    ggplot(aes(x = "", y = n, fill = !!target_var)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = percent), position = position_stack(vjust = 0.5), size = 8) +
    labs(title = title_psi) +
    theme_void() +
    theme(
      plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
      legend.position = "none"
    )

  # Combine
  plot_ips + plot_psi
}


plot_moderator_per_group <- function(data, group_var, control_var, target_var, title, legend_title, pd) {
  group_var <- enquo(group_var)
  control_var <- enquo(control_var)
  target_var <- enquo(target_var)
  data %>%
  filter(!!control_var != "-") %>%
  ggplot(aes(x = !!group_var, y = !!target_var, color = !!control_var, group = !!control_var)) +
    stat_summary(fun = mean, geom = "line", position = pd) +
    stat_summary(fun = mean, geom = "point", position = pd) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = pd) +
    labs(
      x = "Group",
      y = "Relative Learning Gain",
      color = legend_title,
      title = title
    ) + theme(
      plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 23),
      axis.text = element_text(size = 18),
      legend.title = element_text(size = 23),
      legend.text = element_text(size = 21)
    )
}
