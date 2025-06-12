#install packages
library(pacman)
p_load(bslib,
       DT,
       glue,
       rio,
       scales,
       tidyverse)

version <- "2024"

#=import data & codebook=======================================================

#long + geo
if(!file.exists(glue("data/aa_data_long_{version}.rds"))) {
  system(glue("cp ../DATA_aa/output/events/{version}/aa_data_long_{version}.rds data/."))
}

#long + no geo
if(!file.exists(glue("data/aa_data_long_nogeo_{version}.rds"))) {
  system(glue("cp ../DATA_aa/output/events/{version}/aa_data_long_nogeo_{version}.rds data/."))
}

#wide + geo
if(!file.exists(glue("data/aa_data_wide_{version}.rds"))) {
  system(glue("cp ../DATA_aa/output/events/{version}/aa_data_wide_{version}.rds data/."))
}

#wide + no geo
if(!file.exists(glue("data/aa_data_wide_nogeo_{version}.rds"))) {
  system(glue("cp ../DATA_aa/output/events/{version}/aa_data_wide_nogeo_{version}.rds data/."))
}

#codebook
if(!file.exists(glue("data/aa_data_wide_codebook_{version}.docx"))) {
  system(glue("cp ../DATA_aa/output/codebook/{version}/aa_data_wide_codebook_{version}.docx data/."))
}

#convert data to .csv for download
if(!file.exists(glue("data/aa_data_wide_nogeo_{version}.csv"))) {
  aa_data <- import(glue("data/aa_data_wide_nogeo_{version}.rds", trust=TRUE))
  export(aa_data, glue("data/aa_data_wide_nogeo_{version}.csv"))
  rm(aa_data)
}

#=function to format numbers inline============================================
format_num <- scales::label_comma(big.mark = ",")

#=function to get percentages of specific levels of a variable=================
get_pct <- function(data, var_name, value) {
  data |>
    mutate(target = {{ var_name }} %in% value) |> # Flag rows matching the value/range
    summarise(
      n_total = n(),
      n_target = sum(target),
      pct = round((n_target / n_total) * 100, 1)
    ) |>
    pull(pct)
}

#=function to get the mean of specific levels of a variable====================
get_mean <- function(data, var_name, value) {
  data |>
    filter({{ var_name }} %in% value) |> # Filter to relevant categories or ranges
    count({{ var_name }}) |> # Count rows per value
    summarise(mean_count = round(mean(n, na.rm = TRUE)), 1) |> # Calculate mean of those counts
    pull(mean_count)
}

#=function to get the raw counts of specific levels of a variable==============
get_count <- function(data, var_name, value) {
  data |>
    filter({{ var_name }} %in% value) |> # Filter based on your categories or ranges
    nrow() # Count the number of rows
}

#=function to get the raw counts of specific levels of a variable==============
get_total <- function(data, var_name) {
  data |>
    summarise(across({{ var_name }}, ~ sum(.x, na.rm = TRUE))) |>
    summarise(total = sum(across(everything()))) |>
    pull(total)
}

#function to collapse strings with the appropriate and after the last comma====
oxford_collapse <- function(strings) {
  n <- length(strings)
  
  if (n == 0) {
    return("")
  } else if (n == 1) {
    return(strings)
  } else if (n == 2) {
    return(paste(strings, collapse = ", and "))
  } else {
    return(paste(paste(strings[-n], collapse = ", "), ", and ", strings[n], sep = ""))
  }
}


