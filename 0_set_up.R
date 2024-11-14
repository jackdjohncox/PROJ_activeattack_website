#install packages
library(pacman)
p_load(bslib,
       DT,
       rio,
       scales,
       shiny,
       shinylive,
       tidyverse)

#=import data==================================================================

#long + geo
if(!file.exists("data/aa_data_long_2023.rds")) {
  system("cp ../DATA_aa/DATA_clean/events/2023/aa_data_long_2023.rds data/.")
}

#long + no geo

if(!file.exists("data/aa_data_long_nogeo_2023.rds")) {
  system("cp ../DATA_aa/DATA_clean/events/2023/aa_data_long_nogeo_2023.rds data/.")
}

#wide + geo

if(!file.exists("data/aa_data_wide_2023.rds")) {
  system("cp ../DATA_aa/DATA_clean/events/2023/aa_data_wide_2023.rds data/.")
}

#wide + no geo

if(!file.exists("data/aa_data_wide_nogeo_2023.rds")) {
  system("cp ../DATA_aa/DATA_clean/events/2023/aa_data_wide_nogeo_2023.rds data/.")
}

#=function to format numbers inline============================================
format_num <- scales::label_comma(big.mark = ",")



