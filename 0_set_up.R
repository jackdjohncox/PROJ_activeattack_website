#install packages
library(pacman)
p_load(bslib,
       DT,
       rio,
       shiny,
       shinylive,
       tidyverse)

#import data
if(!file.exists("data/aa_data_clean_nogeo_2023.rds")) {
  system("cp ../DATA_aa/DATA_clean/events/2023/aa_data_clean_nogeo_2023.rds data/.")
}





