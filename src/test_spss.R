library(tidyverse)
library(haven)
library(sjlabelled)
library(labelled) 
library(surveytoolbox) # install with devtools::install_github("martinctc/surveytoolbox")

file_path <- "data/original/General Social Survey, 1991.SAV"
source_data_hv <- haven::read_sav(file_path)
source_data_hv$MARITAL %>% attr('labels')
source_data_hv$MARITAL %>% attr('label')
