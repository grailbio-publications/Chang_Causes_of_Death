#coordination script
library(tidyverse)
library(readxl)

date_code<-"20230621"

#excel to tsv
source("scripts/01_read_data_tables.R")
#clean up data
source("scripts/02_clean_cod_data.R")
#generate synthetic data approximating input
source("scripts/03_generate_synthetic_cod.R")
