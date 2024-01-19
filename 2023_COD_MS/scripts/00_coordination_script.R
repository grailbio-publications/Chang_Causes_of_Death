#final version of COD results
#using latest 2023 draw
library(tidyverse)
library(readxl)
library(patchwork)
library(viridis)

date_code<-"20231103"


#preprocess once from excel
source("scripts/01_read_data_tables.R")

#read in preprocessed data
input_date_code<-"20230623"
all_life_table_data_fixed<-read_tsv(sprintf("generated_data/%s_ms_all_life_table_data.tsv",input_date_code))

#now swing into analyses, similar to ASCO but newly updated
source("scripts/02_all_sites_coarse.R")
source("scripts/03_each_site_coarse.R")
source("scripts/04_all_sites_fine.R")
source("scripts/05_ethnicity_all_sites_coarse.R")
source("scripts/06_each_site_fine.R")
source("scripts/07_sex_coarse.R")
source("scripts/08_age_coarse.R")
source("scripts/09_stage_shift.R")

#now figures - 10 deliberately skipped to organize these
source("scripts/11_Figure_One.R")
source("scripts/12_Figure_Two_and_Three.R")
source("scripts/13_Figure_Four.R")

#supplemental figures: note reordering in the final manuscript
source("scripts/101_supplemental_Figure_One.R") #one
source("scripts/301_sensitivity_to_missing.R") #two
source("scripts/102_supplemental_Figure_Two_Three.R") #three/four
source("scripts/104_supplemental_Figure_Five.R") #five
source("scripts/103_supplemental_Figure_Four.R") #six

#supplemental xlsx with all data tables
source("scripts/201_aggregate_tables.R")