#aggregate result tables into single reference xlsx document

library(openxlsx)

#special data frame describing other sheets

my_local_description=tribble(~Sheet,~Group,~Results,~DataType,
                             "overall_coarse","overall","aggregate","raw",
                             "table_coarse","overall","aggregate","table",
                             "each_site_coarse","each site","aggregate","raw",
                             "table_each_site_coarse","each site","aggregate","table",
                             "table_each_site_coarse_es","each site","summary","table",
                             "overall_fine","overall","individual","raw",
                             "table_fine","overall","individual","table",
                             "overall_ethnic","ethnic","aggregate","raw",
                             "table_ethnic","ethnic","aggregate","table",
                             "table_ethnic_coarse_es","ethnic","summary","table",
                             "overall_each_site_fine","each site","individual","raw",
                             "table_each_site_fine","each site","individual","table",
                             "overall_sex","sex","aggregate","raw",
                             "table_sex","sex","aggregate","table",
                             "table_sex_coarse_es","sex","summary","table",
                             "overall_age","age","aggregate","raw",
                             "table_age","age","aggregate","table",
                             "table_age_coarse_es","age","summary","table",
                             "aggregate_stage_shift","aggregate","summary","table"
)


#this is in order
my_output_sheets<-list("Sheet Description"=my_local_description,
                       "overall_coarse"=overall_coarse_analysis_extrapolated,
                       "table_coarse"=table_coarse_analysis_extrapolated,
                       "each_site_coarse"=overall_each_site_coarse_analysis_extrapolated,
                       "table_each_site_coarse"=table_each_site_coarse_analysis_extrapolated,
                       "table_each_site_coarse_es"=table_each_site_coarse_early_stage,
                       "overall_fine"=overall_fine_analysis_extrapolated,
                       "table_fine"=table_fine_analysis_extrapolated,
                       "overall_ethnic"=overall_ethnic_analysis_extrapolated,
                       "table_ethnic"=table_ethnic_analysis_extrapolated,
                       "table_ethnic_coarse_es"=table_ethnic_coarse_early_stage,
                       "overall_each_site_fine"=overall_each_site_fine_analysis_extrapolated,
                       "table_each_site_fine"=table_each_site_fine_analysis_extrapolated,
                       "overall_sex"=overall_sex_analysis_extrapolated,
                       "table_sex"=table_sex_analysis_extrapolated,
                       "table_sex_coarse_es"=table_sex_coarse_early_stage,
                       "overall_age"=overall_age_analysis_extrapolated,
                       "table_age"=table_age_analysis_extrapolated,
                       "table_age_coarse_es"=table_age_coarse_early_stage,
                       "aggregate_stage_shift"=hypothetical_aggregate_shift_coarse)

write.xlsx(my_output_sheets, file = sprintf("reports/%s_MS_all_data.xlsx",date_code))