#Analysis: 
#  All sites combined, 
#Everyone
#stage I-IV, 
#Index/ Individual Non-Index /Individual Non-cancer COD
#(number/proportion extrapolated for those alive)

cause_cancer_df<-tribble(~Cause,~Status,
                         "Bladder","Cancer",
                         "Brain-Nervous", "Cancer",
                         "Breast","Cancer",
                         "Cervix","Cancer",
                         "Colon-Rectum","Cancer",
                         "Esophagus","Cancer",
                         "Index","Cancer",
                         "Kidney","Cancer",
                         "Leukemia","Cancer",
                         "Liver-Bile Duct","Cancer",
                         "Lung","Cancer",
                         "Lymphoma","Cancer",
                         "Melanoma","Cancer",
                         "Myeloma","Cancer",
                         "Oral-Pharynx","Cancer",
                         "Other Cancer","Cancer",
                         "Other Digestive","Cancer",
                         "Other Respiratory","Cancer",
                         "Other Female","Cancer",
                         "Other Urinary","Cancer",
                         "Other Male","Cancer",
                         "Ovary","Cancer",
                         "Pancreas","Cancer",
                         "Prostate","Cancer",
                         "Stomach","Cancer",
                         "Uterus","Cancer")


#fine scale numbers

overall_fine_analysis_one<-all_life_table_data_fixed %>% 
  filter(id=="Overall",IndexCancer=="All Types") %>%
  #  semi_join(overall_stratum_fix) %>%
  left_join(overall_stratum_fix) %>%
  mutate(Fix=case_when(is.na(Fix) ~ Stratum,
                       TRUE ~ Fix)) %>%
  filter(Fix!="NIndex", Fix!="NC") %>%
  select(-Stratum) %>%
  complete(id,IndexCancer,Fix,Stage,Index,fill=list(alive_start=0,died=0,lost=0)) %>%
  group_by(id,IndexCancer,Stage,Index) %>%
  mutate(number_at_risk=sum(alive_start),
         lost_at_risk=sum(lost),
         died_at_risk=sum(died),
         number_minus_risk=number_at_risk-lost_at_risk-died_at_risk,
         prop_died_at_risk=died/pmax(died_at_risk,1e-6), #guard zeros
         total_os_i=(1-died_at_risk/pmax(1e-6,number_at_risk))) %>%
  ungroup() %>%
  group_by(id,IndexCancer,Stage,Fix) %>%
  mutate(lost_total=cumsum(lost_at_risk),
         died_cause=cumsum(died),
         total_os_c=cumprod(total_os_i),
         diff_os_c=lag(total_os_c,default=1.0)-total_os_c,
         m_os_c=diff_os_c*prop_died_at_risk,
         c_os_c=cumsum(m_os_c)) %>%
  ungroup() 

#final values
overall_fine_analysis_extrapolated<-overall_fine_analysis_one %>%
  group_by(id,IndexCancer,Stage,Fix) %>% 
  summarize(total_individuals=max(number_at_risk),
            number_last_known_outcome=max(alive_start),
            final_os=min(total_os_c),
            final_fraction_cod=max(c_os_c),
            extrapolated_prop_died=sum(died[Index>index_ext]/pmax(1e-6,sum(died_at_risk[Index>index_ext])))) %>% 
  ungroup() %>% 
  mutate(extrapolated_cod=final_os*extrapolated_prop_died+final_fraction_cod) %>%
  mutate(final_proportion=case_when(Fix=="Alive" ~ final_os,
                                    TRUE ~ final_fraction_cod)) %>% 
  mutate(number_final_outcome=final_proportion*total_individuals) %>%
  mutate(extrapolated_outcome=extrapolated_cod*total_individuals) %>%
  select(id,IndexCancer,Stage,Fix,
         total_individuals,number_last_known_outcome,
         final_proportion,number_final_outcome,
         extrapolated_prop_died,extrapolated_cod,extrapolated_outcome) 

#too many for columns: make rows instead

table_fine_analysis_extrapolated<-overall_fine_analysis_extrapolated %>%
  select(id,Site=IndexCancer,Stage,Cause=Fix,extrapolated_outcome) %>%
  left_join(cause_cancer_df) %>%
  mutate(Status=replace_na(Status,replace="NonCancer")) %>%
  group_by(Stage) %>%
  mutate(Percent=100*extrapolated_outcome/sum(extrapolated_outcome)) %>%
  mutate(Percent_Excluding_Index=case_when(Cause=="Index" ~ 0.0,
                                           TRUE ~ 100*extrapolated_outcome/sum(extrapolated_outcome[Cause!="Index"]))) %>%
  ungroup() %>%
  mutate(Stage=case_when(Stage=="?" ~ "Unknown/missing",
                         TRUE ~ Stage)) %>% 
  arrange(Stage,Status,desc(Percent_Excluding_Index))

write_tsv(overall_fine_analysis_extrapolated,sprintf("reports/%s_MS_overall_fine_analysis_extrapolated.tsv",date_code))
write_tsv(table_fine_analysis_extrapolated,sprintf("reports/%s_MS_table_fine_analysis_extrapolated.tsv",date_code))

