#Analysis: 
#  All sites combined: 
#  Non-Hispanic White, non-Hispanic Black, Hispanic, non-Hispanic API, non-Hispanic AIAN
#Stage I-IV
#Index/Aggregate Non-Index/Aggregate Non-Cancer COD
#(number/proportion extrapolated for those alive)

race_ethnic_df<-tribble(~id,
                        "White",
                        "Black",
                        "Hispanic",
                        "AAPI",
                        "AIAN")

#coding change between sheets - note name-space collision for Other Cancer!
ethnic_stratum_fix<-tribble(~Stratum,~Fix,
                            "Alive","Alive",
                            "Index Cancer","Index",
                            "Non-Cancer","NC",
                            "Other Cancer","NIndex")

overall_ethnic_analysis_one<-all_life_table_data_fixed %>% 
  semi_join(race_ethnic_df) %>%
  filter(IndexCancer=="All Types") %>%
  semi_join(ethnic_stratum_fix) %>%
  left_join(ethnic_stratum_fix) %>%
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
overall_ethnic_analysis_extrapolated<-overall_ethnic_analysis_one %>%
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

#arranged in a table
table_ethnic_analysis_extrapolated<-overall_ethnic_analysis_extrapolated %>%
  select(id,IndexCancer,Stage,Fix,extrapolated_outcome) %>%
  pivot_wider(names_from=Fix,values_from=extrapolated_outcome) %>%
  select(id,Site=IndexCancer,Stage,Index,NonIndex=NIndex,NonCancer=NC) %>%
  mutate(IndexPercent=100*Index/(Index+NonIndex+NonCancer),
         NonIndexPercent=100*NonIndex/(Index+NonIndex+NonCancer),
         NonCancerPercent=100*NonCancer/(Index+NonIndex+NonCancer)) %>%
  mutate(Stage=case_when(Stage=="?" ~ "Unknown/missing",
                         TRUE ~ Stage)) %>%
  arrange(Stage)

#do we want stage I-II combined?
table_ethnic_coarse_early_stage<-overall_ethnic_analysis_extrapolated %>%
  select(id,IndexCancer,Stage,Fix,extrapolated_outcome) %>%
  filter(Stage %in% c("I","II")) %>%
  group_by(id,IndexCancer,Fix) %>%
  summarize(extrapolated_outcome=sum(extrapolated_outcome)) %>%
  ungroup() %>%
  mutate(Stage="I-II") %>%
  pivot_wider(names_from=Fix,values_from=extrapolated_outcome) %>%
  select(id,Site=IndexCancer,Stage,Index,NonIndex=NIndex,NonCancer=NC) %>%
  mutate(IndexPercent=100*Index/(Index+NonIndex+NonCancer),
         NonIndexPercent=100*NonIndex/(Index+NonIndex+NonCancer),
         NonCancerPercent=100*NonCancer/(Index+NonIndex+NonCancer)) %>%
  mutate(Stage=case_when(Stage=="?" ~ "Unknown/missing",
                         TRUE ~ Stage)) %>%
  arrange(Stage)

write_tsv(overall_ethnic_analysis_extrapolated,sprintf("reports/%s_MS_overall_ethnic_analysis_extrapolated.tsv",date_code))
write_tsv(table_ethnic_analysis_extrapolated,sprintf("reports/%s_MS_table_ethnic_analysis_extrapolated.tsv",date_code))
write_tsv(table_ethnic_coarse_early_stage,sprintf("reports/%s_MS_table_ethnic_coarse_early_stage.tsv",date_code))

