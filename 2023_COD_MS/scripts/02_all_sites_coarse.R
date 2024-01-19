#All sites combined
#everyone
#stage I-IV and unknown/missing
#index/aggregate non-index, aggregate non-cancer = coarse analysis of COD

#which of these entries are aggregated data
overall_stratum_fix<-tribble(~Stratum,~Fix,
                             "Alive","Alive",
                             "Index cancer COD","Index",
                             "Non-cancer COD","NC",
                             "Non-index cancer COD","NIndex")


#raw events+ratios
overall_coarse_analysis_one<-all_life_table_data_fixed %>% 
  filter(id=="Overall",IndexCancer=="All Types") %>%
  semi_join(overall_stratum_fix) %>%
  left_join(overall_stratum_fix) %>%
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

index_max<-overall_coarse_analysis_one %>% summarize(mm=max(Index)) %>% pull(mm)
index_ext<-index_max-4 #use last 4 years to extrapolate

#final values
overall_coarse_analysis_extrapolated<-overall_coarse_analysis_one %>%
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
table_coarse_analysis_extrapolated<-overall_coarse_analysis_extrapolated %>%
  select(id,IndexCancer,Stage,Fix,extrapolated_outcome) %>%
  pivot_wider(names_from=Fix,values_from=extrapolated_outcome) %>%
  select(id,Site=IndexCancer,Stage,Index,NonIndex=NIndex,NonCancer=NC) %>%
  mutate(IndexPercent=100*Index/(Index+NonIndex+NonCancer),
         NonIndexPercent=100*NonIndex/(Index+NonIndex+NonCancer),
         NonCancerPercent=100*NonCancer/(Index+NonIndex+NonCancer)) %>%
  mutate(Stage=case_when(Stage=="?" ~ "Unknown/missing",
                         TRUE ~ Stage)) %>%
  arrange(Stage)

#part 1
write_tsv(overall_coarse_analysis_extrapolated,sprintf("reports/%s_MS_overall_coarse_analysis_extrapolated.tsv",date_code))
write_tsv(table_coarse_analysis_extrapolated,sprintf("reports/%s_MS_table_coarse_analysis_extrapolated.tsv",date_code))

