#stage shift hypothetical outcomes

#build the complete stage shift table
#including ? stages

#need to suppress individual subgroups of cancers or they add to more than 100%
per_site_cod_coarse<-overall_each_site_coarse_analysis_extrapolated %>%
  select(Site=IndexCancer,Stage,COD=Fix,total_individuals,extrapolated_cod) %>%
  mutate(redundant_flag=case_when(Site=="Lung, NSCLC" ~ TRUE,
                                  Site=="Lung, SCLC" ~ TRUE,
                                  Site=="Breast, HR+" ~ TRUE,
                                  Site=="Breast, HR-" ~ TRUE,
                                  Site=="Breast, HR?" ~ TRUE,
                                  TRUE ~ FALSE)) %>%
  filter(!redundant_flag)


#build all the individual shifts per site
per_site_shift_coarse<-per_site_cod_coarse %>%
  mutate(clinical=match(Stage,c("I","II","III","IV","?"))) %>%
  select(Site,clinical,COD,total_individuals,clinical_cod=extrapolated_cod) %>%
  left_join(per_site_cod_coarse %>%
              mutate(prequel=match(Stage,c("I","II","III","IV","?"))) %>%
              select(Site,prequel,COD,prequel_cod=extrapolated_cod),multiple="all") %>%
  filter(prequel<=clinical) %>%
  mutate(tmp_flag=case_when(clinical==5 & prequel<5 ~ FALSE,
                            TRUE ~ TRUE)) %>%
  filter(tmp_flag) %>%
  select(-tmp_flag) %>%
  mutate(clinical_absolute_cod=clinical_cod*total_individuals,
         prequel_absolute_cod=prequel_cod*total_individuals) %>%
  select(Site,COD,prequel,clinical,total_individuals,clinical_cod,prequel_cod,prequel_absolute_cod,clinical_absolute_cod) %>%
  filter(COD!="Alive")

#select hypothesis for shifting procedure
hypothetical_per_site_shift_coarse<-per_site_shift_coarse %>% 
  mutate(prop_Z=case_when(prequel==clinical ~ 1.0,
                          TRUE ~ 0.0),
         prop_A=case_when(prequel==3 & clinical==4 ~ 1.0,
                          clinical==4 ~ 0.0,
                          prequel==clinical ~ 1.0,
                          TRUE ~ 0.0),
         prop_B=case_when(prequel<clinical & clinical==4 ~ 1/3,
                          clinical==4 ~ 0.0,
                          prequel==clinical ~ 1.0,
                          TRUE ~ 0.0),
         prop_U=case_when(prequel==1 ~ 1.0,
                          prequel==5 ~ 1.0,
                          TRUE ~ 0.0)) %>%
  mutate(total_Z=prop_Z*prequel_absolute_cod,
         total_A=prop_A*prequel_absolute_cod,
         total_B=prop_B*prequel_absolute_cod,
         total_U=prop_U*prequel_absolute_cod) 

scenario_label_df<-tribble(~Scenario,~ScenarioLabel,
                           "Z","Usual",
                           "A","IV->III",
                           "B","IV->III,II,I",
                           "U","IV,III,II->I")

# 
hypothetical_aggregate_shift_coarse<-hypothetical_per_site_shift_coarse  %>%
  group_by(COD) %>%
  summarize(final_Z=sum(total_Z),
            final_A=sum(total_A),
            final_B=sum(total_B),
            final_U=sum(total_U)) %>%
  ungroup() %>%
  pivot_longer(-COD,names_sep="_",names_to=c("Total","Scenario")) %>%
  select(COD,Scenario,COD_count=value) %>% 
  group_by(Scenario) %>%
  mutate(Percent_COD=100*COD_count/sum(COD_count)) %>%
  ungroup() %>%
  arrange(Scenario,COD) %>%
  left_join(scenario_label_df)

# by stage
hypothetical_aggregate_shift_coarse_by_stage<-hypothetical_per_site_shift_coarse  %>%
  group_by(COD,prequel) %>%
  summarize(final_Z=sum(total_Z),
            final_A=sum(total_A),
            final_B=sum(total_B),
            final_U=sum(total_U)) %>%
  ungroup() %>%
  pivot_longer(contains("final"),names_sep="_",names_to=c("Total","Scenario")) %>%
  select(COD,prequel,Scenario,COD_count=value) %>% 
  group_by(Scenario) %>%
  mutate(Percent_COD=100*COD_count/sum(COD_count)) %>%
  ungroup() %>%
  arrange(Scenario,COD,prequel) %>%
  left_join(scenario_label_df)

write_tsv(hypothetical_aggregate_shift_coarse,sprintf("reports/%s_MS_aggregate_stage_shift.tsv",date_code))



