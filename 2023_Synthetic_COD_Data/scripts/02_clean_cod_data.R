#Index Cancer All Types should be the sum of all individual Index Cancer
# breast HR and Lung subtypes do not add up to "all"

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
                         "Uterus","Cancer",
                         "IndexCancerCOD","Cancer",
                         "NonIndexCOD","Cancer")


index_cancer_id_df<-tribble(~IndexCancer,~Category,~Parent,
                            "All Types","Aggregate",NA,
                            "Bladder","Individual","All Types",
                            "Breast, All","Individual","All Types",
                            "Breast, HR+","Individual","Breast, All",
                            "Breast, HR-","Individual","Breast, All",
                            "Breast, HR?","Individual","Breast, All",
                            "Cervix","Individual","All Types",
                            "Colon-Rectum","Individual","All Types",
                            "Esophagus","Individual","All Types",
                            "Kidney","Individual","All Types",
                            "Larynx","Individual","All Types",
                            "Liver-Bile Duct","Individual","All Types",
                            "Lung, All","Individual","All Types",
                            "Lung, NSCLC","Individual","Lung, All",
                            "Lung, SCLC","Individual","Lung, All",
                            "Lymphoma","Individual","All Types",
                            "Melanoma","Individual","All Types",
                            "Oral-Pharynx","Individual","All Types",
                            "Other Types","Individual","All Types",
                            "Ovary","Individual","All Types",
                            "Pancreas","Individual","All Types",
                            "Prostate","Individual","All Types",
                            "Stomach","Individual","All Types",
                            "Thyroid","Individual","All Types",
                            "Uterus","Individual","All Types")

#fix some inconsistencies between sheets
all_life_table_regular<-all_life_table_data_fixed %>%
  mutate(Cause=case_when(id=="Overall" & Stratum=="Index cancer COD" ~ "IndexCancerCOD",
                         id=="Overall" & Stratum=="Non-cancer COD" ~ "NonCancerCOD",
                         id=="Overall" & Stratum=="Non-index cancer COD" ~ "NonIndexCOD",
                         id=="Overall" & Stratum=="Other Cancer" ~ "Other Cancer", #individual other cancer cod
                         Stratum=="Alive" ~ "Alive",
                         Stratum=="Index Cancer" ~ "IndexCancerCOD",
                         Stratum=="Non-Cancer" ~ "NonCancerCOD",
                         Stratum=="Other Cancer" ~ "NonIndexCOD",
                         TRUE ~ Stratum)) %>% 
  left_join(index_cancer_id_df) %>%
  left_join(cause_cancer_df) %>%
  mutate(Status=replace_na(Status,replace="NonCancer"))




