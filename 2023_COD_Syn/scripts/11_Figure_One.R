#renumbered for recall purposes
#Stage-specific distribution of causes of death: overall, and by first primary incident cancer site
#corresponds to all sites coarse
#corresponds to each site coarse



figure_one_data<-table_coarse_analysis_extrapolated %>%
  bind_rows(table_each_site_coarse_analysis_extrapolated)

## soft hyphen: `\u00ad`

#change from internal to human-readable
#also this gives the order of the factors
figure_one_factor_order<-tribble(~DisplayAlias, ~Site,
                                 "All Types","All Types",
                                 "Oral Cavity/Pharynx","Oral-Pharynx",
                                 "Esophagus","Esophagus",
                                 "Stomach","Stomach",
                                 "Colon/Rectum","Colon-Rectum",
                                 "Liver/Intrahepatic Bile Duct","Liver-Bile Duct",
                                 "Pancreas","Pancreas",
                                 "Larynx","Larynx",
                                 "Lung, All", "Lung, All",
                                 "Lung, Non\u00adSmall\u00adCell","Lung, NSCLC",
                                 "Lung, Small\u00adCell","Lung, SCLC",
                                 "Melanoma","Melanoma",
                                 "Breast, All", "Breast, All",
                                 "Breast, HR\u00adPositive","Breast, HR+",
                                 "Breast, HR\u00adNegative","Breast, HR-",
                                 "Breast, HR\u00adUnknown", "Breast, HR?",
                                 "Cervix","Cervix",
                                 "Uterus","Uterus",
                                 "Ovary","Ovary",
                                 "Prostate","Prostate",
                                 "Bladder","Bladder",
                                 "Kidney","Kidney",
                                 "Thyroid","Thyroid",
                                 "Lymphoma","Lymphoma",
                                 "Other Types","Other Types")

COD_figure_one_plot<-figure_one_data %>%
  left_join(figure_one_factor_order) %>%
  mutate(DisplayAlias=factor(DisplayAlias,levels=figure_one_factor_order$DisplayAlias)) %>%
  select(DisplayAlias,Stage,IndexPercent,NonIndexPercent,NonCancerPercent) %>%
  pivot_longer(contains("Percent")) %>%
  mutate(Cause=case_when(name=="IndexPercent" ~ "Index Cancer",
                         name=="NonCancerPercent" ~ "Non\u00adCancer",
                         name=="NonIndexPercent" ~ "Non\u00adIndex Cancer",
                         TRUE ~ "BOGUS")) %>%
  mutate(Cause=factor(Cause,levels=c("Non\u00adCancer","Non\u00adIndex Cancer","Index Cancer"))) %>%
  mutate(Stage=case_when(Stage=="Unknown/missing" ~ "U",
                         TRUE ~ Stage)) %>%
  ggplot(aes(x=Stage,y=value))+
  geom_col(aes(fill=Cause),color="black")+
  facet_wrap(~DisplayAlias)+
  theme_bw()+
  scale_fill_viridis(option="viridis",discrete=TRUE)+
  theme(
    text=element_text(size=16),
    # axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    legend.position="top",
    title=element_text(size=18))+
  guides(fill=guide_legend(title="Cause of Death",reverse=TRUE))+
  labs(x="Stage at Diagnosis",y="Percent Cause of Death")+
  ggtitle("")

ggsave(sprintf("figs/%s_MS_cod_coarse_figure_one.pdf",date_code),
       COD_figure_one_plot,
       width=12,height=12)


ggsave(sprintf("figs/%s_MS_cod_coarse_figure_one.eps",date_code),
       COD_figure_one_plot,
       width=12,height=12)

  ggsave(sprintf("figs/%s_MS_cod_coarse_figure_one.tiff",date_code),
         COD_figure_one_plot,
         width=12,height=12)



#svg doesn't handle unicode right
#ggsave(sprintf("figs/%s_MS_cod_coarse_figure_one.svg",date_code),
#       COD_figure_one_plot,
#       width=12,height=12)