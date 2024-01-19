#prostate,breast,lung,colorectal

supp_figure_two_data<-table_each_site_fine_analysis_extrapolated

supp_figure_two_data<-supp_figure_two_data %>%
  filter(Site %in% c("Prostate","Breast, All","Lung, All","Colon-Rectum")) %>%
  mutate(Site=gsub("-","/",Site,fixed=TRUE)) %>%
  filter(Status=="Cancer",Cause!="Index") %>%
  group_by(Site,Stage) %>%
  mutate(Percent_Final=100*Percent_Excluding_Index/sum(Percent_Excluding_Index)) %>%
  ungroup() %>%
  arrange(Stage,desc(Percent_Final))


#keep alternate figure two order here as well
#should this be altered?  
supp_figure_two_leveller<-supp_figure_two_data %>% 
  mutate(Cause=gsub("-","/",Cause,fixed=TRUE)) %>% 
  group_by(Cause) %>% 
  summarize(Percent_Max=max(Percent_Final)) %>% 
  ungroup() %>% 
  left_join(figure_two_cause_alt_level_set %>% select(Cause,my_split))

supp_alt_figure_two_ni_plot<-supp_figure_two_data %>% 
  mutate(Cause=gsub("-","/",Cause,fixed=TRUE)) %>% 
  left_join(figure_two_cause_alt_level_set %>% select(Cause,my_split)) %>%
  mutate(Cause=factor(Cause,levels=rev(figure_two_cause_alt_level_set %>% pull(Cause)))) %>%
  mutate(Stage=case_when(Stage=="Unknown/missing" ~ "U",
                         TRUE ~ Stage)) %>%
  ggplot(aes(x=Cause,y=Percent_Final)) +
  geom_col(aes(fill=Stage),color="black",position="dodge")+
  geom_blank(aes(x=Cause,y=Percent_Max),data=supp_figure_two_leveller)+
  coord_flip()+
  facet_wrap(my_split~Site,scales="free",ncol=4)+
  scale_fill_viridis(option="turbo",discrete=TRUE)+
  theme_bw()+
  theme(
    text=element_text(size=16),
    # axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    legend.position="right",
    title=element_text(size=18))+
  guides(fill=guide_legend(reverse=TRUE,ncol=1))+
  labs(x="Cause",y="Percent of Non-Index Cancer Death")+
  ggtitle("Cause of Death: Non-Index Cancers by Stage")

ggsave(sprintf("figs/%s_MS_cod_alt_supp_figure_ni_three.pdf",date_code),
       supp_alt_figure_two_ni_plot,
       width=24,height=16)

ggsave(sprintf("figs/%s_MS_cod_alt_supp_figure_ni_three.eps",date_code),
       supp_alt_figure_two_ni_plot,
       width=24,height=16)

ggsave(sprintf("figs/%s_MS_cod_alt_supp_figure_ni_three.tiff",date_code),
       supp_alt_figure_two_ni_plot,
       width=24,height=16)


supp_figure_three_data<-table_each_site_fine_analysis_extrapolated

#figure 3 data here deliberate: keep same order of COD as in figure 3
supp_figure_three_data<-supp_figure_three_data %>%
  filter(Site %in% c("Prostate","Breast, All","Lung, All","Colon-Rectum")) %>%
  mutate(Site=gsub("-","/",Site,fixed=TRUE)) %>%
  filter(Status=="NonCancer",Cause!="Alive") %>%
  group_by(Site,Stage) %>%
  mutate(Percent_Final=100*Percent_Excluding_Index/sum(Percent_Excluding_Index)) %>%
  ungroup() %>%
  arrange(Stage,desc(Percent_Final))

#alternate figure three
supp_figure_three_leveller<-supp_figure_three_data %>% 
  mutate(Cause=gsub("-","/",Cause,fixed=TRUE)) %>% 
  mutate(Cause=case_when(Cause=="Other Non/Cancer" ~ "Other Non\u00adCancer",
                         Cause=="Suicide/Self/Injury" ~ "Suicide/Self\u00adInjury",
                         TRUE ~ Cause)) %>%
  group_by(Cause) %>% 
  summarize(Percent_Max=max(Percent_Final)) %>% 
  ungroup() %>% 
  left_join(figure_three_cause_alt_level_set %>% select(Cause,my_split))

supp_alt_figure_four_nc_plot<-supp_figure_three_data %>% 
  mutate(Cause=gsub("-","/",Cause,fixed=TRUE)) %>% 
  mutate(Cause=case_when(Cause=="Other Non/Cancer" ~ "Other Non\u00adCancer",
                         Cause=="Suicide/Self/Injury" ~ "Suicide/Self\u00adInjury",
                         TRUE ~ Cause)) %>%
  left_join(figure_three_cause_alt_level_set %>% select(Cause,my_split)) %>%
  mutate(Cause=factor(Cause,levels=rev(figure_three_cause_alt_level_set %>% pull(Cause)))) %>%
  mutate(Stage=case_when(Stage=="Unknown/missing" ~ "U",
                         TRUE ~ Stage)) %>%
  ggplot(aes(x=Cause,y=Percent_Final)) +
  geom_col(aes(fill=Stage),color="black",position="dodge")+
  geom_blank(aes(x=Cause,y=Percent_Max),data=supp_figure_three_leveller)+
  coord_flip()+
  scale_fill_viridis(option="turbo",discrete=TRUE)+
  facet_wrap(my_split~Site,scales="free",ncol=4)+
  theme_bw()+
  theme(
    text=element_text(size=16),
    # axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    legend.position="right",
    title=element_text(size=18))+
  guides(fill=guide_legend(reverse=TRUE,ncol=1))+
  labs(x="Cause",y="Percent of Non-Cancer Death")+
  ggtitle("Cause of Death: Non-Cancers by Stage")

ggsave(sprintf("figs/%s_MS_cod_alt_supp_figure_nc_four.pdf",date_code),
       supp_alt_figure_four_nc_plot,
       width=24,height=16)

ggsave(sprintf("figs/%s_MS_cod_alt_supp_figure_nc_four.eps",date_code),
       supp_alt_figure_four_nc_plot,
       width=24,height=16)
ggsave(sprintf("figs/%s_MS_cod_alt_supp_figure_nc_four.tiff",date_code),
       supp_alt_figure_four_nc_plot,
       width=24,height=16)