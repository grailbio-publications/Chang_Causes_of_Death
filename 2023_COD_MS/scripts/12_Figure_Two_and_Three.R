#stage-specific cause of death if not index cancer
#all sites
#figure 2 and 3

figure_two_data<-table_fine_analysis_extrapolated

figure_two_data<-figure_two_data %>%
  filter(Status=="Cancer",Cause!="Index") %>%
  group_by(Stage) %>%
  mutate(Percent_Final=100*Percent_Excluding_Index/sum(Percent_Excluding_Index)) %>%
  ungroup() %>%
  arrange(Stage,desc(Percent_Final))

figure_three_data<-table_fine_analysis_extrapolated

figure_three_data<-figure_three_data %>%
  filter(Status=="NonCancer",Cause!="Alive") %>%
  group_by(Stage) %>%
  mutate(Percent_Final=100*Percent_Excluding_Index/sum(Percent_Excluding_Index)) %>%
  ungroup() %>%
  arrange(Stage,desc(Percent_Final))


#potentially better as top 10 and remainder?
figure_two_cause_alt_level_set<-figure_two_data %>% 
  mutate(Cause=gsub("-","/",Cause,fixed=TRUE)) %>% 
  select(Cause,Percent_Final) %>%
  group_by(Cause) %>%
  summarize(Percent_Final=mean(Percent_Final),Percent_Max=max(Percent_Final)) %>%
  ungroup() %>% 
  arrange(desc(Percent_Final)) %>%
  mutate(row=1:length(Cause)) %>%
  mutate(my_split=case_when(row<11 ~ "Top 10",
                            TRUE ~ "Remaining 15")) %>%
  mutate(my_split=factor(my_split,levels=c("Top 10","Remaining 15")))


#look at alternate plotting methods
COD_alt_figure_two_plot<-figure_two_data %>% 
  mutate(Cause=gsub("-","/",Cause,fixed=TRUE)) %>% 
  left_join(figure_two_cause_alt_level_set %>% select(Cause,my_split)) %>%
  mutate(Cause=factor(Cause,levels=rev(figure_two_cause_alt_level_set %>% pull(Cause)))) %>%
  mutate(Stage=case_when(Stage=="Unknown/missing" ~ "U",
                         TRUE ~ Stage)) %>%
  ggplot(aes(x=Cause,y=Percent_Final)) +
  geom_col(aes(fill=Stage),color="black",position="dodge")+
  coord_flip()+
  facet_wrap(~my_split,scale="free") +
  theme_bw()+
  scale_fill_viridis(option="turbo",discrete=TRUE)+
  theme(
    text=element_text(size=16),
    # axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    legend.position="right",
    title=element_text(size=18))+
  guides(fill=guide_legend(reverse=TRUE,ncol=1))+
  labs(x="Cause of Non\u00adIndex Cancer Death",y="Percent of Non\u00adIndex Cancer Death")+
  ggtitle("")

ggsave(sprintf("figs/%s_MS_cod_figure_two.pdf",date_code),
       COD_alt_figure_two_plot,
       width=18,height=12)


ggsave(sprintf("figs/%s_MS_cod_figure_two.eps",date_code),
       COD_alt_figure_two_plot,
       width=18,height=12)

ggsave(sprintf("figs/%s_MS_cod_figure_two.tiff",date_code),
       COD_alt_figure_two_plot,
       width=18,height=12)

#ggsave(sprintf("figs/%s_cod_alt_figure_two.svg",date_code),
#       COD_alt_figure_two_plot,
#       width=18,height=12)

#figure three
#potentially better as top5 and others?
figure_three_cause_alt_level_set<-figure_three_data %>% 
  mutate(Cause=gsub("-","/",Cause,fixed=TRUE)) %>% 
  mutate(Cause=case_when(Cause=="Other Non/Cancer" ~ "Other Non\u00adCancer",
                         Cause=="Suicide/Self/Injury" ~ "Suicide/Self\u00adInjury",
                         TRUE ~ Cause)) %>%
  select(Cause,Percent_Final) %>%
  group_by(Cause) %>%
  summarize(Percent_Final=mean(Percent_Final)) %>%
  ungroup() %>% 
  arrange(desc(Percent_Final)) %>%
  mutate(row=1:length(Cause)) %>%
  mutate(my_split=case_when(row<6 ~ "Top 5",
                            TRUE ~ "Remaining 9")) %>%
  mutate(my_split=factor(my_split,levels=c("Top 5","Remaining 9")))



COD_alt_figure_three_plot<-figure_three_data %>% 
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
  facet_wrap(~my_split,scale="free") +
  coord_flip()+
  theme_bw()+
  scale_fill_viridis(option="turbo",discrete=TRUE)+
  theme(
    text=element_text(size=16),
    # axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    legend.position="right",
    title=element_text(size=18))+
  guides(fill=guide_legend(reverse=TRUE,ncol=1))+
  labs(x="Cause of Non\u00adCancer Death",y="Percent of Non\u00adCancer Death")+
  ggtitle("")

ggsave(sprintf("figs/%s_MS_cod_figure_three.pdf",date_code),
       COD_alt_figure_three_plot,
       width=18,height=12)

ggsave(sprintf("figs/%s_MS_cod_figure_three.eps",date_code),
       COD_alt_figure_three_plot,
       width=18,height=12)
ggsave(sprintf("figs/%s_MS_cod_figure_three.tiff",date_code),
       COD_alt_figure_three_plot,
       width=18,height=12)


#ggsave(sprintf("figs/%s_cod_alt_figure_three.svg",date_code),
#       COD_alt_figure_three_plot,
 #      width=18,height=12)


