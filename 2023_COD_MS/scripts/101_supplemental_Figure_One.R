#extrapolation two ways
#1) raw event ratio
#2) event ratio with lost adjusted
#3) event ratio with alive broken out by last 4 years extrapolation
#4) final event ratio

#demonstrate with coarse


#my_one_values=c("Non-Cancer"="#F8766D","Non-Index Cancer"="#00BA38","Index"="#619CFF","Lost"="grey",Alive="plum")
my_one_values=c("Non-Cancer"="#440154FF","Non-Index Cancer"="#21908CFF","Index Cancer"="#FDE725FF","Lost"="grey",Alive="plum")

#known fates of individuals at end of each year
supp_figure_one_a<-overall_coarse_analysis_one %>% 
  filter(Stage=="I") %>%
  select(Fix,Index,number_minus_risk,lost_total,died_cause) %>% 
  pivot_wider(names_from=c("Fix"),values_from=c("died_cause","lost_total")) %>%
  select(Index,number_minus_risk,died_cause_Index,died_cause_NC,died_cause_NIndex,lost_total_Alive) %>%
  pivot_longer(-Index) %>% 
  group_by(Index) %>%
  mutate(prop=value/sum(value)) %>%
  ungroup() %>%
  mutate(token=case_when(name=="number_minus_risk" ~ "Alive",
                         name=="lost_total_Alive" ~ "Lost",
                         name=="died_cause_Index" ~ "Index Cancer",
                         name=="died_cause_NIndex" ~ "Non-Index Cancer",
                         name=="died_cause_NC" ~ "Non-Cancer")) %>%
  mutate(token=factor(token,levels=c("Non-Cancer","Non-Index Cancer","Index Cancer","Lost","Alive"))) %>%
  mutate(TIndex=case_when(Index<=index_max ~ sprintf("%02d",Index),
                          Index==(index_max+1) ~ "Assign Alive",
                          TRUE ~ "Final Total")) %>%
  ggplot(aes(x=TIndex,y=prop))+
  geom_col(aes(fill=token),color="black")+
  scale_fill_manual(name="Status or Cause of Death",values=my_one_values)+
  theme_bw()+
  theme(
    text=element_text(size=16),
    # axis.text = element_text(size=14),
    axis.text = element_text(size=14),
    axis.text.x=element_text(angle=45,vjust=1.0,hjust=1.0),
    legend.text = element_text(size=14),
    legend.position="top",
    title=element_text(size=18))+
  guides(fill=guide_legend(reverse=TRUE))+
  labs(x="Year After Diagnosis",y="Proportion Status")+
  ggtitle("Cumulative Proportion of Events")

#filling in lost with last ratio
supp_figure_one_b<-overall_coarse_analysis_one %>%
  filter(Stage=="I") %>%
  select(Fix,Index,total_os_c,c_os_c) %>%
  mutate(t_os_c=case_when(Fix=="Alive" ~ total_os_c,
                          TRUE ~ c_os_c)) %>%
  mutate(token=case_when(Fix=="Index" ~ "Index Cancer",
                         Fix=="NC" ~ "Non-Cancer",
                         Fix=="NIndex" ~ "Non-Index Cancer",
                         TRUE ~ Fix)) %>%
  mutate(token=factor(token,levels=c("Non-Cancer","Non-Index Cancer","Index Cancer","Lost","Alive"))) %>%
  mutate(TIndex=case_when(Index<=index_max ~ sprintf("%02d",Index),
                          Index==(index_max+1) ~ "Assign Alive",
                          TRUE ~ "Final Total")) %>%
  ggplot(aes(x=TIndex,y=t_os_c))+
  geom_col(aes(fill=token),color="black",show.legend=FALSE)+
  scale_fill_manual(name="Status or Cause of Death",values=my_one_values)+
  theme_bw()+
  theme(
    text=element_text(size=16),
    # axis.text = element_text(size=14),
    axis.text = element_text(size=14),
    axis.text.x=element_text(angle=45,vjust=1.0,hjust=1.0),
    legend.text = element_text(size=14),
    legend.position="top",
    title=element_text(size=18))+
  guides(fill=guide_legend(reverse=TRUE))+
  labs(x="Year After Diagnosis",y="Proportion Status")+
  ggtitle("Proportion of Events: Lost Imputed")


##death ratio

supp_figure_one_c<-overall_coarse_analysis_one %>%
  filter(Stage=="I",Fix!="Alive") %>%
  select(Fix,Index,died,died_at_risk,prop_died_at_risk) %>%
  mutate(token=case_when(Fix=="Index" ~ "Index Cancer",
                         Fix=="NC" ~ "Non-Cancer",
                         Fix=="NIndex" ~ "Non-Index Cancer",
                         TRUE ~ Fix)) %>%
  mutate(token=factor(token,levels=c("Non-Cancer","Non-Index Cancer","Index Cancer","Lost","Alive"))) %>%
  mutate(TIndex=case_when(Index<=index_max ~ sprintf("%02d",Index),
                          Index==(index_max+1) ~ "Assign Alive",
                          TRUE ~ "Final Total")) %>%
  ggplot(aes(x=TIndex,y=prop_died_at_risk))+
  geom_col(aes(fill=token),color="black",show.legend=FALSE)+
  scale_fill_manual(name="Status or Cause of Death",values=my_one_values)+
  theme_bw()+
  theme(
    text=element_text(size=16),
    # axis.text = element_text(size=14),
    axis.text = element_text(size=14),
    axis.text.x=element_text(angle=45,vjust=1.0,hjust=1.0),
    legend.text = element_text(size=14),
    legend.position="top",
    title=element_text(size=18))+
  guides(fill=guide_legend(reverse=TRUE))+
  labs(x="Year After Diagnosis",y="Proportion Status")+
  ggtitle("Death Ratio by Year")


#filling in last alive with last cause
#bind rows to get two more steps
#index +1 = alive, broken into proportions
#index +2 = final proportions, rearranged?



fake_row_one<-overall_coarse_analysis_extrapolated %>%
  filter(Stage=="I") %>%
  select(Fix,final_proportion,extrapolated_cod) %>%
  mutate(difference=extrapolated_cod-final_proportion) %>%
  mutate(Index=index_max+1) %>%
  filter(Fix!="Alive") %>%
  select(Fix,Index,t_os_c=difference)

fake_row_two<-overall_coarse_analysis_extrapolated %>%
  filter(Stage=="I") %>%
  select(Fix,final_proportion,extrapolated_cod) %>%
  mutate(Index=index_max+2) %>%
  filter(Fix!="Alive") %>%
  select(Fix,Index,t_os_c=extrapolated_cod)

supp_figure_one_d<-overall_coarse_analysis_one %>%
  filter(Stage=="I") %>%
  select(Fix,Index,total_os_c,c_os_c) %>%
  mutate(t_os_c=case_when(Fix=="Alive" ~ total_os_c,
                          TRUE ~ c_os_c)) %>%
  select(Fix,Index,t_os_c) %>%
  bind_rows(fake_row_one) %>%
  bind_rows(fake_row_two) %>%
  mutate(token=case_when(Fix=="Index" ~ "Index Cancer",
                         Fix=="NC" ~ "Non-Cancer",
                         Fix=="NIndex" ~ "Non-Index Cancer",
                         TRUE ~ Fix)) %>%
  mutate(token=factor(token,levels=c("Non-Cancer","Non-Index Cancer","Index Cancer","Lost","Alive"))) %>%
  mutate(TIndex=case_when(Index<=index_max ~ sprintf("%02d",Index),
                          Index==(index_max+1) ~ "Assign Alive",
                          TRUE ~ "Final Total")) %>%
  ggplot(aes(x=TIndex,y=t_os_c))+
  geom_col(aes(fill=token),color="black",show.legend=FALSE)+
  scale_fill_manual(name="Status or Cause of Death",values=my_one_values)+
  theme_bw()+
  theme(
    text=element_text(size=16),
    # axis.text = element_text(size=14),
    axis.text = element_text(size=14),
    axis.text.x=element_text(angle=45,vjust=1.0,hjust=1.0),
    legend.text = element_text(size=14),
    legend.position="top",
    title=element_text(size=18))+
  guides(fill=guide_legend(reverse=TRUE))+
  labs(x="Year After Diagnosis",y="Proportion Status")+
  ggtitle("Cause of Death Extrapolated for Alive")

supp_figure_one_all<-(supp_figure_one_a + supp_figure_one_b) / (supp_figure_one_c + supp_figure_one_d)

supp_figure_one_final<-supp_figure_one_all +plot_annotation(tag_levels='A')+plot_layout(guides="collect") & theme(legend.position="bottom")



ggsave(sprintf("figs/%s_MS_supp_figure_steps_one.pdf",date_code),
       supp_figure_one_final,
       width=16,height=16)  

ggsave(sprintf("figs/%s_MS_supp_figure_steps_one.eps",date_code),
       supp_figure_one_final,
       width=16,height=16)  
ggsave(sprintf("figs/%s_MS_supp_figure_steps_one.tiff",date_code),
       supp_figure_one_final,
       width=16,height=16)  


#ggsave(sprintf("figs/%s_supp_figure_one.svg",date_code),
#       supp_figure_one_final,
#       width=16,height=16)  

