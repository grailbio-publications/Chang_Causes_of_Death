#sensitivity analysis for reviewer
#what components of lost/alive at end contribute to ratios
# this is for coarse analysis only


#overall_coarse_analysis_extrapolated
#contains successive cumulative esimates from the pipeline
#number_last_known_outcome = individuals where the outcome is known (no lost, no extrapolation)
#number_final_outcome = imputed lost up to duration of time -  (no extrapolation)
#extrapolated_outcome = extrapolated fates of those alive
#taking differences allows generation of the numbers and relative causes of death within each group

overall_coarse_stepwise_balance<-overall_coarse_analysis_extrapolated %>%
  filter(Stage!="?") %>%
  mutate(delta_known=number_last_known_outcome,
         delta_lost=number_final_outcome-number_last_known_outcome,
         delta_extrapolated=extrapolated_outcome-number_final_outcome,
         total_final=extrapolated_outcome) %>%
  select(Fix,Stage,final=total_final,known=delta_known,lost=delta_lost,xtra=delta_extrapolated) %>% 
  filter(Fix!="Alive") %>% 
  pivot_longer(c("final","known","lost","xtra")) %>% 
  rename(Source=name,COD=Fix) %>% 
  group_by(Source,Stage) %>%
  mutate(total=sum(value),
         ratio=100*value/total,
         top_level=100) %>%
  ungroup() %>% 
  group_by(COD,Stage) %>%
  mutate(fraction=2*total/sum(total)) %>%
  ungroup() 

source_map<-tribble(~Source,~SourceName,
                    "final","Final Estimate",
                    "known","Known Deaths",
                    "lost","Lost Imputed Deaths",
                    "xtra","Extrapolated Deaths") %>%
  mutate(SourceName=factor(SourceName,levels=c("Final Estimate","Known Deaths","Lost Imputed Deaths","Extrapolated Deaths")))

supp_figure_ratio_by_step<-overall_coarse_stepwise_balance %>% 
  mutate(Status=COD) %>%
  mutate(token=case_when(
                         Status=="Index" ~ "Index Cancer",
                         Status=="NIndex" ~ "Non-Index Cancer",
                         Status=="NC" ~ "Non-Cancer")) %>%
  mutate(token=factor(token,levels=c("Non-Cancer","Non-Index Cancer","Index Cancer","Lost","Alive"))) %>%
  left_join(source_map) %>%
  ggplot(aes(x=SourceName,y=ratio))+
  geom_col(aes(fill=token),color="black",position="dodge")+
  geom_label(aes(y=top_level,label=round(100*fraction)),size=6)+
  geom_vline(xintercept=1.5,lty="dashed")+
  facet_wrap(~Stage)+
  scale_fill_manual(name="Cause of Death",values=my_one_values)+
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
  labs(x="",y="Percent Cause of Death")+
  ggtitle("Overall Cancers: Ratio of Causes of Death\n by Stage And Computational Step")

ggsave(sprintf("figs/%s_MS_supp_figure_ratio_two.pdf",date_code),
       supp_figure_ratio_by_step,
       width=16,height=16)  

ggsave(sprintf("figs/%s_MS_supp_figure_ratio_two.eps",date_code),
       supp_figure_ratio_by_step,
       width=16,height=16)  

ggsave(sprintf("figs/%s_MS_supp_figure_ratio_two.tiff",date_code),
       supp_figure_ratio_by_step,
       width=16,height=16)  



  