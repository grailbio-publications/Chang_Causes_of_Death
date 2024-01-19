#race_ethnicity
#contrast within-stage across ethnicity

figure_four_data<-table_ethnic_analysis_extrapolated 

COD_figure_four_plot<-figure_four_data %>%
  select(id,Site,Stage,IndexPercent,NonIndexPercent,NonCancerPercent) %>%
  pivot_longer(contains("Percent")) %>%
  mutate(Cause=case_when(name=="IndexPercent" ~ "Index Cancer",
                         name=="NonCancerPercent" ~ "Non\u00adCancer",
                         name=="NonIndexPercent" ~ "Non\u00adIndex Cancer",
                         TRUE ~ "BOGUS")) %>%
  mutate(Cause=factor(Cause,levels=c("Non\u00adCancer","Non\u00adIndex Cancer","Index Cancer"))) %>%
  mutate(Stage=case_when(Stage=="Unknown/missing" ~ "Unknown/Missing Stage",
                         TRUE ~ sprintf("Stage %s",Stage))) %>%
  ggplot(aes(x=id,y=value))+
  geom_col(aes(fill=Cause),color="black")+
  facet_wrap(~Stage,ncol=2)+
  #coord_flip()+
  theme_bw()+
  scale_fill_viridis(option="viridis",discrete=TRUE)+
  theme(
    text=element_text(size=16),
    # axis.text = element_text(size=14),
    axis.text = element_text(size=14),
    axis.text.x=element_text(angle=45,vjust=1.0,hjust=1.0),
    legend.text = element_text(size=14),
    legend.position="top",
    title=element_text(size=18))+
  guides(fill=guide_legend(title="Cause of Death", reverse=TRUE))+
  labs(x="",y="Percent Cause of Death")+
  ggtitle("")


ggsave(sprintf("figs/%s_cod_race_ethnic_coarse_figure_four.pdf",date_code),
       COD_figure_four_plot,
       width=12,height=12)

ggsave(sprintf("figs/%s_cod_race_ethnic_coarse_figure_four.eps",date_code),
       COD_figure_four_plot,
       width=12,height=12)

ggsave(sprintf("figs/%s_cod_race_ethnic_coarse_figure_four.tiff",date_code),
       COD_figure_four_plot,
       width=12,height=12)

#ggsave(sprintf("figs/%s_cod_race_ethnic_coarse_figure_four.svg",date_code),
#       COD_figure_four_plot,
#       width=12,height=12)