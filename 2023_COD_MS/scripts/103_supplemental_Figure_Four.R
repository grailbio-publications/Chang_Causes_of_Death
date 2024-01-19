#sex cause of death

supp_figure_four_data<-table_sex_analysis_extrapolated

supp_figure_four_sex_plot<-supp_figure_four_data %>%
  select(id,Stage,IndexPercent,NonIndexPercent,NonCancerPercent) %>%
  pivot_longer(contains("Percent")) %>%
  mutate(Cause=case_when(name=="IndexPercent" ~ "Index Cancer",
                         name=="NonCancerPercent" ~ "Non-Cancer",
                         name=="NonIndexPercent" ~ "Non-Index Cancer",
                         TRUE ~ "BOGUS")) %>%
  mutate(Cause=factor(Cause,levels=c("Non-Cancer","Non-Index Cancer","Index Cancer"))) %>%
  mutate(Stage=case_when(Stage=="Unknown/missing" ~ "U",
                         TRUE ~ Stage)) %>%
  ggplot(aes(x=Stage,y=value))+
  geom_col(aes(fill=Cause),color="black")+
  facet_wrap(~id)+
  scale_fill_viridis(name="Cause of Death",option="viridis",discrete=TRUE)+
  theme_bw()+
  theme(
    text=element_text(size=16),
    # axis.text = element_text(size=14),
    legend.text = element_text(size=14),
    legend.position="top",
    title=element_text(size=18))+
  guides(fill=guide_legend(reverse=TRUE))+
  labs(x="Stage",y="Percent Cause Of Death")+
  ggtitle("Cause of Death By Sex and Stage")

ggsave(sprintf("figs/%s_MS_sex_coarse_supp_figure_six.pdf",date_code),
       supp_figure_four_sex_plot,
       width=12,height=12)

ggsave(sprintf("figs/%s_MS_sex_coarse_supp_figure_six.eps",date_code),
       supp_figure_four_sex_plot,
       width=12,height=12)

ggsave(sprintf("figs/%s_MS_sex_coarse_supp_figure_six.tiff",date_code),
       supp_figure_four_sex_plot,
       width=12,height=12)