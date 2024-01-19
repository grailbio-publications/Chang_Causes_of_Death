#generate synthetic data that looks like 'cause of death' data
#but preserves privacy for individuals
#this does not preserve all structure in the data as it is  just to demonstrate code

#for each cause of death strata
#pad to full number of years (disguise any particular year of last person dying)
#Case 1: alive->lost - 
# multinomial probabilities of being lost in each year are retrieved from the data
# padded with small probability to avoid zeros
# total number of individuals drawn as poisson from the number observed
# generate multinomial events of being lost in each year
# reverse sum to generate alive_start
#Case 2: cause->died
# multinomial probabilities of dying in each year retrieved from the data
# padded with small probability to avoid zeros
# non-decreasing probability with time enforced (risk population decreasing)
# total number of individuals generated at least 6, or poisson with padding to disguise low numbers of events
# generate multinomial events
# reverse sum to generate alive_start

#this generates generally plausible data
#does not preserve sums of individual causes of death adding up to overall death
#does not replicate exact probabilities of dying from rare causes of death due to padding
#slightly over-estimated death rates at end of time due to not accounting for at-risk population explicitly

#execute procedure
test_syn_life_table<-all_life_table_regular %>%
  complete(Index,nesting(id,IndexCancer,Stage,Stratum,Cause,Category,Status,Parent),
           fill=list(alive_start=0,lost=0,died=0)) %>%
  group_by(id,IndexCancer,Stage,Stratum,Cause) %>%
  mutate(nneeded=max(alive_start),
         ndist=died+lost,
         lost_flag=sum(lost)>0) %>%
  mutate(input_multi=case_when(lost_flag ~ pmax(ndist+0.5,1),
                               TRUE ~ pmax(rev(cummax(rev(ndist)))+0.5,1)),
         syn_data=rmultinom(1,max(6,rpois(1,nneeded[1]+3)),input_multi)[,1]) %>%
  mutate(syn_alive=rev(cumsum(rev(syn_data)))) %>%
  ungroup() %>%
  arrange(id,IndexCancer,Stage,Stratum,Index)

#generate output table
output_syn_life_table<-test_syn_life_table %>%
  mutate(syn_lost=syn_data*(Cause=="Alive"),
         syn_died=syn_data*(Cause!="Alive")) %>%
  select(id,IndexCancer,Stratum,Cause,Stage,Index,alive_start=syn_alive,lost=syn_lost,died=syn_died)

write_tsv(output_syn_life_table,sprintf("generated_data/%s_syn_cod_table.tsv", date_code))