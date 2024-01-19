#read in everything together and reprocess
#https://docs.google.com/spreadsheets/d/1nfQguuRQzb_-jnKIQHtbj4tlGkKFy4jzpDh5LKUg32E/edit#gid=268448422
#downloaded as excel to maintain portability
#portal
sheet_path<-"../2023_COD_MS/data/Life tables by index site (incl sub)-COD-stage SEER 17 2006-2010 20230419.xlsx"
sheet_path<-"data/Life tables by index site (incl sub)-COD-stage SEER 17 2006-2010 20230427.xlsx"

tmp_sheets<-excel_sheets(sheet_path)

sheet_set<-tmp_sheets
sheet_set<-sheet_set[!grepl("Specs",sheet_set)]

all_data_sheets<-sapply(sheet_set,function(zz){
  tmp_life_table_cod<-read_excel(sheet_path,
                                 sheet=zz,
                                 range=cell_limits(c(1,1),c(110000,9)), #"A1:I110000",
                                 col_names=FALSE,
                                 col_types="text") 
},simplify=FALSE)

all_data_df<-bind_rows(all_data_sheets,.id="id")

names(all_data_df)<-c("id", "Index","time","alive_start","died","lost","css_i","css_c","se_i","se_c")

all_life_table_data<-all_data_df %>%
  mutate(Istar=case_when(grepl("/",Index)~Index,
                         TRUE ~ NA_character_)) %>%
  fill(Istar) %>%
  filter(grepl(" yr",time) & !grepl("more",time) & !grepl("edian",time)) %>%
  type_convert() %>%
  mutate(css_i=parse_number(css_i)/100,
         css_c=parse_number(css_c)/100,
         se_i=parse_number(se_i)/100,
         se_c=parse_number(se_c)/100) %>%
  select(id, Istar,Index,time,alive_start,died,lost,css_i,css_c,se_i,se_c) %>%
  mutate(Istar=gsub("Oral/Pharynx","Oral-Pharynx",Istar)) %>%
  mutate(Istar=gsub("Brain/Nervous","Brain-Nervous",Istar)) %>%
  mutate(Istar=gsub("Pneumonia/Influenza","Pneumonia-Influenza",Istar)) %>%
  mutate(Istar=gsub("Accident/External","Accident-External",Istar)) %>%
  mutate(Istar=gsub("Nephritis/Nephrosis","Nephritis-Nephrosis",Istar)) %>%
  mutate(Istar=gsub("Suicide/Self-Injury","Suicide-Self-Injury",Istar)) %>%
  mutate(Istar=gsub("Colon/Rectum","Colon-Rectum",Istar)) %>%
  mutate(Istar=gsub("Liver/Bile","Liver-Bile",Istar)) %>%
  separate(Istar,into=c("First","Second","Third"),sep="/",extra="merge") %>%
  mutate(Stage=Third,
         Stratum=Second,
         IndexCancer=First)

all_life_table_data_fixed<-all_life_table_data %>% 
  select(id, IndexCancer,Stratum,Stage,Index,alive_start,died,lost)

write_tsv(all_life_table_data_fixed,sprintf("generated_data/%s_ms_all_life_table_data.tsv",date_code))
