#The purpose of this script is to append diagnoses of age-related diseases
#and their start times to the base dataframe in order to calculate time on study
#to incident diagnosis (healthspan, taken as the miniumum time from enrollment to 
#first diagnosis of one of these diseases)
#Schizophrenia----
#save relevant dataset
filename <- 'dataset_25124167_condition_df.csv'
write_excel_csv(dataset_25124167_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_25124167_condition_df <- read_csv('dataset_25124167_condition_df.csv')
#append schizophrenia diagnoses
#remove duplicates
dataset_25124167_condition_df_schizophrenia_filtered <- dataset_25124167_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_25124167_condition_df_schizophrenia_filtered_ids <- dataset_25124167_condition_df_schizophrenia_filtered$person_id
dataset_25124167_condition_df_schizophrenia_filtered <- dataset_25124167_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_25124167_condition_df_schizophrenia_filtered_ids) %>% 
  mutate(schizophrenia=1) %>% 
  rename(condition_start_datetime_schizophrenia=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_25124167_condition_df_schizophrenia_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(schizophrenia_survival_time_years=as.duration(ymd_hms(condition_start_datetime_schizophrenia)-ymd_hms(survey_datetime_basics))/dyears(1))
#Multiple sclerosis----
#save relevant dataset
filename <- 'dataset_84978705_condition_df.csv'
write_excel_csv(dataset_84978705_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_84978705_condition_df <- read_csv('dataset_84978705_condition_df.csv')
#append multiple sclerosis diagnoses
dataset_84978705_condition_df_filtered <- dataset_84978705_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_84978705_condition_df_filtered_ids <- dataset_84978705_condition_df_filtered$person_id
dataset_84978705_condition_df_filtered <- dataset_84978705_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_84978705_condition_df_filtered_ids) %>% 
  mutate(multiple_sclerosis=1) %>% 
  rename(condition_start_datetime_multiple_sclerosis=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_84978705_condition_df_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(multiple_sclerosis_survival_time_years=as.duration(ymd_hms(condition_start_datetime_multiple_sclerosis)-ymd_hms(survey_datetime_basics))/dyears(1))
#Major depressive disorder----
#save relevant dataset
filename <- 'dataset_20634890_condition_df'
write_excel_csv(dataset_20634890_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_20634890_condition_df <- read_csv('dataset_20634890_condition_df')
#append MDD diagnoses
#remove duplicates
dataset_20634890_condition_df_MDD_filtered <- dataset_20634890_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_20634890_condition_df_MDD_filtered_ids <- dataset_20634890_condition_df_MDD_filtered$person_id
dataset_20634890_condition_df_MDD_filtered <- dataset_20634890_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_20634890_condition_df_MDD_filtered_ids) %>% 
  mutate(MDD=1) %>% 
  rename(condition_start_datetime_MDD=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_20634890_condition_df_MDD_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(MDD_survival_time_years=as.duration(ymd_hms(condition_start_datetime_MDD)-ymd_hms(survey_datetime_basics))/dyears(1))
#Systemic lupus erythematosus----
#save relevant dataset
filename <- 'dataset_48244260_condition_df.csv'
write_excel_csv(dataset_48244260_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_48244260_condition_df <- read_csv('dataset_48244260_condition_df.csv')
#append systemic lupus erythematosus diagnoses
dataset_48244260_condition_lupus_df_filtered <- dataset_48244260_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_48244260_condition_lupus_df_filtered_ids <- dataset_48244260_condition_lupus_df_filtered$person_id
dataset_48244260_condition_lupus_df_filtered <- dataset_48244260_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_48244260_condition_lupus_df_filtered_ids) %>% 
  mutate(lupus=1) %>% 
  rename(condition_start_datetime_lupus=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_48244260_condition_lupus_df_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(lupus_survival_time_years=as.duration(ymd_hms(condition_start_datetime_lupus)-ymd_hms(survey_datetime_basics))/dyears(1))

#Vascular dementia----
#save relevant dataset
filename <- 'dataset_32273011_condition_df.csv'
write_excel_csv(dataset_32273011_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_32273011_condition_df <- read_csv('dataset_32273011_condition_df.csv')
#append vascular dementia diagnoses
#remove duplicates
dataset_32273011_condition_df_vascular_dementia_filtered <- dataset_32273011_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_32273011_condition_df_vascular_dementia_filtered_ids <- dataset_32273011_condition_df_vascular_dementia_filtered$person_id
dataset_32273011_condition_df_vascular_dementia_filtered <- dataset_32273011_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_32273011_condition_df_vascular_dementia_filtered_ids) %>% 
  mutate(vascular_dementia=1) %>% 
  rename(condition_start_datetime_vascular_dementia=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_32273011_condition_df_vascular_dementia_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(vascular_dementia_survival_time_years=as.duration(ymd_hms(condition_start_datetime_vascular_dementia)-ymd_hms(survey_datetime_basics))/dyears(1))

#Amyotrophic lateral sclerosis----
#no cases as of 26/11/12
#IBD----
#save relevant dataset
filename <- 'dataset_73962798_condition_df.csv'
write_excel_csv(dataset_73962798_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_73962798_condition_df <- read_csv('dataset_73962798_condition_df.csv')
#append IBD diagnoses
dataset_73962798_condition_df_IBD_filtered <- dataset_73962798_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_73962798_condition_df_IBD_filtered_ids <- dataset_73962798_condition_df_IBD_filtered$person_id
dataset_73962798_condition_df_IBD_filtered <- dataset_73962798_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_73962798_condition_df_IBD_filtered_ids) %>% 
  mutate(IBD=1) %>% 
  rename(condition_start_datetime_IBD=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_73962798_condition_df_IBD_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(IBD_survival_time_years=as.duration(ymd_hms(condition_start_datetime_IBD)-ymd_hms(survey_datetime_basics))/dyears(1))
#Liver disease----
filename <- 'dataset_75004124_condition_df.csv'
write_excel_csv(dataset_75004124_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_75004124_condition_df <- read_csv('dataset_75004124_condition_df.csv')
#append liver disease diagnoses
dataset_75004124_condition_df_liver_disease_filtered <- dataset_75004124_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_75004124_condition_df_liver_disease_filtered_ids <- dataset_75004124_condition_df_liver_disease_filtered$person_id
dataset_75004124_condition_df_liver_disease_filtered <- dataset_75004124_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_75004124_condition_df_liver_disease_filtered_ids) %>% 
  mutate(liver_disease=1) %>% 
  rename(condition_start_datetime_liver_disease=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_75004124_condition_df_liver_disease_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(liver_disease_survival_time_years=as.duration(ymd_hms(condition_start_datetime_liver_disease)-ymd_hms(survey_datetime_basics))/dyears(1))
#Alzheimer's dementia----
#save relevant dataset
filename <- 'dataset_00598362_condition_df.csv'
write_excel_csv(dataset_00598362_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_00598362_condition_df <- read_csv('dataset_00598362_condition_df.csv')
#append Alzheimer's disease diagnoses
#remove duplicates
dataset_00598362_condition_Alzheimers_df_filtered <- dataset_00598362_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_00598362_condition_Alzheimers_df_filtered_ids <- dataset_00598362_condition_Alzheimers_df_filtered$person_id
dataset_00598362_condition_Alzheimers_df_filtered <- dataset_00598362_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_00598362_condition_Alzheimers_df_filtered_ids) %>% 
  mutate(Alzheimers=1) %>% 
  rename(condition_start_datetime_Alzheimers=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_00598362_condition_Alzheimers_df_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(Alzheimers_survival_time_years=as.duration(ymd_hms(condition_start_datetime_Alzheimers)-ymd_hms(survey_datetime_basics))/dyears(1))
#Rheumatoid arthritis----
#save relevant dataset
filename <- 'dataset_08287734_condition_df'
write_excel_csv(dataset_08287734_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_08287734_condition_df <- read_csv('dataset_08287734_condition_df')
#append rheumatoid arthritis diagnoses
#remove duplicates
dataset_08287734_condition_rheumatoid_df_filtered <- dataset_08287734_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_08287734_condition_rheumatoid_df_filtered_ids <- dataset_08287734_condition_rheumatoid_df_filtered$person_id
dataset_08287734_condition_rheumatoid_df_filtered <- dataset_08287734_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_08287734_condition_rheumatoid_df_filtered_ids) %>% 
  mutate(rheumatoid_arthritis=1) %>% 
  rename(condition_start_datetime_rheumatoid_arthritis=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_08287734_condition_rheumatoid_df_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(rheumatoid_arthritis_survival_time_years=as.duration(ymd_hms(condition_start_datetime_rheumatoid_arthritis)-ymd_hms(survey_datetime_basics))/dyears(1))






#Parkinson's disease----
#save relevant dataset
filename <- 'dataset_88546124_condition_df.csv'
write_excel_csv(dataset_88546124_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_88546124_condition_df <- read_csv('dataset_88546124_condition_df.csv')
#append Parkinson's disease diagnoses
#remove duplicates
dataset_88546124_condition_Parkinsons_df_filtered <- dataset_88546124_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_88546124_condition_Parkinsons_df_filtered_ids <- dataset_88546124_condition_Parkinsons_df_filtered$person_id
dataset_88546124_condition_Parkinsons_df_filtered <- dataset_88546124_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_88546124_condition_Parkinsons_df_filtered_ids) %>% 
  mutate(Parkinsons=1) %>% 
  rename(condition_start_datetime_Parkinsons=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_88546124_condition_Parkinsons_df_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(Parkinsons_survival_time_years=as.duration(ymd_hms(condition_start_datetime_Parkinsons)-ymd_hms(survey_datetime_basics))/dyears(1))


#Ischaemic stroke----
filename <- 'dataset_99466174_condition_df.csv'
write_excel_csv(dataset_99466174_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_99466174_condition_df <- read_csv('dataset_99466174_condition_df.csv')
#append ischaemic stroke diagnoses
#remove duplicates
dataset_99466174_condition_ischaemic_stroke_df_filtered <- dataset_99466174_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_99466174_condition_ischaemic_stroke_df_filtered_ids <- dataset_99466174_condition_ischaemic_stroke_df_filtered$person_id
dataset_99466174_condition_ischaemic_stroke_df_filtered <- dataset_99466174_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_99466174_condition_ischaemic_stroke_df_filtered_ids) %>% 
  mutate(ischaemic_stroke=1) %>% 
  rename(condition_start_datetime_ischaemic_stroke=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_99466174_condition_ischaemic_stroke_df_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(ischaemic_stroke_survival_time_years=as.duration(ymd_hms(condition_start_datetime_ischaemic_stroke)-ymd_hms(survey_datetime_basics))/dyears(1))
#COPD----
#save relevant dataset
filename <- 'dataset_74463777_condition_df.csv'
write_excel_csv(dataset_74463777_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_74463777_condition_df <- read_csv('dataset_74463777_condition_df.csv')
#append COPD diagnoses
#remove duplicates
dataset_74463777_condition_COPD_df_filtered <- dataset_74463777_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_74463777_condition_COPD_df_filtered_ids <- dataset_74463777_condition_COPD_df_filtered$person_id
dataset_74463777_condition_COPD_df_filtered <- dataset_74463777_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_74463777_condition_COPD_df_filtered_ids) %>% 
  mutate(COPD=1) %>% 
  rename(condition_start_datetime_COPD=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_74463777_condition_COPD_df_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(COPD_survival_time_years=as.duration(ymd_hms(condition_start_datetime_COPD)-ymd_hms(survey_datetime_basics))/dyears(1))

#Type II diabetes----
#save relevant dataset
filename <- 'dataset_73584556_condition_df.csv'
write_excel_csv(dataset_73584556_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_73584556_condition_df <- read_csv('dataset_73584556_condition_df.csv')
#append type II diabetes mellitus diagnoses
dataset_73584556_condition_diabetes_2_df_filtered <- dataset_73584556_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_73584556_condition_diabetes_2_df_filtered_ids <- dataset_73584556_condition_diabetes_2_df_filtered$person_id
dataset_73584556_condition_diabetes_2_df_filtered <- dataset_73584556_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_73584556_condition_diabetes_2_df_filtered_ids) %>% 
  mutate(diabetes_2=1) %>% 
  rename(condition_start_datetime_diabetes_2=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_73584556_condition_diabetes_2_df_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(diabetes_2_survival_time_years=as.duration(ymd_hms(condition_start_datetime_diabetes_2)-ymd_hms(survey_datetime_basics))/dyears(1))
#Ischaemic heart disease----
#save relevant dataset
filename <- 'dataset_64569991_condition_df.csv'
write_excel_csv(dataset_64569991_condition_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_64569991_condition_df <- read_csv('dataset_64569991_condition_df.csv')
#append ischaemic heart disease diagnoses
#remove duplicates
dataset_64569991_condition_ischaemic_heart_disease_df_filtered <- dataset_64569991_condition_df %>% 
  distinct(person_id, condition_start_datetime) %>% 
  count(person_id) %>% 
  filter(n<=1)
#store their person_ids
dataset_64569991_condition_ischaemic_heart_disease_df_filtered_ids <- dataset_64569991_condition_ischaemic_heart_disease_df_filtered$person_id
dataset_64569991_condition_ischaemic_heart_disease_df_filtered <- dataset_64569991_condition_df %>%
  distinct(person_id, condition_start_datetime) %>% 
  filter(person_id %in% dataset_64569991_condition_ischaemic_heart_disease_df_filtered_ids) %>% 
  mutate(ischaemic_heart_disease=1) %>% 
  rename(condition_start_datetime_ischaemic_heart_disease=condition_start_datetime)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_64569991_condition_ischaemic_heart_disease_df_filtered, join_by(person_id))
#calculate relevant survival time
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  mutate(ischaemic_heart_disease_survival_time_years=as.duration(ymd_hms(condition_start_datetime_ischaemic_heart_disease)-ymd_hms(survey_datetime_basics))/dyears(1))
#inspect base dataframe----
dataset_34030956_base_healthspan_no_cancer_df <- dataset_34030956_base_df
#save modified base dataframe with healthspan-related diagnoses and
#non sex-stratified
filename <- 'dataset_34030956_base_healthspan_no_cancer_df.csv'
write_excel_csv(dataset_34030956_base_healthspan_no_cancer_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_34030956_base_healthspan_no_cancer_df <- read_csv('dataset_34030956_base_healthspan_no_cancer_df.csv')

#remove left-truncated participants (those with any negative survival times)
disease_col_names <- dataset_34030956_base_healthspan_no_cancer_df %>% 
  select(contains("survival_time", ignore.case=TRUE)) %>% 
  colnames()
#code no disease (NA) as 99
for(i in disease_col_names){
  dataset_34030956_base_healthspan_no_cancer_df <- dataset_34030956_base_healthspan_no_cancer_df%>% 
    mutate(!!i := coalesce(.data[[i]], 99))
}
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df %>%
  filter(schizophrenia_survival_time_years>=0 &
         multiple_sclerosis_survival_time_years>=0 &
         MDD_survival_time_years>=0 &
         lupus_survival_time_years>=0 &
         vascular_dementia_survival_time_years>=0 &
         IBD_survival_time_years>=0 &
         liver_disease_survival_time_years>=0 &
         Alzheimers_survival_time_years>=0 &
         rheumatoid_arthritis_survival_time_years>=0 &
         Parkinsons_survival_time_years>=0 &
         ischaemic_stroke_survival_time_years>=0 &
         COPD_survival_time_years>=0 &
         diabetes_2_survival_time_years>=0 &
         ischaemic_heart_disease_survival_time_years>=0)
#for each participant, identify the shortest survival time (healthspan)
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>%
  mutate(healthspan=pmin(schizophrenia_survival_time_years,
                         multiple_sclerosis_survival_time_years,
                         MDD_survival_time_years,
                         lupus_survival_time_years,
                         vascular_dementia_survival_time_years,
                         IBD_survival_time_years,
                         liver_disease_survival_time_years,
                         Alzheimers_survival_time_years,
                         rheumatoid_arthritis_survival_time_years,
                         Parkinsons_survival_time_years,
                         ischaemic_stroke_survival_time_years,
                         COPD_survival_time_years,
                         diabetes_2_survival_time_years,
                         ischaemic_heart_disease_survival_time_years,
                         na.rm=TRUE))
#code 99 as 0 (right-censored) in the healthspan column
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>%
  mutate(healthspan = ifelse(dataset_34030956_base_healthspan_no_cancer_df_filtered$healthspan == 99, 0, healthspan))
#quality control (this person has pre-existing vascular dementia and a negative survival time for it)
object <- dataset_34030956_base_healthspan_no_cancer_df %>% 
  filter(person_id==1818578)
#save dataframe----
#this is the base dataframe, with survey datetime, disease diagnoses and their survival times,
#and healthspan. Non-sex stratified. 
filename <- 'dataset_34030956_base_healthspan_no_cancer_df_filtered.csv'
write_excel_csv(dataset_34030956_base_healthspan_no_cancer_df_filtered, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_34030956_base_healthspan_no_cancer_df_filtered <- read_csv('dataset_34030956_base_healthspan_no_cancer_df_filtered.csv')
