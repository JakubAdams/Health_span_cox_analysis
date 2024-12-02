#The purpose of this script is to idnetify participants on long-term drug therapy
#Load in datasets----
dataset_34030956_zip_code_socioeconomic_df <- read_csv('dataset_34030956_zip_code_socioeconomic_df')
dataset_34030956_drug_df <- read_csv('dataset_34030956_drug_df')
dataset_34030956_person_df <- read_csv('dataset_34030956_person_df')
dataset_34030956_survey_df <- read_csv('dataset_34030956_survey_df')
dataset_34030956_observation_df <- read_csv('dataset_34030956_observation_df')
dataset_34030956_measurement_df <- read_csv('dataset_34030956_measurement_df')
#BMI
dataset_66889873_measurement_df <- read_csv('dataset_66889873_measurement_df.csv')
#smoking
dataset_75554551_survey_df <- read_csv('dataset_75554551_survey_df.csv')
#systolic bp
dataset_58659492_measurement_df <- read_csv('dataset_58659492_measurement_df.csv')
#smoking
dataset_91650162_survey_df <- read_csv('dataset_91650162_survey_df.csv')
#base dataframe
dataset_34030956_base_df <- read_csv('dataset_34030956_base_df')
#Extract unique person_ids of participants on long-term drug therapy (ascertained from EHR data)----
dataset_34030956_observation_df_long_term_drug_users <- 
  dataset_34030956_observation_df %>% 
  filter(grepl("Long-term current use of drug therapy", standard_concept_name, ignore.case=TRUE))
long_term_drug_user_person_ids <- unique(dataset_34030956_observation_df_long_term_drug_users$person_id)

#Subset the drugs dataframe for only those on long-term drug therapy----
dataset_34030956_drug_df_long_term_drug_users <- 
  dataset_34030956_drug_df %>% 
  filter(person_id %in% long_term_drug_user_person_ids) %>% 
  distinct(person_id, standard_concept_name, .keep_all = TRUE)

#Add columns with active ingredients as indicator variables----
active_ingredients <- c("metformin", "methotrexate")
for(col_name in active_ingredients){
  dataset_34030956_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_34030956_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}
#Check #unique participants
dataset_34030956_drug_df_long_term_drug_users <- dataset_34030956_drug_df_long_term_drug_users %>% 
  group_by(person_id) %>% 
  summarize(n=n())
#or
n_distinct(dataset_34030956_drug_df_long_term_drug_users$person_id)
#both of the above should be equal (to 10868)

#Collapse down to one row per unique participant (or more specifically, one----
#per unique person_id, drugs in active_ingredients list combination)
dataset_34030956_drug_df_long_term_drug_users <- dataset_34030956_drug_df_long_term_drug_users %>% 
distinct(person_id, pick(active_ingredients), .keep_all = TRUE)
#check this unique participant taking metformin has only one row (they have eight initially
#due to multiple metformin matches)
participant_1941954 <- dataset_34030956_drug_df_long_term_drug_users %>% 
  filter(person_id==1941954)

#identify participants with polypharmacy and (optionally)----
#remove them from the final subset drugs dataframe
participants_with_polypharmacy <- dataset_34030956_drug_df_long_term_drug_users %>% 
  count(person_id) %>% 
  filter(n>1)
participants_with_polypharmacy_person_ids <- participants_with_polypharmacy$person_id
dataset_34030956_drug_df_long_term_drug_users <- dataset_34030956_drug_df_long_term_drug_users %>% 
  filter(!person_id %in% participants_with_polypharmacy$person_id)
#check #rows has decreased since last viewing dataset_34030956_drug_df_long_term_drug_users
#and optionally count (in this case the result should be < 10868)
n_distinct(dataset_34030956_drug_df_long_term_drug_users)
dataset_34030956_drug_df_long_term_drug_users %>%
  count(person_id)
#join with base dataframe----
dataset_34030956_drug_df_long_term_drug_users <- dataset_34030956_drug_df_long_term_drug_users %>% 
  select(person_id, all_of(active_ingredients))

dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  left_join(dataset_34030956_drug_df_long_term_drug_users, join_by(person_id)) 
#replace NA values in the drugs column with "FALSE", indicating no treatment
for(col_name in active_ingredients){
  dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  mutate(!!col_name := coalesce(.data[[col_name]], FALSE))
}
view(dataset_34030956_base_healthspan_no_cancer_df_filtered)
#remove participants with polypharmacy at the end----
remove(dataset_34030956_drug_df_long_term_drug_users)
sort(dataset_34030956_drug_df_long_term_drug_users$person_id)
view(dataset_34030956_drug_df_long_term_drug_users)