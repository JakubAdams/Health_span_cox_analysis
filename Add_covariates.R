#Format base dataframe with covariates (age, sex,----
#BMI, socioeconomic status, educational status, physical activity (systolic blood pressure used as proxy), alcohol intake, smoking status)
#define data-cutoff date for controlled tier CDR v7 (available from
#https://support.researchallofus.org/hc/en-us/articles/360033200232-Data-Dictionaries)
data_cutoff_date <- mdy_hms("07/01/22 00:00:00")
dataset_34030956_person_df <-  dataset_34030956_person_df %>% 
  mutate(gender_filtered = grepl("Female", gender, ignore.case=TRUE) & gender==sex_at_birth,
         age_years = as.duration(data_cutoff_date-ymd_hms(date_of_birth))/dyears(1))
#append socioeconomic metadata----
dataset_34030956_person_df <- dataset_34030956_person_df %>% 
  left_join(dataset_34030956_zip_code_socioeconomic_df, join_by(person_id)) 
#append systolic blood pressure data (used to proxy for physical activity)
#append systolic blood pressure----
#Remove systolic blood pressure outliers using IQR
#calculate IQR
Q1_bp <- quantile(dataset_58659492_measurement_df$value_as_number, 0.25, na.rm=TRUE)
Q3_bp <- quantile(dataset_58659492_measurement_df$value_as_number, 0.75, na.rm=TRUE)
IQR_bp <- Q3_bp-Q1_bp
#define bounds
lower_bound_bp <- Q1_bp - 1.5*IQR_bp
upper_bound_bp <- Q3_bp + 1.5*IQR_bp
#remove outliers
dataset_58659492_measurement_df_filtered <- dataset_58659492_measurement_df %>% 
  filter(value_as_number>=lower_bound_bp & value_as_number<=upper_bound_bp) %>% 
  select(person_id, systolic_bp = value_as_number) %>% 
  group_by(person_id) %>% 
  summarize(systolic_bp = mean(systolic_bp))

#join with base dataframe
dataset_34030956_person_df <- dataset_34030956_person_df %>% 
  left_join(dataset_58659492_measurement_df_filtered, join_by(person_id))

range(dataset_66889873_measurement_df$value_as_number,na.rm=TRUE)

#append BMI----
#Remove BMI outliers using IQR
#calculate IQR
Q1_BMI <- quantile(dataset_66889873_measurement_df$value_as_number, 0.25, na.rm=TRUE)
Q3_BMI <- quantile(dataset_66889873_measurement_df$value_as_number, 0.75, na.rm=TRUE)
IQR_BMI <- Q3_BMI - Q1_BMI
#define bounds
lower_bound_BMI <- Q1_BMI - 1.5 * IQR_BMI
upper_bound_BMI <- Q3_BMI + 1.5 * IQR_BMI
#remove outliers
dataset_66889873_measurement_df_filtered <- dataset_66889873_measurement_df %>%
  filter(value_as_number >= lower_bound_BMI & value_as_number <= upper_bound_BMI) %>% 
  select(person_id, BMI=value_as_number) %>% 
  group_by(person_id) %>% 
  summarize(BMI=mean(BMI))
#check range makes sense after filtering
range(dataset_66889873_measurement_df_filtered$value_as_number)
#join with base dataframe
dataset_34030956_person_df <- dataset_34030956_person_df %>%
  left_join(dataset_66889873_measurement_df_filtered, join_by(person_id))

view(dataset_66889873_measurement_df_filtered)

#append tobacco smoking status----
dataset_91650162_survey_df_smoking <- dataset_91650162_survey_df %>% 
  filter(question=="Smoking: Smoke Frequency")
dataset_91650162_survey_df_smoking$answer <- factor(dataset_91650162_survey_df_smoking$answer)
dataset_91650162_survey_df_smoking <- dataset_91650162_survey_df_smoking %>% 
  mutate(answer_smoking_frequency = fct_recode(answer,
                           "0" = "Smoke Frequency: Not At All",
                           "2" = "Smoke Frequency: Some Days",
                           "3" = "Smoke Frequency: Every Day")) %>% 
  select(person_id, answer_smoking_frequency)
#join with base dataframe
dataset_34030956_person_df <- dataset_34030956_person_df %>% 
  left_join(dataset_91650162_survey_df_smoking, join_by(person_id))
#Use a different survey question to fill in some NAs for smoking
missing_smoking_data_persons <- dataset_34030956_person_df %>% 
  filter(is.na(answer_smoking_frequency))
missing_smoking_data_ids <- missing_smoking_data_persons$person_id
dataset_91650162_survey_df_smoking_2 <- dataset_91650162_survey_df %>% 
  filter(person_id %in% missing_smoking_data_ids & question=="Smoking: 100 Cigs Lifetime")
dataset_91650162_survey_df_smoking_2$answer <- factor(dataset_91650162_survey_df_smoking_2$answer)
dataset_91650162_survey_df_smoking_2 <- dataset_91650162_survey_df_smoking_2 %>% 
  mutate(answer_smoking_frequency_2=fct_recode(answer,
                                             "0" = "100 Cigs Lifetime: No",
                                             "1" = "100 Cigs Lifetime: Yes")) %>% 
  select(person_id, answer_smoking_frequency_2)
#join with base dataframe
dataset_34030956_person_df <- dataset_34030956_person_df %>% 
  left_join(dataset_91650162_survey_df_smoking_2, join_by(person_id))
#replace NA values from the first survey question with available values from the
#second survey question
dataset_34030956_person_df <- dataset_34030956_person_df %>% 
  mutate(answer_smoking_frequency_3 = coalesce(answer_smoking_frequency, answer_smoking_frequency_2)) %>% 
  select(-answer_smoking_frequency, -answer_smoking_frequency_2)
#append alcohol intake----
dataset_75554551_survey_df_alcohol <- dataset_75554551_survey_df %>% 
  filter(question=="Alcohol: Drink Frequency Past Year")
dataset_75554551_survey_df_alcohol$answer <- factor(dataset_75554551_survey_df_alcohol$answer)
dataset_75554551_survey_df_alcohol <- dataset_75554551_survey_df_alcohol %>% 
  mutate(answer_alcohol_intake=fct_recode(answer,
                                          "0" = "Drink Frequency Past Year: Never",
                                          "1" = "Drink Frequency Past Year: Monthly Or Less",
                                          "2" = "Drink Frequency Past Year: 2 to 4 Per Month",
                                          "3" = "Drink Frequency Past Year: 2 to 3 Per Week",
                                          "4" = "Drink Frequency Past Year: 4 or More Per Week")) %>% 
  select(person_id, answer_alcohol_intake)
#join with base dataframe
dataset_34030956_person_df <- dataset_34030956_person_df %>% 
  left_join(dataset_75554551_survey_df_alcohol, join_by(person_id))
#append educational status----
dataset_75554551_survey_df_education <- dataset_75554551_survey_df %>% 
  filter(question=="Education Level: Highest Grade")
dataset_75554551_survey_df_education$answer <- factor(dataset_75554551_survey_df_education$answer)
dataset_75554551_survey_df_education <- dataset_75554551_survey_df_education %>% 
  mutate(answer_education=fct_collapse(answer,
                                     "1" = c("Highest Grade: Advanced Degree", "Highest Grade: College Graduate"),
                                     "0" = c( "Highest Grade: College One to Three", "Highest Grade: Nine Through Eleven",
                                              "Highest Grade: Five Through Eight", "Highest Grade: One Through Four",
                                              "Highest Grade: Never Attended", "Highest Grade: Twelve Or GED"))) %>% 
  select(person_id, answer_education)
#join with base dataframe
dataset_34030956_person_df <- dataset_34030956_person_df %>% 
  left_join(dataset_75554551_survey_df_education, join_by(person_id))
#inspect base dataframe----
view(dataset_34030956_person_df)
dataset_34030956_base_df <- dataset_34030956_person_df
filename <- 'dataset_34030956_base_df'
write_excel_csv(dataset_34030956_base_df, filename)
dataset_34030956_base_df <- read_csv('dataset_34030956_base_df')
#append date of enrollment from baseline "The Basics" survey----
dataset_34030956_survey_df <- dataset_34030956_survey_df %>% 
  filter(survey=="The Basics") %>% 
  distinct(person_id, survey, .keep_all = TRUE) %>% 
  select(person_id, survey_datetime_basics=survey_datetime)
#check there are no duplicates
dataset_34030956_survey_df %>% 
  count(person_id) %>% 
  filter(n>1)
#join with base dataframe
dataset_34030956_base_df <- dataset_34030956_base_df %>% 
  left_join(dataset_34030956_survey_df, join_by(person_id))