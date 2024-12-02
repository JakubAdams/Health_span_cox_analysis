#Run Cox proportional hazards models between each drug, covariates and healthspan as the event
#With the base dataframe formatted with the covariates and healthspan----
#append the relevant drug columns and perform Cox analysis
#First load in a list of the top active ingredients (drugs) and required packages----
library(survival)
top_drugs_df <- read_csv('Top_drugs.csv')
#add column with event status
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>%
  mutate(event_status=ifelse(healthspan==0, 0, 1))
#filter those whose gender identity does not match sex at birth. Code female as 1; male as 0
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>%
  filter(gender==sex_at_birth) %>% 
  mutate(gender_filtered = ifelse(grepl("Female", sex_at_birth, ignore.case=TRUE), 1, 0))

#specify covariates as factors
dataset_34030956_base_healthspan_no_cancer_df_filtered$answer_education <- factor(dataset_34030956_base_healthspan_no_cancer_df_filtered$answer_education)
dataset_34030956_base_healthspan_no_cancer_df_filtered$gender_filtered <- factor(dataset_34030956_base_healthspan_no_cancer_df_filtered$gender_filtered)
dataset_34030956_base_healthspan_no_cancer_df_filtered$answer_smoking_frequency_3 <- factor(dataset_34030956_base_healthspan_no_cancer_df_filtered$answer_smoking_frequency_3)
dataset_34030956_base_healthspan_no_cancer_df_filtered$answer_alcohol_intake <- factor(dataset_34030956_base_healthspan_no_cancer_df_filtered$answer_alcohol_intake)
dataset_34030956_base_healthspan_no_cancer_df_filtered$metformin <- factor(dataset_34030956_base_healthspan_no_cancer_df_filtered$metformin)
dataset_34030956_base_healthspan_no_cancer_df_filtered$methotrexate <- factor(dataset_34030956_base_healthspan_no_cancer_df_filtered$methotrexate)

#(optionally) remove those with polypharmacy----
object <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>%
  count(person_id) %>% 
  filter(n>1)
polypharmacy_person_ids <- object$person_id

dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  filter(!person_id %in% polypharmacy_person_ids )
#Define final active ingredients----
final_active_ingredients <- c(active_ingredients_batch_1, 
                              active_ingredients_batch_2,
                              active_ingredients_batch_3,
                              active_ingredients_batch_4,
                              active_ingredients_batch_5,
                              active_ingredients_batch_7,
                              active_ingredients_batch_8)
#Run Cox proportional hazards models----
#initialise lists in which to store model results
model_results <- list()
model_summary_results <- list()
model_schoenfeld_residuals <- list()
model_summary_df <- data.frame()
model_stats <- data.frame()
for(i in final_active_ingredients){
  formula <- as.formula(paste("Surv(healthspan, event_status)~", "gender_filtered+
             age_years+
             BMI+
             answer_education+
             answer_smoking_frequency_3+
             answer_alcohol_intake+
             deprivation_index+", i))
  model <- coxph(formula, data=dataset_34030956_base_healthspan_no_cancer_df_filtered)
  model_results[[i]] <- model
  
  model_summary <- summary(model)
  model_summary_results[[i]] <- model_summary
  
  model_schoenfeld_residuals[[i]] <- cox.zph(model)
  
  model_stats <- data.frame(
    drug=i,
    hazard_ratio=model_summary$coefficients[13, "exp(coef)"],
    lower_CI=model_summary$conf.int[13,3],
    upper_CI=model_summary$conf.int[13,4],
    p_value=model_summary$coefficients[13,5],
    logrank_test_p=as.numeric(model_summary$logtest["pvalue"]),
    wald_test_p=as.numeric(model_summary$waldtest["pvalue"]),
    likelihood_test_p=as.numeric(model_summary$sctest["pvalue"]))

  
  
  model_summary_df <- rbind(model_summary_df, model_stats)
}

#Schoenfeld residual tests for all models----
lapply(model_schoenfeld_residuals, function(res) {
  print(res)
})
#View summary table----
view(model_summary_df)

#demo----
object_demo <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  group_by(person_id)
cox.demo <- coxph(Surv(healthspan, event_status)~gender_filtered+
                    age_years+
                    BMI+
                    systolic_bp+
                    answer_education+
                    answer_smoking_frequency_3+
                    answer_alcohol_intake+
                    deprivation_index+
                    cephalexin,
                    data=object_demo)
summary_demo <- summary(cox.demo)
#check model assumptions
cox.zph(cox.demo)