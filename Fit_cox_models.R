#With the base dataframe formatted with the covariates and healthspan----
#append the relevant drug columns and perform Cox analysis
#First load in a list of the top active ingredients (drugs) and required packages----
library(tidyverse)
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

for(i in colnames(dataset_34030956_base_healthspan_no_cancer_df_filtered)[60:196]){
  dataset_34030956_base_healthspan_no_cancer_df_filtered[[i]] <- factor(dataset_34030956_base_healthspan_no_cancer_df_filtered[[i]])
}

#rename columns
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  rename(sodium_chloride='sodium chloride',
         calcium_chloride='calcium chloride',
         potassium_chloride='potassium chloride',
         polyethylene_glycol='polyethylene glycol',
         magnesium_sulfate='magnesium sulfate',
         magnesium_oxide='magnesium oxide',
         ferrous_sulfate='ferrous sulfate',
         acetaminophen_codeine='acetaminophen, codeine',
         magnesium_hydroxide_aluminium_hydroxide='magnesium hydroxide, aluminium hydroxide',
         codeine_guaifenesin='codeine, guaifenesin',
         folic_acid='folic acid',
         coal_tar='coal tar',
         sodium_bicarbonate_sodium_sulfate='sodium bicarbonate, sodium sulfate',
         vitamin_B12='vitamin B12',
         ascorbic_acid='ascorbic acid',
         piperacillin_tazobactam='piperacillin, tazobactam',
         interferon_alfa_2a='interferon alfa-2a')

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

for(i in colnames(dataset_34030956_base_healthspan_no_cancer_df_filtered)[60:196]){
  # Correctly insert column name into the formula
  formula <- as.formula(paste("Surv(healthspan, event_status) ~ gender_filtered +
             age_years +
             BMI +
             systolic_bp +
             answer_education +
             answer_smoking_frequency_3 +
             answer_alcohol_intake +
             deprivation_index +", i))
  
  # Fit the Cox model
  model <- coxph(formula, data = dataset_34030956_base_healthspan_no_cancer_df_filtered)
  model_results[[i]] <- model
  
  # Get the summary
  model_summary <- summary(model)
  model_summary_results[[i]] <- model_summary
  
  # Schoenfeld residuals
  model_schoenfeld_residuals[[i]] <- cox.zph(model)
  
  # Extract key statistics from the summary
  model_stats <- data.frame(
    drug = i,
    hazard_ratio = model_summary$coefficients[nrow(model_summary$coefficients), "exp(coef)"],  # Last row for the drug
    lower_CI = model_summary$conf.int[nrow(model_summary$conf.int), 3],
    upper_CI = model_summary$conf.int[nrow(model_summary$conf.int), 4],
    p_value = model_summary$coefficients[nrow(model_summary$coefficients), "Pr(>|z|)"],
    logrank_test_p = as.numeric(model_summary$logtest["pvalue"]),
    wald_test_p = as.numeric(model_summary$waldtest["pvalue"]),
    likelihood_test_p = as.numeric(model_summary$sctest["pvalue"])
  )
  
  model_summary_df <- rbind(model_summary_df, model_stats)
}

#Schoenfeld residual tests for all models----
lapply(model_schoenfeld_residuals, function(res) {
  print(res)
})
#View summary table----
view(model_summary_df)

#demo----
cox.demo <- coxph(Surv(healthspan, event_status)~gender_filtered+
                    age_years+
                    BMI+
                    systolic_bp+
                    answer_education+
                    answer_smoking_frequency_3+
                    answer_alcohol_intake+
                    deprivation_index+
                    naproxen,
                    data=dataset_34030956_base_healthspan_no_cancer_df_filtered)
summary_demo <- summary(cox.demo)
#check model assumptions
cox.zph(cox.demo)

