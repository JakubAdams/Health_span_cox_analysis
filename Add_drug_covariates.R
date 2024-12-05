#With the base dataframe formatted with the covariates and healthspan----
#append the relevant drug columns and perform Cox analysis
#identify unique active ingredients----
all_active_ingredients <- data.frame(active_ingredients=unique(top_drugs_df$active_ingredients))
#Sodium chloride-fluticasone----
filename <- 'dataset_08934972_drug_df.csv'
write_excel_csv(dataset_08934972_drug_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_08934972_drug_df <- read_csv('dataset_08934972_drug_df.csv')

#Subset the drugs dataframe for only those on long-term drug therapy
dataset_08934972_drug_df_long_term_drug_users <- 
  dataset_08934972_drug_df %>% 
  filter(person_id %in% long_term_drug_user_person_ids) 

#Add columns with active ingredients as indicator variables
active_ingredients_batch_1 <- c("sodium chloride", "calcium chloride", "potassium chloride", "lactate", "acetaminophen", "oxycodone", "midazolam", "hydrocodone", "ibuprofen", "lidocaine", "fentanyl", "ondansetron", "docusate", "fluticasone")
for(col_name in active_ingredients_batch_1){
  dataset_08934972_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_08934972_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_08934972_drug_df_long_term_drug_users <- dataset_08934972_drug_df_long_term_drug_users %>% 
  group_by(person_id) %>%
  summarize(across(everything(), ~ any(. == TRUE, na.rm = TRUE))) %>% 
  select(person_id, all_of(active_ingredients_batch_1))

#join with base dataframe
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  left_join(dataset_08934972_drug_df_long_term_drug_users, join_by(person_id)) 
#replace NA values in the drugs column with "FALSE", indicating no treatment
for(col_name in active_ingredients_batch_1){
  dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
    mutate(!!col_name := coalesce(.data[[col_name]], FALSE))
}
view(dataset_34030956_base_healthspan_no_cancer_df_filtered)

object_1 <- dataset_34030956_base_healthspan_no_cancer_df_filtered
#Fluticasone-dexamethasone----
filename <- 'dataset_87187148_drug_df.csv'
write_excel_csv(dataset_87187148_drug_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_87187148_drug_df <- read_csv('dataset_87187148_drug_df.csv')

#Subset the drugs dataframe for only those on long-term drug therapy
dataset_87187148_drug_df_long_term_drug_users <- 
  dataset_87187148_drug_df %>% 
  filter(person_id %in% long_term_drug_user_person_ids)

#Add columns with active ingredients as indicator variables
active_ingredients_batch_2 <- c("cefazolin", "omeprazole", "tramadol", "gabapentin", "rocuronium", "prednisone", "cephalexin", "polyethylene glycol", "diphenhydramine", "pantoprazole", "amoxicillin", "dexamethasone")
for(col_name in active_ingredients_batch_2){
  dataset_87187148_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_87187148_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_87187148_drug_df_long_term_drug_users <- dataset_87187148_drug_df_long_term_drug_users %>% 
  group_by(person_id) %>%
  summarize(across(everything(), ~ any(. == TRUE, na.rm = TRUE))) %>% 
  select(person_id, all_of(active_ingredients_batch_2))

#join with base dataframe
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  left_join(dataset_87187148_drug_df_long_term_drug_users, join_by(person_id)) 
#replace NA values in the drugs column with "FALSE", indicating no treatment
for(col_name in active_ingredients_batch_2){
  dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
    mutate(!!col_name := coalesce(.data[[col_name]], FALSE))
}
object_2 <- dataset_34030956_base_healthspan_no_cancer_df_filtered
#Dexamethasone-azithromycin----
filename <- 'dataset_14233649_drug_df.csv'
write_excel_csv(dataset_14233649_drug_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_14233649_drug_df <- read_csv('dataset_14233649_drug_df.csv')

#Subset the drugs dataframe for only those on long-term drug therapy
dataset_14233649_drug_df_long_term_drug_users <- 
  dataset_14233649_drug_df %>% 
  filter(person_id %in% long_term_drug_user_person_ids)

#Add columns with active ingredients as indicator variables
active_ingredients_batch_3 <- c("cyclobenzaprine","sulfamethoxazole", "trimethoprim", "ciprofloxacin",   
                                "ketorolac", "naloxone", "hydromorphone", "aspirin",         
                                "metoclopramide", "famotidine", "propofol", "naproxen",        
                                "albuterol", "bisacodyl", "azithromycin")
for(col_name in active_ingredients_batch_3){
  dataset_14233649_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_14233649_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_14233649_drug_df_long_term_drug_users <- dataset_14233649_drug_df_long_term_drug_users %>% 
  group_by(person_id) %>%
  summarize(across(everything(), ~ any(. == TRUE, na.rm = TRUE))) %>% 
  select(person_id, all_of(active_ingredients_batch_3))

#join with base dataframe
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  left_join(dataset_14233649_drug_df_long_term_drug_users, join_by(person_id)) 
#replace NA values in the drugs column with "FALSE", indicating no treatment
for(col_name in active_ingredients_batch_3){
  dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
    mutate(!!col_name := coalesce(.data[[col_name]], FALSE))
}
object_3 <- dataset_34030956_base_healthspan_no_cancer_df_filtered
#Azithromycin-ephedrin----
filename <- 'dataset_25391995_drug_df.csv'
write_excel_csv(dataset_25391995_drug_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_25391995_drug_df <- read_csv('dataset_25391995_drug_df.csv')

#Subset the drugs dataframe for only those on long-term drug therapy
dataset_25391995_drug_df_long_term_drug_users <- 
  dataset_25391995_drug_df %>% 
  filter(person_id %in% long_term_drug_user_person_ids) 

#Add columns with active ingredients as indicator variables
active_ingredients_batch_4 <- c( "sennosides", "amlodipine", "benzonatate",        
                                 "atorvastatin", "ceftriaxone", "diclofenac",         
                                 "heparin", "magnesium sulfate", "metronidazole",      
                                 "trazodone", "promethazine", "hydralazine",        
                                 "neostigmine", "glycopyrronium", "hydrochlorothiazide",
                                 "ephedrin")
for(col_name in active_ingredients_batch_4){
  dataset_25391995_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_25391995_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_25391995_drug_df_long_term_drug_users <- dataset_25391995_drug_df_long_term_drug_users %>% 
  group_by(person_id) %>%
  summarize(across(everything(), ~ any(. == TRUE, na.rm = TRUE))) %>% 
  select(person_id, all_of(active_ingredients_batch_4))

#join with base dataframe
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  left_join(dataset_25391995_drug_df_long_term_drug_users, join_by(person_id)) 
#replace NA values in the drugs column with "FALSE", indicating no treatment
for(col_name in active_ingredients_batch_4){
  dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
    mutate(!!col_name := coalesce(.data[[col_name]], FALSE))
}
object_4 <- dataset_34030956_base_healthspan_no_cancer_df_filtered
object_4 <- object_4 %>% 
  select(-metformin)
#Ephedrin-codeine----
filename <- 'dataset_67347389_drug_df.csv'
write_excel_csv(dataset_67347389_drug_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_67347389_drug_df <- read_csv('dataset_67347389_drug_df.csv')

#Subset the drugs dataframe for only those on long-term drug therapy
dataset_67347389_drug_df_long_term_drug_users <- 
  dataset_67347389_drug_df %>% 
  filter(person_id %in% long_term_drug_user_person_ids) 

#Add columns with active ingredients as indicator variables
active_ingredients_batch_5 <- c("doxycycline", "ergocalciferol", "iohexol",               
                                "mupirocin", "loratadine", "nitrofurantoin",        
                                "triamcinolone", "lisinopril", "cholecalciferol",       
                                "methylprednisolone", "cetirizine", "magnesium oxide",       
                                "metformin", "meperidine", "morphine",              
                                "tamsulosin", "ipratropium", "ferrous sulfate",       
                                "fluconazole", "metoprolol", "acetaminophen, codeine" )
for(col_name in active_ingredients_batch_5){
  dataset_67347389_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_67347389_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_67347389_drug_df_long_term_drug_users <- dataset_67347389_drug_df_long_term_drug_users %>% 
  group_by(person_id) %>%
  summarize(across(everything(), ~ any(. == TRUE, na.rm = TRUE))) %>% 
  select(person_id, all_of(active_ingredients_batch_5))

#join with base dataframe
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  left_join(dataset_67347389_drug_df_long_term_drug_users, join_by(person_id)) 
#replace NA values in the drugs column with "FALSE", indicating no treatment
for(col_name in active_ingredients_batch_5){
  dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
    mutate(!!col_name := coalesce(.data[[col_name]], FALSE))
}
object_5 <- dataset_34030956_base_healthspan_no_cancer_df_filtered
#Codeine-coal tar----
filename <- 'dataset_50573487_drug_df.csv'
write_excel_csv(dataset_50573487_drug_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_50573487_drug_df <- read_csv('dataset_50573487_drug_df.csv')

#Subset the drugs dataframe for only those on long-term drug therapy
dataset_50573487_drug_df_long_term_drug_users <- 
  dataset_50573487_drug_df %>% 
  filter(person_id %in% long_term_drug_user_person_ids) 

#Add columns with active ingredients as indicator variables
active_ingredients_batch_6 <- c("enoxaparin",                                           
                                "lorazepam",                                            
                                "ranitidine",                                           
                                "labetalol",                                            
                                "succinylcholine",                                      
                                "phenylephrine",                                        
                                "diazepam",                                             
                                "magnesium hydroxide, aluminium hydroxide",
                                "nitroglycerin",                                        
                                "meloxicam",                                            
                                "zolpidem",                                             
                                "vancomycin",                                           
                                "chlorhexidine",                                       
                                "prednisolone",                                         
                                "furosemide",                                           
                                "codeine, guaifenesin",                                 
                                "prochlorperazine",                                     
                                "clavulanate",                             
                                "folic acid",                                           
                                "glucose",                                              
                                "bupivacaine",                                          
                                "montelukast",                                          
                                "coal tar")
for(col_name in active_ingredients_batch_6){
  dataset_50573487_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_50573487_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_50573487_drug_df_long_term_drug_users <- dataset_50573487_drug_df_long_term_drug_users %>% 
  group_by(person_id) %>%
  summarize(across(everything(), ~ any(. == TRUE, na.rm = TRUE))) %>% 
  select(person_id, all_of(active_ingredients_batch_6))

#join with base dataframe
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  left_join(dataset_50573487_drug_df_long_term_drug_users, join_by(person_id)) 
#replace NA values in the drugs column with "FALSE", indicating no treatment
for(col_name in active_ingredients_batch_6){
  dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
    mutate(!!col_name := coalesce(.data[[col_name]], FALSE))
}
object_6 <- dataset_34030956_base_healthspan_no_cancer_df_filtered
#Coal tar-sumatriptan----
filename <- 'dataset_49515747_drug_df.csv'
write_excel_csv(dataset_49515747_drug_df, filename)
system(paste0("gsutil cp ./", filename, " ", my_bucket, "/data/"), intern=T)
dataset_49515747_drug_df <- read_csv('dataset_49515747_drug_df.csv')

#Subset the drugs dataframe for only those on long-term drug therapy
dataset_49515747_drug_df_long_term_drug_users <- 
  dataset_49515747_drug_df %>% 
  filter(person_id %in% long_term_drug_user_person_ids) 

#Add columns with active ingredients as indicator variables
active_ingredients_batch_7 <- c("losartan",                                                                                    
                                "sertraline",                                                                                  
                                "levofloxacin",                                                                                
                                "cromolyn",                                                                                    
                                "erythromycin",                                                                                
                                "insulin",                                                                                    
                                "clindamycin",                                                                                 
                                "melatonin",                                                                                   
                                "hydroxyzine",                                                                                 
                                "sodium bicarbonate, sodium sulfate",
                                "meclizine",                                                                                   
                                "methocarbamol",                                                                               
                                "iopamidol",                                                                                   
                                "valacyclovir",                                                                                
                                "vitamin B12",                                                                                 
                                "glucagon",                                                                                    
                                "ketoconazole",                                                                                
                                "celecoxib",                                                                                   
                                "scopolamine",                                                                                 
                                "haloperidol",                                                                                 
                                "simvastatin",                                                                                 
                                "ascorbic acid",                                                                               
                                "hydrocortisone",                                                                              
                                "clopidogrel",                                                                                 
                                "baclofen",                                                                                    
                                "oseltamivir",                                                                                 
                                "clotrimazole",                                                                                
                                "escitalopram",                                                                                
                                "piperacillin, tazobactam",                                                                    
                                "interferon alfa-2a",                                                                          
                                "fluoxetine",                                                                                  
                                "citalopram",                                                                                  
                                "duloxetine",                                                                                  
                                "Multivitamin",                                                                    
                                "clonazepam",                                                                                  
                                "sumatriptan" )
for(col_name in active_ingredients_batch_7){
  dataset_49515747_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_49515747_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_49515747_drug_df_long_term_drug_users <- dataset_49515747_drug_df_long_term_drug_users %>% 
  group_by(person_id) %>%
  summarize(across(everything(), ~ any(. == TRUE, na.rm = TRUE))) %>% 
  select(person_id, all_of(active_ingredients_batch_7))

#join with base dataframe
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  left_join(dataset_49515747_drug_df_long_term_drug_users, join_by(person_id)) 
#replace NA values in the drugs column with "FALSE", indicating no treatment
for(col_name in active_ingredients_batch_7){
  dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
    mutate(!!col_name := coalesce(.data[[col_name]], FALSE))
}
object_7 <- dataset_34030956_base_healthspan_no_cancer_df_filtered