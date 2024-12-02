#The purpose of this script is to add indicator variable columns for each unique active ingredient
#to the base dataset so that Cox analysis may be performed 
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
  filter(person_id %in% long_term_drug_user_person_ids) %>% 
  distinct(person_id, standard_concept_name, .keep_all = TRUE)

#Add columns with active ingredients as indicator variables
active_ingredients_batch_1 <- c("sodium_chloride", "calcium_chloride", "potassium_chloride", "lactate", "acetaminophen", "oxycodone", "midazolam", "hydrocodone", "ibuprofen", "lidocaine", "fentanyl", "ondansetron", "docusate", "fluticasone")
print(top_drugs_df$active_ingredients)
for(col_name in active_ingredients_batch_1){
  dataset_08934972_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_08934972_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_08934972_drug_df_long_term_drug_users <- dataset_08934972_drug_df_long_term_drug_users %>% 
  distinct(person_id, pick(active_ingredients_batch_1), .keep_all = TRUE)

#join with base dataframe
dataset_08934972_drug_df_long_term_drug_users <- dataset_08934972_drug_df_long_term_drug_users %>% 
  select(person_id, all_of(active_ingredients_batch_1))

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
  filter(person_id %in% long_term_drug_user_person_ids) %>% 
  distinct(person_id, standard_concept_name, .keep_all = TRUE)

#Add columns with active ingredients as indicator variables
active_ingredients_batch_2 <- c("cefazolin", "omeprazole", "tramadol", "gabapentin", "rocuronium", "prednisone", "cephalexin", "polyethylene_glycol", "diphenhydramine", "pantoprazole", "amoxicillin", "dexamethasone")
for(col_name in active_ingredients_batch_2){
  dataset_87187148_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_87187148_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_87187148_drug_df_long_term_drug_users <- dataset_87187148_drug_df_long_term_drug_users %>% 
  distinct(person_id, pick(active_ingredients_batch_2), .keep_all = TRUE)

#join with base dataframe
dataset_87187148_drug_df_long_term_drug_users <- dataset_87187148_drug_df_long_term_drug_users %>% 
  select(person_id, all_of(active_ingredients_batch_2))

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
  filter(person_id %in% long_term_drug_user_person_ids) %>% 
  distinct(person_id, standard_concept_name, .keep_all = TRUE)

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
  distinct(person_id, pick(active_ingredients_batch_3), .keep_all = TRUE)

#join with base dataframe
dataset_14233649_drug_df_long_term_drug_users <- dataset_14233649_drug_df_long_term_drug_users %>% 
  select(person_id, all_of(active_ingredients_batch_3))

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
  filter(person_id %in% long_term_drug_user_person_ids) %>% 
  distinct(person_id, standard_concept_name, .keep_all = TRUE)

#Add columns with active ingredients as indicator variables
active_ingredients_batch_4 <- c( "senosides", "amlodipine", "benzonatate",        
                                 "atorvastatin", "ceftriaxone", "diclofenac",         
                                 "heparin", "magnesium_sulfate", "metronidazole",      
                                 "trazodone", "promethazine", "hydralazine",        
                                 "neostigmine", "glycopyrronium", "hydrochlorothiazide",
                                 "ephedrin")
for(col_name in active_ingredients_batch_4){
  dataset_25391995_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_25391995_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_25391995_drug_df_long_term_drug_users <- dataset_25391995_drug_df_long_term_drug_users %>% 
  distinct(person_id, pick(active_ingredients_batch_4), .keep_all = TRUE)

#join with base dataframe
dataset_25391995_drug_df_long_term_drug_users <- dataset_25391995_drug_df_long_term_drug_users %>% 
  select(person_id, all_of(active_ingredients_batch_4))

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
  filter(person_id %in% long_term_drug_user_person_ids) %>% 
  distinct(person_id, standard_concept_name, .keep_all = TRUE)

#Add columns with active ingredients as indicator variables
active_ingredients_batch_5 <- c("doxycycline", "ergocalciferol", "iohexol",               
                                "mupirocin", "loratadine", "nitrofurantoin",        
                                "triamcinolone", "lisinopril", "cholecalciferol",       
                                "methylprednisolone", "cetirizine", "magnesium_oxide",       
                                "metformin", "meperidine", "morphine",              
                                "tamsulosin", "albuterol", "ipratropium", "ferrous_sulfate",       
                                "fluconazole", "metoprolol", "acetaminophen, codeine" )
for(col_name in active_ingredients_batch_5){
  dataset_67347389_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_67347389_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_67347389_drug_df_long_term_drug_users <- dataset_67347389_drug_df_long_term_drug_users %>% 
  distinct(person_id, pick(active_ingredients_batch_5), .keep_all = TRUE)

#join with base dataframe
dataset_67347389_drug_df_long_term_drug_users <- dataset_67347389_drug_df_long_term_drug_users %>% 
  select(person_id, all_of(active_ingredients_batch_5))

object_8 <- object_4 %>% 
  left_join(dataset_67347389_drug_df_long_term_drug_users, join_by(person_id))
object_8 <- object_8 %>% 
  rename(albuterol=albuterol.x)
dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  left_join(dataset_67347389_drug_df_long_term_drug_users, join_by(person_id)) 
#replace NA values in the drugs column with "FALSE", indicating no treatment
for(col_name in active_ingredients_batch_5){
  object_8 <- object_8 %>% 
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
  filter(person_id %in% long_term_drug_user_person_ids) %>% 
  distinct(person_id, standard_concept_name, .keep_all = TRUE)

#Add columns with active ingredients as indicator variables
active_ingredients_batch_7 <- c("enoxaparin",                                           
                                "lorazepam",                                            
                                "ranitidine",                                           
                                "labetalol",                                            
                                "succinylcholine",                                      
                                "phenylephrine",                                        
                                "diazepam",                                             
                                "magnesium_hydroxide",
                                "aluminium_hydroxide",
                                "nitroglycerin",                                        
                                "meloxicam",                                            
                                "zolpidem",                                             
                                "vancomycin",                                           
                                "chlorhexidine",                                       
                                "prednisolone",                                         
                                "furosemide",                                           
                                "guaifenesin",                                 
                                "prochlorperazine",                                     
                                "clavulanate",                             
                                "folic_acid",                                           
                                "glucose",                                              
                                "bupivacaine",                                          
                                "montelukast",                                          
                                "coal_tar")
print(top_drugs_df$active_ingredients)
for(col_name in active_ingredients_batch_7){
  dataset_50573487_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_50573487_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_50573487_drug_df_long_term_drug_users <- dataset_50573487_drug_df_long_term_drug_users %>% 
  distinct(person_id, pick(active_ingredients_batch_7), .keep_all = TRUE)

#join with base dataframe
dataset_08934972_drug_df_long_term_drug_users <- dataset_50573487_drug_df_long_term_drug_users %>% 
  select(person_id, all_of(active_ingredients_batch_7))

dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  left_join(dataset_50573487_drug_df_long_term_drug_users, join_by(person_id)) 
#replace NA values in the drugs column with "FALSE", indicating no treatment
for(col_name in active_ingredients_batch_7){
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
  filter(person_id %in% long_term_drug_user_person_ids) %>% 
  distinct(person_id, standard_concept_name, .keep_all = TRUE)

#Add columns with active ingredients as indicator variables
active_ingredients_batch_8 <- c("losartan",                                                                                    
                                "sertraline",                                                                                  
                                "levofloxacin",                                                                                
                                "cromolyn",                                                                                    
                                "erythromycin",                                                                                
                                "insulin",                                                                                    
                                "clindamycin",                                                                                 
                                "melatonin",                                                                                   
                                "hydroxyzine",                                                                                 
                                "sodium_bicarbonate",
                                "sodium_sulfate",
                                "meclizine",                                                                                   
                                "methocarbamol",                                                                               
                                "iopamidol",                                                                                   
                                "valacyclovir",                                                                                
                                "vitamin_B12",                                                                                 
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
                                "piperacillin",
                                "tazobactam",                                                                    
                                "interferon_alfa-2a",                                                                          
                                "fluoxetine",                                                                                  
                                "citalopram",                                                                                  
                                "duloxetine",                                                                                  
                                "Multivitamin",                                                                    
                                "clonazepam",                                                                                  
                                "sumatriptan" )
print(top_drugs_df$active_ingredients)
for(col_name in active_ingredients_batch_8){
  dataset_49515747_drug_df_long_term_drug_users[[col_name]] <- grepl(col_name, dataset_49515747_drug_df_long_term_drug_users$standard_concept_name, ignore.case=TRUE)
}

#Collapse down to one row per unique participant (or more specifically, one
#per unique person_id, drugs in active_ingredients list combination)
dataset_49515747_drug_df_long_term_drug_users <- dataset_49515747_drug_df_long_term_drug_users %>% 
  distinct(person_id, pick(active_ingredients_batch_8), .keep_all = TRUE)

#join with base dataframe
dataset_49515747_drug_df_long_term_drug_users <- dataset_49515747_drug_df_long_term_drug_users %>% 
  select(person_id, all_of(active_ingredients_batch_8))

dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
  left_join(dataset_49515747_drug_df_long_term_drug_users, join_by(person_id)) 
#replace NA values in the drugs column with "FALSE", indicating no treatment
for(col_name in active_ingredients_batch_8){
  dataset_34030956_base_healthspan_no_cancer_df_filtered <- dataset_34030956_base_healthspan_no_cancer_df_filtered %>% 
    mutate(!!col_name := coalesce(.data[[col_name]], FALSE))
}
object_7 <- dataset_34030956_base_healthspan_no_cancer_df_filtered

