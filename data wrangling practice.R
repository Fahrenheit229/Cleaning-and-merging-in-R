##For this assignment, I have 3 type of dataset that I will try to clean by making them tidy using mutate, string, and many other function

#package installation
install.packages("tidyverse")
library("tidyverse")
library("dplyr")

#data import 
bla <- read.csv("case1_bla_results.csv")
patients <- read.csv("case1_patients.csv")
sample <- read.csv("case1_sample_map.csv")

#check for data inconsistencies 
glimpse(bla) #inconsist in Gene_Name, Detection.
glimpse(patients) #inconsist in naming. everything must be either all lower or upper
glimpse(sample) #Date_Collected is in chr, Patients_ID has repeated P3

#now data cleaning one by one 
bla <- rename(bla, "Gene_name" = "Gene_Name", "Sample_id" = "Sample_ID", "CT_value" = "CT_Value")
bla_clean <- bla %>% 
  mutate(Detection = str_to_lower(Detection)) %>% 
  mutate(Detection = str_replace(Detection, "detected", "positive")) %>% 
  mutate(Detection = as.factor(Detection)) %>% 
  mutate(Gene_name = str_trim(Gene_name)) %>%
  mutate(Gene_name = str_to_lower(Gene_name)) %>% 
  mutate(Gene_name = str_replace(Gene_name, "^bla", "")) %>%
  mutate(Gene_name = str_extract(Gene_name, "kpc-\\d+|ndm-\\d+|oxa-\\d+")) %>% 
  mutate(Gene_family = str_extract(Gene_name, "kpc|ndm|oxa"))

patients_clean <- patients %>% 
  mutate(Infection = str_to_lower(Infection)) %>% 
  mutate(Ward = str_to_lower(Ward)) %>% 
  mutate(Age = as.integer(Age))


sample_clean <- sample %>% 
  mutate(Date_Collected = as.Date(Date_Collected))
sample_clean[6, "Patient_ID"] <- "P6"
sample_clean <- rename(sample_clean, "Sample_id" = "Sample_ID") #for consistency in the pattern of of my next join function 


#data are tidy and clean. Now, it is time to join them. My main data is bla_clean. I will then add the sample table to id before addind the patients data. because of the patterns shared by each tables
bla_sample_clean <- bla_clean %>% 
  left_join(y = sample_clean, by = "Sample_id")
#now that I have it, I can join the last table. I can either use full join or left join from my previous table.
complete_data <- bla_sample_clean %>% 
  left_join(y = patients_clean, by = "Patient_ID")

# or I can also do
complete_data1 <- bla_sample_clean %>% 
  full_join(y = patients_clean, by ="Patient_ID") #indeed, i got the same result from both code

#now it is time to rearrange my new dataset. the everything function is funny. hahaha
complete_data <- complete_data %>% 
  select(Patient_ID, Sample_id, Age, everything())

#my data is now tidy. I can now try to reshape it with pivot functions
gene_matrix <- select(.data = complete_data, Patient_ID, Gene_name, Detection)
gene_matrix %>%
  mutate(Detection = as.character(Detection)) %>% 
  pivot_wider(names_from = "Gene_name", 
            values_from = "Detection")

#create a matrix values of Ct
Ct_matrix <- complete_data %>% 
  select(Patient_ID, CT_value, Gene_name) %>% 
  pivot_wider(names_from = "Gene_name", 
              values_from = "CT_value",
              values_fill = 0)
view(Ct_matrix)

# now let's create a summary tables with 
  # - Patient_ID
  # - Number of positive genes
  # - List of positive genes

patient_summary <- complete_data %>%
  select(Patient_ID, Gene_name, Detection, CT_value) %>% 
  filter(Detection == "positive") %>% 
  group_by(Patient_ID) %>% 
  summarise(n_positive_genes = n(), Gene_name)

complete_data %>% 
  flextable() %>% 
  save_as_docx(path = "complete_data_clean.docx")
write.csv(complete_data, "complete_data_clean.csv", row.names = FALSE)

