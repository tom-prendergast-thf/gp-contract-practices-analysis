###########################
###### LOAD IN DATA #######
###########################

# Load in GP contract data

if (file.exists('Raw_data/core_gp_contract_2023_24_Q1.rds')){
  
  gp_contract_data <- readRDS('Raw_data/core_gp_contract_2023_24_Q1.rds')
  
} else {
  
  temp <- tempfile()
  
  download.file('https://files.digital.nhs.uk/C2/54C677/Core%20GP%20Contract_2324%20csv%20files.zip', 
                temp)
  
  gp_contract_data <- read.csv(unz(temp, 'Core GP Contract Q1 2023-24.csv'))
  
  saveRDS(gp_contract_data, 'Raw_data/core_gp_contract_2023_24_Q1.rds')
  
  unlink(temp)
  
}


# Load in GP patients by practice and lsoa data

if (file.exists('Raw_data/gp_practice_lsoa_july-23.rds')){
  
  gp_patients_data <- readRDS('Raw_data/gp_practice_lsoa_july-23.rds')
  
} else {
  
  temp <- tempfile()
  
  download.file('https://files.digital.nhs.uk/E3/7F080B/gp-reg-pat-prac-lsoa-male-female-July-23.zip', temp)
  
  gp_patients_data <- read.csv(unz(temp, 'gp-reg-pat-prac-lsoa-all.csv'))
  
  saveRDS(gp_patients_data, "Raw_data/gp_practice_lsoa_july-23.rds")
  
  unlink(temp)
}



# Load in LSOA -> LA mapping data

LA_mapping <- s3read_using(read.csv,
                           object = '/Tom/GP-contract-unpaid-carers/Data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales_(Version_2) (1).csv',
                           bucket = bucket) 


#####################################
####### CLEAN & JOIN GP DATA ########
#####################################


# Prepare contract data for join
gp_contract_data <- gp_contract_data %>%
  filter(ACH_DATE == "30/06/2023")

gp_contract_data$VALUE <- as.numeric(gp_contract_data$VALUE)

gp_contract_UC <- gp_contract_data %>%
  filter(IND_CODE == "CGPCMI01")

gp_contract_frailty <- gp_contract_data %>%
  filter(IND_CODE == "CCDCMI32") %>%
  select(PRACTICE_CODE, PRACTICE_NAME, IND_CODE, MEASURE, VALUE) %>%
  pivot_wider(names_from = MEASURE, values_from = VALUE) %>%
  mutate(prop_frailty = Numerator/Denominator)


ggplot(gp_contract_UC, aes(x = VALUE)) +
  geom_histogram()

ggplot(gp_contract_frailty, aes(x = prop_frailty)) +
  geom_histogram()

gp_contract_join <- left_join(gp_contract_UC, gp_contract_frailty, by = "PRACTICE_CODE")

# The GP patients by LSOA data 

gp_patients_data <- gp_patients_data %>%
  filter(str_detect(LSOA_CODE, "^E")|str_detect(LSOA_CODE, "^N"))

patients_noLSOA <- gp_patients_data %>%
  filter(LSOA_CODE == "NO2011")

fixed_gp_patients <- gp_patients_data %>%
  group_by(PRACTICE_CODE) %>%
  mutate(MAX_LSOA = LSOA_CODE[which.max(NUMBER_OF_PATIENTS)]) %>%
  dplyr::ungroup() %>%
  mutate(LSOA_CODE_FIXED = case_when(LSOA_CODE == "NO2011" ~ MAX_LSOA,
                                     TRUE ~ LSOA_CODE))

fixed_gp_patients$LSOA_CODE <- fixed_gp_patients$LSOA_CODE_FIXED

# Join gp datasets
gp_join <- full_join(fixed_gp_patients, gp_contract_join, by = "PRACTICE_CODE") %>%
  select(PRACTICE_CODE, PRACTICE_NAME, LSOA_CODE, NUMBER_OF_PATIENTS, VALUE, prop_frailty) %>%
  rename(TOTAL_UNPAID_CARERS = VALUE) %>%
  group_by(PRACTICE_CODE) %>%
  mutate(TOTAL_PRACTICE_PATIENTS = sum(NUMBER_OF_PATIENTS)) %>%
  dplyr::ungroup() %>%
  mutate(PATIENT_PROPORTION = NUMBER_OF_PATIENTS/TOTAL_PRACTICE_PATIENTS) %>%
  mutate(CARERS_IN_LSOA = TOTAL_UNPAID_CARERS*PATIENT_PROPORTION)

practice_aggregation <- gp_join %>%
  group_by(PRACTICE_CODE, PRACTICE_NAME) %>%
  summarise(UC = median(TOTAL_UNPAID_CARERS), PATIENTS = median(TOTAL_PRACTICE_PATIENTS), prop_frailty = median(prop_frailty)) %>%
  mutate(prop_carers = UC/PATIENTS)

ggplot(practice_aggregation, aes(x = prop_frailty, y = prop_carers)) +
  geom_point()
  