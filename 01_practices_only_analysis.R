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


### Call fingertips data from API
inds <- data.frame(indicators())

positive_experience <- data.frame(fingertips_data(IndicatorID = 93438, AreaTypeID = 7)) %>%
  filter(TimeperiodSortable == '20210000' & AreaType == 'GPs') %>%
  select(AreaCode, Value) %>%
  rename(positive_experience = Value)

QOF_achievement <- data.frame(fingertips_data(IndicatorID = 295, AreaTypeID = 7)) %>%
  filter(TimeperiodSortable == '20210000' & AreaType == 'GPs') %>%
  select(AreaCode, Value) %>%
  rename(QOF_achievement = Value)

deprivation <- data.frame(fingertips_data(IndicatorID = 93553, AreaTypeID = 7)) %>%
  filter(TimeperiodSortable == '20190000' & AreaType == 'GPs') %>%
  select(AreaCode, Value) %>%
  rename(deprivation = Value)

#over_65 <- data.frame(fingertips_data(IndicatorID = 93081, AreaTypeID = 7))
 # filter(TimeperiodSortable == '20210000' & AreaType == 'GPs')

#over_85 <- data.frame(fingertips_data(IndicatorID = 93226, AreaTypeID = 7))
 # filter(TimeperiodSortable == '20210000' & AreaType == 'GPs')

caring_responsibility <- data.frame(fingertips_data(IndicatorID = 352, AreaTypeID = 7)) %>%
  filter(TimeperiodSortable == '20210000' & AreaType == 'GPs') %>%
  select(AreaCode, Value, Count) %>%
  rename(carers_prop_GPsurvey = Value, numbers_carers_GPsurvey = Count)

caring_responsibility_allvars <- data.frame(fingertips_data(IndicatorID = 352, AreaTypeID = 7)) %>%
  filter(TimeperiodSortable == '20210000' & AreaType == 'GPs')

#carers_social_contact <- data.frame(fingertips_data(IndicatorID = 90638, AreaTypeID = 15))
 # filter(TimeperiodSortable == '20210000')

#carer_support_spend <- data.frame(fingertips_data(IndicatorID = 2080, AreaTypeID = 15))
 # filter(TimeperiodSortable == '20210000' & AreaType == 'GPs')

#carer_QoL <- data.frame(fingertips_data(IndicatorID = 2505, AreaTypeID = 15))
 # filter(TimeperiodSortable == '20210000' & AreaType == 'GPs')


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
  select(PRACTICE_CODE, PRACTICE_NAME, LSOA_CODE, NUMBER_OF_PATIENTS, VALUE, prop_frailty, Denominator) %>%
  rename(TOTAL_UNPAID_CARERS = VALUE) %>%
  group_by(PRACTICE_CODE) %>%
  mutate(TOTAL_PRACTICE_PATIENTS = sum(NUMBER_OF_PATIENTS)) %>%
  dplyr::ungroup() %>%
  mutate(PATIENT_PROPORTION = NUMBER_OF_PATIENTS/TOTAL_PRACTICE_PATIENTS) %>%
  mutate(CARERS_IN_LSOA = TOTAL_UNPAID_CARERS*PATIENT_PROPORTION)

practice_aggregation <- gp_join %>%
  group_by(PRACTICE_CODE, PRACTICE_NAME) %>%
  summarise(UC = median(TOTAL_UNPAID_CARERS), PATIENTS = median(TOTAL_PRACTICE_PATIENTS), prop_frailty = median(prop_frailty), over65 = median(Denominator)) %>%
  mutate(prop_carers = UC/PATIENTS) %>%
  mutate(prop_over65 = over65/PATIENTS)

practice_aggregation_join <- practice_aggregation %>%
  left_join(., positive_experience, by = c('PRACTICE_CODE' = 'AreaCode')) %>%
  left_join(., QOF_achievement, by = c('PRACTICE_CODE' = 'AreaCode')) %>%
  left_join(., deprivation, by = c('PRACTICE_CODE' = 'AreaCode')) %>%
  left_join(., caring_responsibility, by = c('PRACTICE_CODE' = 'AreaCode')) %>%
  mutate(imputed_GPsurvey_carers = (carers_prop_GPsurvey/100) * PATIENTS) %>%
  mutate(GP_survey_coverage = UC/imputed_GPsurvey_carers)

ggplot(practice_aggregation_join, aes(x = carers_prop_GPsurvey)) +
  geom_histogram()

ggplot(practice_aggregation_join, aes(x = GP_survey_coverage)) +
  geom_histogram()

ggplotly(ggplot(practice_aggregation_join, aes(x = prop_carers, y = (carers_prop_GPsurvey/100))) +
  geom_point())

