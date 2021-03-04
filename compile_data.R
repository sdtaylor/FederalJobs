library(tidyverse)


#------------------
# Format all the raw year files to something more readable, and
# filtering to only science fields
#-------------------

tag_agencies = function(df){
  # agency codes from from DTagy.txt
  agency_info = tribble(
    ~agency_code, ~agency_acronym, ~agency,
    'AG11',  'USFS',  'US Forest Service',
    'AG03', 'ARS',    'Agricultural Research Servce',
    'CM54', 'NOAA',   'Nat. Oceanic and Atmospheric Admin.',
    'IN05', 'BLM',    'Bureau of Land Management',
    'IN08', 'USGS',   'US Geological Survey',
    'IN10', 'NPS',    'National Park Service',
    'IN15', 'USFWS',  'US Fish & Wildlife Service',
    'IN07', 'USBR',   'Bureau of Reclamation',
    'IN06', 'BIA',    'Bureau of Indian Affairs',
    'IN27', 'BOEM',   'Bureau of Ocean Energy Mgnt.',
    'HE38', 'NIH',    'National Institute of Health',
    'HE36', 'FDA',    'Food & Drug Administration',
    'HE39', 'CDC',    'Centers for Disease Control',
    'CM57', 'NIST',   'Nat. Institute of Stds. & Tech.',
    'NF00', 'NSF',    'National Science Foundation',
    'DN00', 'DOE',    'Department of Energy',
    'EP00', 'EPA',    'Environmental Protection Agency',
    'AG22', 'NIFA',   'Nat. Institute of Food & Ag.',
    'AG34', 'APHIS',  'Animal/Plant Health Inspection Svc.',
    'VATA', 'VA',     'Department of Veterans Affairs',
    'AG16', 'NRCS',   'Nat. Resources Conservation Svc.',
    'AG37', 'FSIS',   'Food Safety & Inspection Svc.',
    'DLLS', 'BLS',    'Bureau of Labor Statistics',
    'CM53', 'BEA',    'Bureau of Economic Analysis',
    'HSBD', 'CBBP',   'Customs & Border Protection',
    'SM03', 'Smith.', 'The Smithsonian',
  )
  
  # Use a join with specific agencies above, and also search
  # for some places which have numerous codes
  
  df %>%
    left_join(agency_info, by='agency_code') %>%
    mutate(agency = case_when(
      str_detect(agency_code, 'NV') ~ 'Military',  # navy
      str_detect(agency_code, 'AF') ~ 'Military',  # air force
      str_detect(agency_code, 'AR') ~ 'Military',  # army
      str_detect(agency_code, 'DD') ~ 'Military',  # dod
      str_detect(agency_code, 'DJ') ~ 'Military',  # dept. justice
      str_detect(agency_code, 'NN') ~ 'Nat. Aeronautics & Space Adm.',
      TRUE ~ agency
    )) %>%
    mutate(agency = ifelse(is.na(agency), 'Other', agency)) %>%
    mutate(agency_acronym = case_when(
      str_detect(agency_code, 'NN') ~ 'NASA',
      agency == 'Other' ~ '',
      agency == 'Military' ~ 'Mil',
      TRUE ~ agency_acronym
    ))
  
}

tag_education = function(df){
  # There are more fine grained levels defind but the primary degrees work here
  df %>%
    mutate(education_code = as.numeric(education_code)) %>%
    mutate(education = case_when(
      education_code %in% 1:3   ~ 'No HS',
      education_code %in% 4:12  ~ 'HS/GED',
      education_code %in% 13:16 ~ 'Bachelors',
      education_code %in% 17:20 ~ 'Masters',
      education_code %in% 21:22 ~ 'PhD',
      TRUE                      ~ 'Unspecified'
    ))
}

occupation_info = read_csv('data/DTocc.txt') %>%
  rename_with(tolower) %>%
  select(occupation_code = occ, occupation_desc = occt)

appt_type_info = read_csv('./data/DTtoa.txt') %>%
  select(appt_type = TOA, perm_status = TOATYPT)

location_info = read_csv('./data/DTloc.txt') %>%
  select(location_code = LOC, location=LOCT) %>%
  separate(location, into = c('loc2','location'), sep='-') %>%
  select(-loc2)

load_raw_data = function(filepath){
  main_data = read_csv(filepath) %>%
    rename_with(tolower) %>%
    rename(agency_code = agysub, location_code = loc, age = agelvl, education_code = edlvl, 
           gs_level = gsegrd, length_of_service = loslvl, occupation_code = occ,
           occupation_cat = patco, pay_plan = ppgrd, salary_level = sallvl,
           stem_occ = stemocc, supervisor = supervis, appt_type = toa, 
           schedule = worksch, status = workstat, date=datecode, 
           employment = employment, avg_salary = salary, avg_length_of_service = los)
  
  # physical (13XX) or natural science (04XX) or social science (ie. geography, archeaology, 01XX) fields 
  # bring more descriptive fields too and return only the relevant ones
  main_data %>%
    filter((str_detect(occupation_code, '04\\d{2}')) | (str_detect(occupation_code, '13\\d{2}')) | (str_detect(occupation_code, '01\\d{2}'))) %>%
    mutate(year = as.numeric(str_sub(date,1,4))) %>%
    mutate(appt_type = as.character(appt_type),
           avg_length_of_service = as.numeric(avg_length_of_service)) %>%
    tag_agencies() %>%
    tag_education() %>%
    left_join(occupation_info, by='occupation_code') %>%
    left_join(appt_type_info, by='appt_type') %>% 
    left_join(location_info, by='location_code') %>%
    select(year, agency_code,agency_acronym, agency, occupation_desc, pay_plan, avg_salary, avg_length_of_service, education, perm_status, location, employment)
}

raw_data_files = list.files('./data', pattern='FACTDATA*', full.names = T)

all_data = purrr::map_df(raw_data_files, load_raw_data)

write_csv(all_data, 'data/processed_data.csv.gz')
