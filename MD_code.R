
pacman::p_load(dplyr,
               readr,
               readxl,
               redlistr,
               here,
               data.table,
               Gmisc,
               glue,
               grid,
               INLA,
               magrittr,
               usethis,
               stringr,
               tidyverse, # general data wrangling
               tidycensus, # importing Census attribute data into R,
               spdep,
               sp,
               rgdal,
               sf, # Spatial data classes
               tigris) # importing Census geography into R

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)

## Download HMDA data
###Link for data: https://www.consumerfinance.gov/data-research/hmda/historic-data/
###Link for data dictionary: https://files.consumerfinance.gov/hmda-historic-data-dictionaries/lar_record_codes.pdf

hmda <- read.csv(here("data", file = "hmda_2010_nationwide_all-records_codes.csv"),
                 colClasses = c("applicant_race_2" = "NULL", "applicant_race_3" = "NULL", "applicant_race_4" = "NULL", "applicant_race_5" = "NULL",
                                "co_applicant_race_1" = "NULL", "co_applicant_race_2" = "NULL", "co_applicant_race_3" = "NULL", "co_applicant_race_4" = "NULL", 
                                "co_applicant_race_5" = "NULL", "co_applicant_ethnicity" = "NULL", "applicant_income_000s" = "numeric", "loan_amount_000s" = "numeric", 
                                .default = "character"))

hmda11 <- read.csv(here("data", file="hmda_2011_nationwide_all-records_codes.csv"), 
                 colClasses=c("applicant_race_2"="NULL", "applicant_race_3"="NULL", "applicant_race_4"="NULL", "applicant_race_5"="NULL",
                              "co_applicant_race_1"="NULL", "co_applicant_race_2"="NULL", "co_applicant_race_3"="NULL", "co_applicant_race_4"="NULL", 
                              "co_applicant_race_5"="NULL", "co_applicant_ethnicity"="NULL", "applicant_income_000s"="numeric", "loan_amount_000s"="numeric", 
                              .default="character"))
                 
                 
hmda12 <- read.csv(here("data", file="hmda_2012_nationwide_all-records_codes.csv"), 
                   colClasses=c("applicant_race_2"="NULL", "applicant_race_3"="NULL", "applicant_race_4"="NULL", "applicant_race_5"="NULL",
                                "co_applicant_race_1"="NULL", "co_applicant_race_2"="NULL", "co_applicant_race_3"="NULL", "co_applicant_race_4"="NULL", 
                                "co_applicant_race_5"="NULL", "co_applicant_ethnicity"="NULL", "applicant_income_000s"="numeric", "loan_amount_000s"="numeric", 
                                .default="character"))


hmda13 <- read.csv(here("data", file="hmda_2013_nationwide_all-records_codes.csv"),
                   colClasses=c("applicant_race_2"="NULL", "applicant_race_3"="NULL", "applicant_race_4"="NULL", "applicant_race_5"="NULL",
                                                                                          
                                "co_applicant_race_1"="NULL", "co_applicant_race_2"="NULL", "co_applicant_race_3"="NULL", "co_applicant_race_4"="NULL", 
                                "co_applicant_race_5"="NULL", "co_applicant_ethnicity"="NULL", "applicant_income_000s"="numeric", "loan_amount_000s"="numeric", 
                                .default="character"))

hmda14 <- read.csv(here("data", file="hmda_2014_nationwide_all-records_codes.csv"),
                    colClasses=c("applicant_race_2"="NULL", "applicant_race_3"="NULL", "applicant_race_4"="NULL", "applicant_race_5"="NULL",
                                 "co_applicant_race_1"="NULL", "co_applicant_race_2"="NULL", "co_applicant_race_3"="NULL", "co_applicant_race_4"="NULL", 
                                  "co_applicant_race_5"="NULL", "co_applicant_ethnicity"="NULL", "census_tract_number"="character", 
                                 .default="character"))

#shrink datasets as much as possible before combining
#apply the same dplyr operation to each dataset using for loop
#code throughout is intentionally completed in as many steps as possible to conserve memory

for (df_name in c("hmda", "hmda11", "hmda12", "hmda13", "hmda14")) {
  # get the dataset by name
  df <- get(df_name)
  
  # apply function 
  df <- df %>% filter(state_code != 15 & state_code != 2 
                      #subset: remove refinancing, home improvement to leave only home purchase
                      #include only Owner-occupied as a principal dwelling and 1-4 fam dwelling and multifam dwelling
                      & loan_purpose==1  
                      & owner_occupancy==1
                      & property_type!=2
                      & !is.na(applicant_sex)
                      & action_taken != 4 
                      & action_taken != 5) %>% 
    #delete missings: county code, census tract #
    drop_na(state_code, census_tract_number, county_code) 
  # assign the filtered dataset 
  assign(df_name, df)
  rm(df)
}

#Load crosswalk file 
#HMDA years 2010 and 2011 use 2000 census geography while 2012-2014 use 2010 census geography
crosswalk<- read.csv(here("data", "us2010trf.txt"), header=FALSE, colClasses ="character")
cw2 <-crosswalk %>% 
  dplyr::select(V4, V13, V26) %>% 
  dplyr::rename(GEOID00 = V4,
                GEOID10 = V13,
                POPPCT00 = V26) %>% 
  group_by(GEOID00) %>% 
  mutate(POPPCT00=as.numeric(POPPCT00),
         GEOID00=as.character(GEOID00)) %>% 
  arrange(-POPPCT00) %>% 
  filter(row_number()==1)

h10_11_r <-rbind(hmda, hmda11)

rm(hmda, hmda11)

h10_11 <- h10_11_r %>% 
  
  #turn all county codes into 3 value variables (i.e., add leading zero if needed)
  dplyr::mutate(county_code = sprintf("%03s", county_code),
                state_code = sprintf("%02d", state_code)) %>% 
  
  #create GEOID
  unite("GEOID", state_code, county_code, census_tract_number, sep = "", remove = FALSE) 

h10_11$GEOID <- str_replace(h10_11$GEOID , "[[:punct:]]", "")

#Join crosswalk with 2010-2011 HMDA data 
h10_11_update <- h10_11 %>% left_join(cw2, by=c("GEOID"="GEOID00")) %>% 
  dplyr::select(-GEOID) %>% 
  rename(GEOID=GEOID10)

h12_14_r <- rbind(hmda12, hmda13, 
                  hmda14)

h12_14 <- h12_14_r %>%
  
  #turn all county codes into 3 value variables (i.e., add leading zero if needed)
  mutate(county_code = sprintf("%03s", county_code),
         state_code = sprintf("%02d", state_code),
         POPPCT00=NA) %>% 
  
  #create GEOID
  unite("GEOID", state_code, county_code, census_tract_number, sep = "", remove = FALSE) 

h12_14$GEOID <- str_replace(h12_14$GEOID , "[[:punct:]]", "")

all_years <- rbind(h10_11_update, h12_14) %>% 
  mutate(denial = case_when(
    action_taken %in% c(1, 2, 6, 8) ~ 0,
    action_taken %in% c(3, 7) ~ 1),
    #applicant sex var (keep M/F same)
    applicant_sex_cat = if_else(applicant_sex == 1, 1,
                                if_else(applicant_sex == 2, 2,
                                        if_else(applicant_sex %in% 3:4, 3, 3))),
    loan_income_ratio = loan_amount_000s / applicant_income_000s) %>% 
  filter(!is.na(loan_income_ratio))

rm(hmda12, hmda13, hmda14, h10_11, h10_11_r, h10_11_update, h12_14, h12_14_r, crosswalk, cw2)

#Load geomtry data from ACS 
census_api_key('1d0ba87279d9ea5aac406b0f11f585e6515784ef')
abbreviations <- state.abb[-c(2, 11)] #state abbreviations for all contiguous US
vars10 <- c("P005003")
US <- get_decennial(geography = "tract", variables = vars10, year = 2010,
                    summary_var = "P001001", state=abbreviations, geometry = TRUE)

#Load MSA data 
#CBSA codes for all state/county FIPS
cbsa19 <- read_csv(here("data", "cbsa-est2019-alldata.csv")) %>% 
  janitor:: clean_names() %>% 
  dplyr::select(cbsa, stcou, name, lsad) %>% 
  filter(is.na(stcou) == FALSE)%>% 
  mutate(stcou = sprintf("%05d", stcou),
         stcou = as.character(stcou))

#all US MSA codes - use to locate specific city MSA codes 
msaUS <- read_csv(here("data", "cbsa-est2019-alldata.csv")) %>% 
  janitor:: clean_names() %>% 
  dplyr::select(cbsa, stcou, name, lsad) %>% 
  filter(lsad == "Metropolitan Statistical Area") %>% 
  mutate(stcou = as.character(stcou))

#creating tract # and stcou variable in geom data
us_msa <- US %>% 
  #creating tract # variable in geom data
  separate(
    col = GEOID,
    sep = c(5, 9),
    into = c("extra1", "tract", "extra2"),
    remove = FALSE) %>% 
  dplyr::select(-extra1, -extra2) %>% 
  #create stcou # variable from GEOID
  separate(
    col = GEOID,
    sep = c(0, 5),
    into = c("extra3", "stcou", "extra4"),
    remove = FALSE) %>% 
  dplyr::select(-extra3, -extra4) %>% 
  #join cbsa codes 
  left_join(cbsa19, by = 'stcou') 

#Join HMDA and geo (ACS) data
hmda_geo <- us_msa %>% left_join(all_years, by="GEOID")

rm(all_years)

#list of all US MSAs (384)
cities <- (msaUS$cbsa)

#loans applied for in the 
hmda_geo2 <- hmda_geo %>% 
  filter(cbsa %in% cities) 

rm(US, cbsa19, msaUS, hmda_geo)

#380 MSAs (total MSAs in US is 384, 2 in HI and 2 in AK)
city_codes <- as.character(unique(hmda_geo2$cbsa))

city_codes <- city_codes[city_codes != "12060"] #remove NYC MSA (need cluster for this!!)

my_func <- function(city_code) {
  # create new hmda_geo for each city_code
  
  hmda_geo_city <- hmda_geo2[hmda_geo2$cbsa == city_code, ]
  hmda_geo_city <- hmda_geo_city[map_lgl(hmda_geo_city$geometry, ~length(.x) != 0), ]
  
  # arrange hmda data by GEOID and create new 'group ID' col
  setDT(hmda_geo_city)
  setorder(hmda_geo_city, GEOID)
  hmda_geo_city[, group_id := .GRP, by = GEOID]
  
  
  ## use this for spatial weights because one row per GEOID
  msa_city <- us_msa %>% 
    filter(cbsa == city_code & !sf::st_is_empty(geometry)) %>% 
    arrange(GEOID) %>% 
    group_by(GEOID) %>% 
    dplyr::mutate(group_id = cur_group_id())
  
  # make weights matrix INLA format

  nb2INLA(paste0('neighbors_', city_code, '.adj'), poly2nb(msa_city))

  
  # Define the formula for the INLA model
  form_city <- formula(
    denial ~ loan_income_ratio + as.factor(applicant_sex_cat) + f(
      group_id,
      model = "bym",
      graph = paste0('neighbors_', city_code, '.adj'))
    )

  # Run the INLA model
  inla_city <- inla(
    form_city,
    family = 'binomial',
    data = hmda_geo_city,
    verbose = TRUE,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE)
  )
  
  # extract summary statistics and merge with msa_city data
  summary.rand_city <- as.data.frame(inla_city$summary.random)
  
  #extract the sum of the spatial random effects (structured spatial random effect and unstructured spatial random effect [models uncorrelated noise]) 
  x <- nrow(msa_city) + 1
  
  summary.rand_city <- summary.rand_city %>% 
    mutate(IRR = exp(group_id.mean)) %>% 
    filter(group_id.ID < x)
  
  alldata_city <- merge(msa_city, summary.rand_city, by.x = "group_id", by.y = "group_id.ID") 
  
  # remove unnecessary variables from memory
  rm(hmda_geo_city, msa_city, summary.rand_city)
  
  # initiate garbage collection
  gc()
  
  return(alldata_city)

}


# Loop through cities and calculate results for each city code
alldata_list <- list()
for (city_code in city_codes) {
  alldata_city <- my_func(city_code)
  alldata_list[[city_code]] <- alldata_city
}

write_rds(alldata_list, here::here("bayesian_US.rds"))

