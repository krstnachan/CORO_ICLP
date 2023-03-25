library(tidyverse)
library(tidycensus)
library(here)
library(xlsx)

#census_api_key("418b0f104148d70951d3790173bffeef6a0ae12e", install=TRUE)

#readRenviron("~/.Renviron")

census_tracts_boroughs_zip_codes_cw <- read.xlsx('Maps/data_exports/census_tracts_boroughs_zip_codes_cw.xlsx', sheetIndex=1)

#load the 2021 American Community Survey (ACS) 1-Year Estimates Detailed Tables
acs_2021 <- load_variables("2021", "acs1", cache=TRUE)

#load the 2021 ACS 5-Year Estimates Detailed Tables
acs_5_year <- load_variables("2017", "acs5", cache=TRUE)

#return variable names and IDs in table B16005
vars <- acs_5_year %>%
  filter(grepl('B16005_', acs_5_year$name)) %>%
  select(name, label)

vars_id <- vars$name

#variables about foreign born
vars_foreign_born_id_df <- vars %>%
  filter(grepl('Foreign born', vars$label)) %>%
  select(name, label) 

#variables about foreign born and speak English "not well" or "not at all"
vars_foreign_born_less_than_well_id_df <- vars_foreign_born_id_df %>%
  filter(grepl('"not ', vars_foreign_born_id_df$label)) %>%
  select(name, label)

vars_foreign_born_id <- vars_foreign_born_id_df$name

vars_foreign_born_less_than_well_id <- vars_foreign_born_less_than_well_id_df$name


#counties in New York City
counties <- c('Bronx County', 'Queens County', 'Richmond County', 'New York County', 'Kings County')


#create df to hold table B16005 by census tract
B16005_acs5_tract <- get_acs(geography = 'tract', 
                             state = 'New York',
                             county = counties,
                             variables = vars_id,
                             year = 2021,
                             survey = 'acs5', 
                             geometry = FALSE)


#bring in the name of the columns in table B16005
B16005_acs5_tract <- merge(x=B16005_acs5_tract, y=vars_foreign_born_id_df, all.x=TRUE, by.x='variable', by.y='label') %>%
  mutate(ID = variable) %>%
  mutate(census_tract = NAME, 
         boro = case_when(grepl('Richmond', NAME, fixed=TRUE) ~ 'R', 
                          grepl('Bronx', NAME, fixed=TRUE) ~ 'X', 
                          grepl('Queens', NAME, fixed=TRUE) ~ 'Q', 
                          grepl('New York County', NAME, fixed=TRUE) ~ 'M', 
                          grepl('Kings', NAME, fixed=TRUE) ~ 'K'
  )) 


#add column for census tract and borough in B16005_acs5_tract
#replace "Census Tract " with an empty string
B16005_acs5_tract$census_tract <- gsub('Census Tract ', '', B16005_acs5_tract$census_tract)

#get census tract
B16005_acs5_tract$census_tract <- sub(",.*", "", B16005_acs5_tract$census_tract)

#remove trailing whitespace
B16005_acs5_tract$census_tract <- str_trim(B16005_acs5_tract$census_tract, side = c('right'))

#concatenate census tract and borough initial
B16005_acs5_tract <- B16005_acs5_tract %>%
  mutate(boro_ct = paste(boro, '_', census_tract, sep=''))



#count of people by census tract (B16005_001 = Estimate!!Total)
total_people <- B16005_acs5_tract %>%
  filter(ID == 'B16005_001') %>%
  select(ID, boro, census_tract, boro_ct, estimate)


#count of foreign born by census tract
B16005_acs5_tract_foreign_born <-  B16005_acs5_tract %>%
  filter(ID == 'B16005_024') %>%
  select(ID, boro, census_tract, boro_ct, estimate)


#count of people who are foreign born and speak English less than very well by census tract
#create dataframe to hold only the columns that have the number of people who speak English less than very well
B16005_acs5_tract_foreign_born_less_than_well <-  B16005_acs5_tract %>%
  filter(ID %in% vars_foreign_born_less_than_well_id) %>%
  select(ID, boro, census_tract, boro_ct, estimate) %>%
  group_by(boro_ct) %>%
  summarize(english_less_than_well=sum(estimate))



#combine tables to get a list of census tracts and a count of people, people who are foreign born, and people who are foreign born and speak English less than "very well"
foreign_born_less_than_well <- merge(x=total_people, y=B16005_acs5_tract_foreign_born[ , c('boro_ct', 'estimate')], by='boro_ct', all.x=TRUE) %>%
  select(boro_ct, estimate.x, estimate.y) %>%
  rename(total_people = estimate.x, 
         foreign_born = estimate.y)

foreign_born_less_than_well <- merge(x=foreign_born_less_than_well, y=B16005_acs5_tract_foreign_born_less_than_well[ , c('boro_ct', 'english_less_than_well')], by='boro_ct', all.x=TRUE)


#export as Excel file
write.xlsx(as.data.frame(foreign_born_less_than_well), 'foreign_born_less_than_well.xlsx', row.names=FALSE)
write.xlsx(as.data.frame(english_less_than_well_by_tract_foreign_born), 'english_less_than_well_by_tract_foreign_born.xlsx', row.names = FALSE)





#language categories of people who speak English less than "well"
languages_of_limited_english_proficiency <- B16005_acs5_tract %>%
  filter(ID %in% vars_foreign_born_less_than_well_id)
  
languages_of_limited_english_proficiency$language <- gsub('Estimate!!Total!!Foreign born!!Speak ', '', languages_of_limited_english_proficiency$variable)

languages_of_limited_english_proficiency$language <-sub("!.*", "", languages_of_limited_english_proficiency$language)

languages_of_limited_english_proficiency <- languages_of_limited_english_proficiency %>%
  select(ID, boro, census_tract, boro_ct, language, estimate) %>%
  group_by(boro, census_tract, boro_ct, language) %>%
  summarize(estimate=sum(estimate))

languages_of_limited_english_proficiency$language <- str_to_title(languages_of_limited_english_proficiency$language)

write.xlsx(as.data.frame(languages_of_limited_english_proficiency), 'languages_of_limited_english_proficiency.xlsx', row.names=FALSE)








