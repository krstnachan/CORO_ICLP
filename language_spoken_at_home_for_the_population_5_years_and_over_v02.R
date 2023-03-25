library(tidyverse)
library(tidycensus)
library(here)
library(xlsx)

#load the 2021 American Community Survey (ACS) 1-Year Estimates Detailed Tables
acs_2021 <- load_variables("2021", "acs1", cache=TRUE)

#load the 2021 ACS 5-Year Estimates Detailed Tables
acs_5_year <- load_variables("2017", "acs5", cache=TRUE)

#return variable names and IDs in table B16005
vars <- acs_5_year %>%
  filter(grepl('C16001_', acs_5_year$name)) %>%
  select(name, label)

vars_id <- vars$name


#variables about populations that speak English less than "very well"
vars_less_than_very_well_id_df <- vars %>%
  filter(grepl('less than', vars$label)) %>%
  select(name, label) 

vars_less_than_very_well_id <- vars_less_than_very_well_id_df$name

#create variable to hold the name of counties in New York City
counties <- c('Bronx County', 'Queens County', 'Richmond County', 'New York County', 'Kings County')
          
#create dataframe to hold table C16001 by census tract
C16001_acs5_tract <- get_acs(geography = 'tract', 
                        state = 'New York',
                        county = counties,
                        variables = vars_id,
                        year = 2021,
                        survey = 'acs5', 
                        geometry = FALSE)


#bring in the name of the columns in table C16001
C16001_acs5_tract <- merge(x=C16001_acs5_tract, y=vars, all.x=TRUE, by.x='variable', by.y='name') %>%
  rename(ID = variable) %>%
  mutate(census_tract = NAME, 
         boro = case_when(grepl('Richmond', NAME, fixed=TRUE) ~ 'R', 
                          grepl('Bronx', NAME, fixed=TRUE) ~ 'X', 
                          grepl('Queens', NAME, fixed=TRUE) ~ 'Q', 
                          grepl('New York County', NAME, fixed=TRUE) ~ 'M', 
                          grepl('Kings', NAME, fixed=TRUE) ~ 'K'
         )) 

#add column for census tract and borough in C16001_acs5_tract
#replace "Census Tract " with an empty string
C16001_acs5_tract$census_tract <- gsub('Census Tract ', '', C16001_acs5_tract$census_tract)

#get census tract
C16001_acs5_tract$census_tract <- sub(",.*", "", C16001_acs5_tract$census_tract)

#remove trailing whitespace
C16001_acs5_tract$census_tract <- str_trim(C16001_acs5_tract$census_tract, side = c('right'))

#concatenate census tract and borough initial
C16001_acs5_tract <- C16001_acs5_tract %>%
  mutate(boro_ct = paste(boro, '_', census_tract, sep=''))



#count of people by census tract (C16001_001 = Estimate!!Total)
total_people <- C16001_acs5_tract %>%
  filter(ID == 'C16001_001') %>%
  select(ID, boro, census_tract, boro_ct, estimate)


#speak english less than very well
english_less_than_very_well <-  C16001_acs5_tract %>%
  filter(ID %in% vars_less_than_very_well_id) %>%
  select(ID, boro, census_tract, boro_ct, estimate) %>%
  group_by(boro_ct) %>%
  summarize(estimate=sum(estimate))


#combine tables to get a list of census tracts and a count of people, people who are foreign born, and people who are foreign born and speak English less than "very well"
english_less_than_very_well_summary <- merge(x=total_people, y=english_less_than_very_well[ , c('boro_ct', 'estimate')], by='boro_ct', all.x=TRUE) %>%
  rename(total_people=estimate.x, english_less_than_very_well=estimate.y)




#languages of people who speak English less than "very well"

languages_of_less_than_very_well <- C16001_acs5_tract %>%
  filter(ID %in% vars_less_than_very_well_id)

languages_of_less_than_very_well$language <- gsub('Estimate!!Total!!', '', languages_of_less_than_very_well$label)

languages_of_less_than_very_well$language <-sub("!.*", "", languages_of_less_than_very_well$language)

languages_of_less_than_very_well <- languages_of_less_than_very_well %>%
  select(ID, boro, census_tract, boro_ct, language, estimate) %>%
  group_by(boro, census_tract, boro_ct, language) %>%
  summarize(estimate=sum(estimate))

languages_of_less_than_very_well$language <- str_to_title(languages_of_less_than_very_well$language)



#export as csv file
write.csv(english_less_than_very_well_summary, 'english_less_than_very_well_summary.csv', row.names = FALSE)
write.csv(languages_of_less_than_very_well, 'languages_of_less_than_very_well.csv', row.names = FALSE)







