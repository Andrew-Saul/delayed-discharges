source(here::here("scripts", "my_funcs.R"))

# cleans excel file into long table
clean_xlfile <- function(xlpathname, sex) {
  read_excel(xlpathname, range = "A3:CQ6981") %>% 
    clean_names(case="big_camel") %>% 
    mutate(sex = sex) %>% 
    slice(c(1,3:6981)) %>% 
    mutate(CouncilArea = if_else(AreaName == "SCOTLAND", "SCOTLAND", CouncilArea)) %>% 
    select(-c(4:22)) %>% 
    pivot_longer(cols = c(4:76), names_to = "Age", values_to = "Pop")
}
##############NRS urls ##########################################
urlmales2020 <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/males/sape-2020-males.xlsx"
urlfemales2020 <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/females/sape-2020-females.xlsx"

####################download data from urls for males and females################################


download_data(urlmales2020,"males_pop.xlsx")
download_data(urlfemales2020,"females_pop.xlsx")

male_pop_path <- here::here("data","males_pop.xlsx")
female_pop_path <- here::here("data","females_pop.xlsx")



male_pop_2020 <- clean_xlfile(male_pop_path, "Male")
female_pop_2020 <- clean_xlfile(female_pop_path, "Female")


## raw_pop_2020 combines male and female age group pops 
raw_pop_2020 <- bind_rows(male_pop_2020, female_pop_2020) %>% 
  drop_na(AreaName) #No rows dropped!

#delete unwanted tibbles
rm(male_pop_2020, female_pop_2020)

# vector "Council_areas" containing LA areas
Council_areas <- raw_pop_2020 %>% 
  select(CouncilArea) %>% 
  unique() %>% 
  pull()

# Population 18+ in "Council_areas"
LA_pop_18plus <- raw_pop_2020 %>% 
  group_by(CouncilArea) %>% 
  summarise(Pop = sum(Pop))


