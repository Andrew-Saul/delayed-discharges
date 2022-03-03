#the pops .rds file does not contain Scotland pop data


  
###########################################################################################
#create pop table for Council Areas
getLAPops <- function(age_lowest = 0){
  pops <- readRDS(paste0("/conf/linkage/output/lookups/Unicode/Populations/",
                         "Estimates/CA2019_pop_est_1981_2020.rds")) 
  
  lkp <- read.csv(glue("{here::here()}/../Ad Hoc/DD_BedDays_Lockdown/DD_Bed Days - R Project/scripts/helper/la_lookup.csv")) %>%
    rename(ca2019name = NRS, CouncilArea = PHS)
 
  suppressWarnings(
    pops <- pops %>% filter(age >= age_lowest) %>%
      group_by(ca2019name, year) %>%
      summarise(Pop = sum(pop), .groups = "drop") %>%
      left_join(lkp, "ca2019name") %>%
      select(CouncilArea, year, Pop) %>%
      mutate(CouncilArea = as.character(CouncilArea))
    )
  
  pops <- pops %>% 
    group_by(year) %>% 
    summarise(Pop = sum(Pop)) %>% 
    add_column(CouncilArea = "Scotland") %>% 
    bind_rows(pops) 
  
 # as mid-2021 doesn't yet exist, copy mid 2020 pop and rename it as year 2021
   pops %>% 
     filter(year == 2020) %>% #isolate year 2020
      mutate(year = 2021) %>% # rename values 2021
      bind_rows(pops)
   
   
}
# 
# ##############NRS urls ##########################################
# urlmales2020 <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/males/sape-2020-males.xlsx"
# urlfemales2020 <- "https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/females/sape-2020-females.xlsx"
# 
# ####################download data from urls for males and females################################
# download_pop_data <- function() {
#   download_data(urlmales2020,"males_pop.xlsx")
#   download_data(urlfemales2020,"females_pop.xlsx")
# }
# 
# # cleans excel file into long table
# clean_xlfile <- function(xlpathname, sex) {
#   read_excel(xlpathname, range = "A3:CQ6981") %>% 
#     clean_names(case="big_camel") %>% 
#     mutate(sex = sex) %>% 
#     slice(c(1,3:6981)) %>% 
#     mutate(CouncilArea = if_else(AreaName == "SCOTLAND", "Scotland", CouncilArea)) %>% 
#     select(-c(4:22)) %>% 
#     pivot_longer(cols = c(4:76), names_to = "Age", values_to = "Pop")
# }
# 
# change_CA_name <- function(tib, orig_name, dest_name) {
#   tib %>% 
#     mutate(CouncilArea = if_else(CouncilArea == orig_name, dest_name, CouncilArea))
# }
# 
# 
# create_population_tibble <- function(...) {
# 
#   male_pop_path <- here::here("data","males_pop.xlsx")
#   female_pop_path <- here::here("data","females_pop.xlsx")
#   
#   male_pop_2020 <- clean_xlfile(male_pop_path, "Male")
#   female_pop_2020 <- clean_xlfile(female_pop_path, "Female")
#   
#   
#   ## raw_pop_2020 combines male and female age group pops 
#   bind_rows(male_pop_2020, female_pop_2020) %>% 
#     drop_na(AreaName) #No rows dropped!
# }
# 
# # get Population 18+ in "Council_areas"
# get_CA_pop <- function(dat){
#   dat %>% 
#     group_by(CouncilArea) %>% 
#     summarise(Pop = sum(Pop))
# }
# 
# 
