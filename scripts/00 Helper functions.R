### Install required packages if they aren't already installed
installRequiredPackages <- function(){
  
  packages <- c("tidyverse",
                "janitor",
                "rvest",
                "xml2",
                "readxl",
                "reshape2",
                "writexl",
                "openxlsx")
  
  installed_packages <- installed.packages()[,1]
  packages_to_install <- packages[!(packages %in% installed_packages)]
  if(length(packages_to_install) == 0){return("All required packages are installed.")}else{
    return(install.packages(packages_to_install))
  }
  
}

### Function to download latest  Delayed Discharge Bed Days data from PHS Website
getBedDaysData <- function(href) {
  require(janitor)
  require(dplyr)
  require(tidyr)

  # if href argument is not specified, use html scraping to get it
  if(missing(href)){
    require(rvest)
    require(xml2)
    doc<-read_html("https://publichealthscotland.scot/publications/delayed-discharges-in-nhsscotland-monthly/delayed-discharges-in-nhsscotland-monthly-figures-for-december-2021/#section-3")
    
    # DOM Element with link to data is the second 'a' tag node with class 'secondary-nav__link'
    link_node <- html_nodes(doc, 'a.secondary-nav__link')[2] 
    
    # Extract the href attribute from the node
    link_href <- html_attrs(link_node)[[1]][2]
    
    # Concatenate to get full link
    full_link <- paste0("https://beta.isdscotland.org", link_href)
    
    # else use the href link
  }else{
    full_link <- href
  }
  temp <- tempfile()
  download.file(full_link, temp)
  sheets <- openxlsx::getSheetNames(file = temp) 
  sheets <- sheets[grepl("LAData", sheets)]
  dd <- readxl::read_excel(temp, sheet = "LAData2021")[0,1:16] %>%
    clean_names() %>% mutate(FY = character(0))


  for(s in sheets){
  temp_dat <- readxl::read_excel(temp, sheet = s)[,1:16] %>% 
    clean_names() %>%
    mutate(FY = substr(s, 7,11))
  
  dd<-rbind(temp_dat, dd)
  }

  unlink(temp)

  dd <- dd %>% drop_na(la)

  return(dd)

}

### Function to get population figures (18+)
getLAPops <- function(){
  require(tidyverse)
  pops <- readRDS(paste0("/conf/linkage/output/lookups/Unicode/Populations/",
                         "Estimates/CA2019_pop_est_1981_2019.rds")) 
  lkp <- read.csv("scripts/helper/la_lookup.csv") %>%
    rename(ca2019name = NRS, la = PHS)
  suppressWarnings(
  pops %>% filter(year == max(year), age > 17) %>%
    group_by(ca2019name) %>%
    summarise(pop_18plus = sum(pop), year = last(year)) %>%
    ungroup() %>%
    left_join(lkp, "ca2019name") %>%
    select(la, pop_18plus) %>%
    mutate(la = as.character(la)))
}

### Simple function to calculate calendar year from financial year
calYear <- function(FY, month){
  if_else(month %in% c("january", "february", "march"),
          paste0(20, substr(FY, 3,4)), 
          paste0(20, substr(FY,1,2)))
}

## Function to identify and aggregate GGC Partnerships
ggc_hscps <- c("East Dunbartonshire", "East Renfrewshire", "Inverclyde",
               "Renfrewshire", "Glasgow City", "West Dunbartonshire")

## Function to get populations for last 5 years
getPreviousPops <- function(level = "scotland"){
  require(tidyverse)
  pops <- readRDS(paste0("/conf/linkage/output/lookups/Unicode/Populations/",
                         "Estimates/CA2019_pop_est_1981_2019.rds")) 
  prev_years <- seq(max(pops$year)-4, max(pops$year), 1)
  if(level %in% c("Scotland", "scotland", "scot")){
    pops <- pops %>% filter(year %in% prev_years, age>17) %>%
      group_by(year) %>%
      summarise(pop = sum(pop)) %>% ungroup() %>%
      rename(CY = year) %>% mutate(CY = as.character(CY))
    pops <- bind_rows(pops, tibble(CY = "2020", pop=last(pops$pop)))
    return(pops)
  }else
  if(level %in% c("Renfrewshire", "renfrewshire", "ren")){
    pops <- pops %>% filter(year %in% prev_years, age>17, ca2019name == "Renfrewshire") %>%
      group_by(year) %>%
      summarise(pop = sum(pop)) %>% ungroup() %>%
      rename(CY = year) %>% mutate(CY = as.character(CY))
    pops <- bind_rows(pops, tibble(CY = "2020", pop=last(pops$pop)))
    return(pops)
    
  }else
    return("Please specify geography, either 'Scotland' or 'Renfrewshire'")
}
  
## Function to save raw data to data folder and archive previous data
saveRawData <- function(to_month, to_year){
  
  file_name <- paste0("raw_data_to_",to_month,"_",to_year,".csv")
  write.csv(bed_days, file = paste0("data/",file_name), row.names = FALSE)
  
  # move old files to the archive
  data_files <- list.files("data/")
  files_to_move <- data_files[!(data_files %in% c(file_name, "archive"))]
  
  if(length(files_to_move) > 0){
    file.copy(from = paste0("data/", files_to_move),
              to = paste0("data/archive/", files_to_move))
    file.remove(paste0("data/", files_to_move))
  }
}
