if(!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

librarian::shelf(tidyverse, here, fs, readxl, janitor, glue, rlang)
###############################################################################
## Scripts to acquire uptodate date from NRS #################################
##############################################################################
# fs::file_create(here::here("data"), "temp_jan_2021.csv")
# capture_basename(here("data", "temp_jan_2021.csv"))
# capture_recent_month_year(here("data", "temp_jan_2021.csv"))
# 

## Function to capture url of data to be downloaded
# url we are looking for contains string "beddays"
scraplinks <- function(text_string = "beddays"){
  NRS_url  <- "https://publichealthscotland.scot/publications/delayed-discharges-in-nhsscotland-monthly/delayed-discharges-in-nhsscotland-monthly-figures-for-december-2021/#section-3"
  #text_string <- "beddays"
  # Create an html document from the url
  webpage <- xml2::read_html(NRS_url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes(".fa-ul a") %>%
    rvest::html_attr("href")
  
  #create df to store urls
  df <- data.frame(url = glue::glue("https://publichealthscotland.scot{url_}"))
  #extract the urls containing the specific text (text_string) and store in a vector
  # url field is factor. need to convert to char field type
  hrefvec <- df %>% 
    mutate(across(where(is.factor), as.character)) %>% 
    filter(str_detect(url, text_string)) %>% unlist()
  #check that the vector contains a unique entry
  if (length(hrefvec) == 1){
    hrefvec
  } else {
    message("There is not a unique file containing the inputed text. Try adding 
            more characters to the <text_string> object located within ") 
  }
}

## Function to download excel spreadsheet from url link
download_data <- function(filepath_url, filename){
  
  if(missing(filepath_url)){
    filepath_url <- scraplinks(filename)
  } # if statement
  
  temp <- here::here("data", glue("{filename}.xlsx"))
  download.file(filepath_url, temp, mode="wb") #mode="wb" for binary excel files
}

##########################################################################
###########functions for use in map_dfr for wide_data acquisition #####
###########################################################################

# creates vector of sheet names dependent on pattern ie LAData or HBData
get_sheetnames <- function(xl_file, sheet_names, pattern_string) {
  sheet_names[str_detect(sheet_names, pattern = pattern_string)]  
}

# create vector of sheet names from which data is required 
# Only required if investigating "HBData" sheets as well

# requires "get_sheetnames" function
create_sheetname_vector <-   function(xl_file = xl_file) {
    sheet_names <- openxlsx::getSheetNames(file = xl_file)
    pattern_match <- c("LAData", "HBData")
    map(pattern_match, ~get_sheetnames(xl_file, sheet_names, .x)) %>%
      unlist()
}

                ##########################################

#takes first two digits of FY and converts it into calender year 20..
convert_FY_to_calender <- function(tib){
  tib %>% 
    mutate (FY_begin = str_sub(FY, 1, 2)) %>%   # extracts last two digits from FY into <suffix year>
    mutate(FY_begin = as.numeric(glue("20{FY_begin}")) ) # adds "20" in front of <suffix year>. Converts to numeric
}

# convert_calender_to_FY <- function(calyear){
#   first_year <- str_sub(calyear, -2, -1) %>%  as.numeric() # extract last 2 digits from cal year into <first year>
#   glue("{first_year}{first_year+1}") 
# }

# function that creates tibble for each sheet name, removes Qtr fields ,
# adds Finanical Year field, creates type of authority field (LA or HB)
# renames LA/health board fields and filters "All" for AgeGroup

#requires "convert_FY_to_calender" function
create_FY_authority <- function(xl_file, sheet_name) {
  df<- read_excel(xl_file, sheet = sheet_name, range = "B1:U497")
  FY_var <- str_extract(sheet_name, pattern = "\\d+")
  tempdf <- df %>% 
    select(-starts_with("Q")) %>% 
    rename(FYToDate = starts_with("FY_")) %>% 
  #  mutate(Authority = if_else(str_detect(sheet_name, "HBData"), "HB", "LA")) %>% 
    drop_na(AgeGroup) %>% #discard these empty rows
    mutate(FY = as.numeric(FY_var)) %>% 
    rename(CouncilArea = 1) %>% 
    convert_FY_to_calender() %>% 
    filter(AgeGroup == "All") 
 # if Health Board, only keeps Scotland element 
  if(str_detect(sheet_name, "HBD")){
   tempdf <-  tempdf %>% 
      filter(CouncilArea == "Scotland")
  } # if
tempdf
}


#######################################################################################

get_FY_CA_data <- function(tib) {
  tib %>%
    select(-c("FYToDate")) %>% 
    pivot_longer(cols = all_of(month_order), names_to = "Months", values_to = "Counts") %>% 
    mutate(Months = factor(Months, levels = c(month_order, ordered = TRUE))) %>% 
    group_by(CouncilArea, ComplexNeedsFlag, FY, FY_begin, Months) %>% 
    summarise(Counts = sum(Counts), .groups = "drop") 
}




#get bed rates (per 100,000) for each region, year or Month/year.
# Rates dependent on pop of that specific year!
# enter tibble, and element of ComplexNeedsFlag
get_CA_bed_rates <- function(tib1, CNeeds_string, mon=NULL, fydate) {
  tib1 <- tib1 %>% 
    filter(ComplexNeedsFlag == CNeeds_string) %>% 
    filter(FY %in% fydate)# %>% 
    #filter(CouncilArea != "Other") 
    
    if (!is.null(mon)){
      tib1 %>% filter(Months %in% mon) %>% 
        # mutate(Months = factor(Months, levels = c(month_order, ordered = TRUE))) %>% 
        left_join(LA_pop_18plus, by = c("CouncilArea", "FY_begin" = "year")) %>%
        group_by(CouncilArea, FY, Months) %>% 
        drop_na() %>% 
        summarise(Counts = sum(Counts), Pop = mean(Pop), .groups = "drop") %>% 
        mutate(Rate = Counts/Pop *100000)
    } 
  else {
    left_join(tib1, LA_pop_18plus, by = c("CouncilArea", "FY_begin" = "year")) %>% 
        group_by(CouncilArea, FY) %>% 
        drop_na() %>% 
        summarise(Counts = sum(Counts), Pop = mean(Pop), .groups = "drop") %>% 
        
        mutate(Rate = Counts/Pop *100000)
  }
}

# gets bedrates for CCG Area or  Scotland

#requires "get_CA_bed_rates" function
get_group_bed_rates <- function(..., ggc_flag = FALSE, scot_flag = FALSE){

  ggc_hcsps <- c("East Dunbartonshire", "East Renfrewshire", "Glasgow City", "Inverclyde",
                 "Renfrewshire", "West Dunbartonshire")

  if (scot_flag == TRUE) {
    get_CA_bed_rates(...) %>%
      filter(CouncilArea == "Scotland")
  } else if (ggc_flag == TRUE ){
    get_CA_bed_rates(...) %>%
      filter(CouncilArea %in% ggc_hcsps) %>%
      select(-c(FY)) %>%
      summarise(Counts = sum(Counts), Pop = sum(Pop)) %>%
      mutate(Rate = Counts/Pop *100000)
  } else get_CA_bed_rates(...)

}

#################################################################################

get_council_area_names <- function(tib){
  tib %>% 
    select(CouncilArea) %>% 
    unique() %>% 
    pull()
}

#get_prev5 selects last 6 years but excludes year 2021 . assumes earliest current year is 21
get_prev5 <- function(tib, CA, CNeeds_string, fydate){
  tib %>% 
    mutate(prev5_flag = ifelse(fydate - FY <607 & fydate - FY > 0 & fydate!= 2021, TRUE, FALSE)) %>% 
    filter(prev5_flag==TRUE) %>% 
    select(!prev5_flag) %>% 
    filter(ComplexNeedsFlag == CNeeds_string) %>% 
    filter(CouncilArea == CA) %>% 
    group_by(Months) %>% 
    summarise(Min_value = min(Counts), Max_value = max(Counts), Avg = mean(Counts), .groups = "drop")
}


get_current_year_tib <- function(tib, CA, CNeeds_string, fydate){
  tib %>% 
    filter(ComplexNeedsFlag == CNeeds_string) %>% 
    filter(FY %in% fydate) %>% 
    filter(CouncilArea == CA) %>% 
    drop_na(Counts)
}


create_5yr_plot <- function(dat1, dat2) {
  ggplot(data=dat1, aes(x=Months,group=1))+
    geom_ribbon(aes(ymin = Min_value, ymax=Max_value), fill = "grey70")+
    geom_line(aes(y=Avg), linetype = "dashed")+
    geom_line(data = dat2, aes(y=Counts), group = 1)
} 
#############################################################################
########### Population data###################################################
# #########################################################################
