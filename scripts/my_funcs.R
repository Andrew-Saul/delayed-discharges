if(!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

librarian::shelf(tidyverse, here, fs, readxl, janitor, glue, rlang)
###############################################################################
# fs::file_create(here::here("data"), "temp_jan_2021.csv")
# capture_basename(here("data", "temp_jan_2021.csv"))
# capture_recent_month_year(here("data", "temp_jan_2021.csv"))
# 


## Function to capture url of data to be downloaded
scraplinks <- function(text_string = "beddays"){
  NRS_url  <- "https://publichealthscotland.scot/publications/delayed-discharges-in-nhsscotland-monthly/delayed-discharges-in-nhsscotland-monthly-figures-for-december-2021/#section-3"
  text_string <- str_replace(text_string, pattern=" ", replacement="") %>% 
    str_to_lower()
  # Create an html document from the url
  webpage <- xml2::read_html(NRS_url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes(".fa-ul a") %>%
    rvest::html_attr("href")
  
  #create df to store urls
  df <- data.frame(url = glue::glue("https://publichealthscotland.scot{url_}"))
  #extract the urls containing the specific text (text_string) and store in a vector
  hrefvec <- df %>% 
    filter(str_detect(url, text_string)) %>% pull()
  #check that the vector contains a unique entry
  if (length(hrefvec) == 1){
    hrefvec
  } else {
    message("There is not a unique file containing the inputed text. Try adding 
            more characters to the <text_string> object") 
  }
}

## Function to download excel spreadsheet from url link
download_data <- function(filepath_url, filename){
  
  if(missing(filepath_url)){
    filepath_url <- scraplinks()
  } # if statement
  
  temp <- here::here("data", filename)
  download.file(filepath_url, temp, mode="wb") #mode="wb" for binary excel files
}

###########################################################################
# creates vector of sheet names dependent on pattern ie LAData or HBData
get_sheetnames <- function(xl_file, pattern_string) {
  sheet_names <- openxlsx::getSheetNames(file = xl_file) 
    sheet_names[str_detect(sheet_names, pattern = pattern_string)]  
}

# create vector of sheet names from which data is required 
create_sheetname_vector <- 
  function(xl_file = xl_file) {
    partial_sheetnames <- c("LAData", "HBData")
    map(partial_sheetnames, ~get_sheetnames(xl_file, .x)) %>% 
      unlist()
  }
# function that creates tibble for each sheet name, removes Qtr fields ,
# adds Finanical Year field, creates type of authority field (LA or HB)
# and renames LA/health board fields
create_FY_authority <- function(xl_file, sheet_name) {
  df<- read_excel(xl_file, sheet = sheet_name, range = "B1:U497")
  FY_var <- str_extract(sheet_name, pattern = "\\d+")
  df %>% 
    select(-starts_with("Q")) %>% 
    rename(FYToDate = starts_with("FY_")) %>% 
    drop_na(AgeGroup) %>% #discard these empty rows
    mutate(FY = as.integer(FY_var)) %>% 
    mutate(Authority = if_else(names(df)[1]=="LA", "LA", "HB")) %>% 
    rename(Region = 1) 
}




#############################################################################
########### Population data###################################################
# #########################################################################
