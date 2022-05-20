# Libraries ---------------------------------------------------------------
# if(!require(librarian)){
#   install.packages("librarian")
#   library(librarian)
# }
# 
# librarian::shelf(tidyverse, here, fs, readxl, janitor, glue, rlang)


# Scripts to acquire uptodate date from NRS --------------------------------------------------------

scraplinks <- function(month, year){
  NRS_url  <- glue("https://publichealthscotland.scot/publications/delayed-discharges-in-nhsscotland-monthly/delayed-discharges-in-nhsscotland-monthly-figures-for-{month}-{year}/#section-3")
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
    filter(str_detect(url, "standard|code9")) 
  #check that the vector contains a unique entry
  if (nrow(hrefvec) == 2){
    hrefvec
  } else {
    stop("There are no longer 2 excel files found containing the data.
            A New script is required to extract the data") 
  }
}

download_data <- function(filepath_url, month, year){
  
  if(missing(filepath_url)){
    filepath_url <- scraplinks(month, year)
  } # if statement
  
  standard_file <- here::here("data", glue("standard.xlsx"))
  code9_file <- here::here("data", glue("code9.xlsx"))
  #standard data file
  download.file(filepath_url[[1]][[1]], standard_file, mode="wb") #mode="wb" for binary excel files
  download.file(filepath_url[[1]][[2]], code9_file, mode="wb")
}

# excel file to tibble -------------------------------------------------

fill_missing_dates <- function(tmp_list_region, region) {
  
  # determine the first and last date entry of each region
  first_date_tibble_entry <- 
    tmp_list_region %>% 
    slice(1)  %>% 
    select(month) %>% 
    pull() %>% 
    as.Date() 
  
  last_date_tibble_entry <- 
    tmp_list_region %>% 
    slice(n()) %>% 
    select(month) %>% 
    pull() %>% 
    as.Date() 
  
  # if the first row is missing, add it with the fields which are constant, except
  # counts which is given a value 0
  if(first_date_tibble_entry !=as.Date("2016-07-01")){
    tmp_list_region <- 
      tibble(month = as.Date("2016-07-01"), delay_reasons = as.character(delay_reason), 
        geog = as.character(region), counts = as.integer(0)) %>% 
      bind_rows(., tmp_list_region)
  }
  
  # only need to add the date field if the last entry is not present,
  # as all fields will be filled down 
  if(last_date_tibble_entry !=ym(paste(params$Report_year, params$Report_month))){
    tmp_list_region <- 
      tibble(month = ym(paste(params$Report_year, params$Report_month))) %>% 
      bind_rows(tmp_list_region, .) 
  }
  
  # use pad to fill in missing dates in sequential order
  # create the fin_yr field . if month => 4, the 1st year of fy will be the same 
  # as the date, otherwise it will be the year previous 
  tmp_list_region %>% 
    padr::pad() %>%  # completes empty date entries
    tidyr::fill(delay_reasons, geog) %>% 
    mutate(fin_yr = if_else(month(month)>=4, 
                            glue::glue("{year(month)}/{str_sub(year(month)+1,3,4)}"),
                            glue::glue("{year(month)-1}/{str_sub(year(month),3,4)}"))) %>% 
    mutate(counts = if_else(is.na(counts), 0, counts))
}

create_FY_authority <- function(xl_file, sheet_name, delay_reason) {
  # excludes NHS names from inclusion in analysis
 tmp_list <-  read_excel(xl_file, sheet = sheet_name) %>% 
    select(c(month, age_groups, delay_reasons, geog, obd)) %>%
    filter(age_groups != "18+", delay_reasons == delay_reason,
           !str_detect(geog, "NHS"), geog!="Other") %>% 
    mutate(month = ymd(month)) %>%
    group_by(month, delay_reasons, geog) %>%
    summarise(counts= sum(obd), .groups = "drop") %>% 
   split(.$geog)

 map2(tmp_list, names(tmp_list), fill_missing_dates) 
}


create_FY_authority_187475plus <- function(xl_file, sheet_name, delay_reason) {
  ## Extracts relevant info from excel sheet
  ## Age groups 18-74 and 75+ are aggregated as there are missing values for 
  ## the age_group 18+ in Scotland
  
  ## Also removes earliest FY 2016/17 as incomplete data for this year
  
  read_excel(xl_file, sheet = sheet_name) %>% 
    select(c(fin_yr, month, age_groups, delay_reasons, geog, obd)) %>%
  #  rename(month_name = month) %>% 
    filter(age_groups != "18+", delay_reasons == delay_reason) %>% 
     group_by(fin_yr, month, delay_reasons, geog) %>% 
     summarise(counts= sum(obd), .groups = "drop") 
}

# remove year 
create_main_tibble <- function(obj1, obj2) {
  bind_rows(obj1, obj2) %>% 
    filter(fin_yr != "2016/17") %>% 
    rename(dates = month) %>% 
    mutate(year = year(dates), months = month(dates)) %>%
    mutate(geog = str_replace_all(geog, "NHS ", "")) %>% #removes NHS in geog field
    mutate(GGC_Region = if_else(geog %in% CA_no_Scotland, TRUE, FALSE)) %>% 
    mutate(fin_yr = factor(fin_yr, labels = create_fctorder_finyr(.), ordered = TRUE))  %>% 
  #  mutate(fin_yr = factor(fin_yr, levels = rev(levels(fin_yr)))) %>% #reverse order of levels 
    mutate(Month_labels = factor(months, levels = c(4:12, 1:3),
                                 labels = c("Apr", "May", "Jun", "Jul",
                                            "Aug", "Sep", "Oct", "Nov",
                                            "Dec", "Jan", "Feb", "Mar"),
                                  ordered = TRUE))  
  

}

#  historic HSCP data acquistion  ------------------

get_historic_counts <- function(tib, CA) {
  ## Excluding current FY 
  
  ## this function first creates a vector of unique, ordered fin_years
  ## the value of the level for the most recent and earliest inputted fin_yrs is obtained
  ## The orginal tibble is filtered by fin_yr levels between and including
  ## these two extremes.  The year "2020/21" is also excluded.
  ## Finally the tibble is filtered by geog HSCP
  
  fin_yr_vec <- 
    tib %>% 
    select(fin_yr) %>% 
    distinct() %>% 
    pull()
  
  most_recent_int <- fin_yr_vec[fin_yr_vec == params$most_recent_historical_fy] %>% as.integer()
  earliest_int <- fin_yr_vec[fin_yr_vec == params$earliest_historical_fy] %>% as.integer()
  
  
  tib %>% 
    filter(as.integer(fin_yr) >= earliest_int, 
           as.integer(fin_yr) <= most_recent_int, 
           fin_yr != "2020/21", 
           geog %in% CA)
}


get_minmax_rates <- function(tib){
  # creates tibble containing min and max rates 
  
  tib %>% 
    mutate(Rate = 100000*counts/Pop) %>% 
    group_by(months) %>% 
    summarise(Min_rate = min(Rate), Max_rate = max(Rate))
}

get_avg_rate <- function(tib){
  #calculates Avg counts for each Month 
  tib %>% 
    group_by(months) %>% 
    summarise(Avg_rate = 100000*sum(counts)/sum(Pop), .groups = "drop") 
}

get_sum_counts_pop <- function(tib){
  # creates tibble containing aggregated rates for each month
  tib %>% 
    group_by(months) %>% 
    summarise(Total_Counts = sum(counts), Total_Pop = sum(Pop)) %>% 
    ungroup()
}



#function used in get_current_rates and get_current_totals
get_pre_currents <- function(tib, CA) {
  
  # vector element containing current fin_yr
  current_fin_yr <- tib %>% 
    select(fin_yr) %>% 
    distinct() %>% 
    pull() %>% 
    max()
  
  tib %>% 
    filter(fin_yr == current_fin_yr) %>% 
    filter(geog == CA) %>% 
    drop_na(counts) %>% 
    mutate(Pop = if_else(is.na(Pop), lag(Pop, n=sum(is.na(Pop))), Pop)) 
}



get_rates <- function(tib, CA, pop_objfile){
  #get last 7yrs data (excluding 2021) in terms of rates
  #assumes tibble has unique ComplexNeedsFlag value
  
  #creates ordered factor fin_yr and joins with pop data
  tib_joined <- tib %>% 
    left_join(pop_objfile, by = c("geog" = "CouncilArea",  "year" )) %>% 
    fill(Pop, .direction = "down") # fills NA col values with prev non NA value downwards
  
  
    
  
  #create tibble of pre current values- ignoring year "2020/21"  and current year
  prev5_tib <- get_historic_counts(tib_joined, CA) 
  
  min_max_rate <- get_minmax_rates(prev5_tib)
  avg_rate <- get_avg_rate(prev5_tib)
  total_counts_pop <- get_sum_counts_pop(prev5_tib)
  
  #join min, max, avg tibbles together and rename to prev5_rate
  prev5_rate <- left_join(min_max_rate, avg_rate, by = "months") %>% 
    left_join(., total_counts_pop, by = "months") 
  
  current_rate <-  
    get_pre_currents(tib_joined, CA) %>% 
    get_avg_rate() %>% 
    rename(Current_rate = Avg_rate) 

  current_totals <- 
    get_pre_currents(tib_joined, CA) %>% 
    get_sum_counts_pop() %>% 
    rename(Current_Counts = Total_Counts, Current_Pop = Total_Pop)

  # join historic and current rates and totals into tibble
  df <- left_join(prev5_rate, current_rate, by = "months") %>%  
        left_join(.,  current_totals, by = "months")  %>% 
    mutate(Month_labels = factor(months, levels = c(4:12, 1:3),
                                 labels = c("Apr", "May", "Jun", "Jul",
                                            "Aug", "Sep", "Oct", "Nov",
                                            "Dec", "Jan", "Feb", "Mar"),
                                 ordered = TRUE)) 
 
  # ordered vector of all FYs
  all_dates_from_xlfile <- tib_joined %>%
    select(fin_yr) %>%
    distinct()

 list(df, all_dates_from_xlfile)
}


# the dataset is split into two lists according to "code9" or "All standard" value of 
# delay reasons
get_rates_years_of_interest <- function(df, delay_reason, CA_of_interest, pop_objfile=LA_pop_18plus){
  
  df_split <- df %>%
    filter(geog %in% CA_of_interest) %>% 
    filter(delay_reasons %in% delay_reason) %>%
    split(.$delay_reasons) 
  
  # for each (2) delay_reason tibble, the historic and current aggreagate rates for each
  # Council Area are calculated.  
  map(df_split, function(x) 
    map(CA_of_interest, function(y) get_rates(tib = x, CA= y, pop_objfile))%>% 
      set_names(CA_of_interest)
  ) 
} 
# ---------------------

# vector with ordered levels of fin_yr
create_fctorder_finyr <- function(tib) {
 # fin_yr_order_fct <-
    tib %>%
    mutate(fin_yr = factor(fin_yr)) %>%
    select(fin_yr) %>%
    pull() %>%
    levels()
}

#  Create plots and tables for each GGC Council Area and Scotland -----------------
# select the latest fin_yr from the column fin_yr 
# used within get_pre_currents function
select_current_fy <- function(tib) {
  tib %>%
    select(fin_yr) %>%
    distinct() %>%
    pull() %>%
    max()
}

create_regional_plot <- function(tibble_name = rates_permonth, index=NULL, delay_reason=NULL, CA_of_interest) {
   
  # extracts current FY from df
  
  fun_df <- tibble_name[[delay_reason]][[index]]
  
  # selects most recent FY from those investigated in df_main tibble

  current_fy <- select_current_fy(fun_df[[2]])

  # Plotly plot
  #sets margin values in mrg variable
  mrg <- list(l = 50, r = 50,
              b = 100, t = 100,
              pad = 20)
  
  plot_ly(fun_df[[1]], x= ~Month_labels) %>% 
    
    add_lines( y=~Current_rate, 
               name = glue("FY {current_fy}"),
               type="scatter", 
               mode="lines+markers",
               line = list(color = c('magenta')), 
               marker=list(color = 'magenta'),
               hovertemplate = paste( "Rate: %{y:.1f}<br>Month: %{x} </br>")) %>%
    
    add_lines(y= ~Avg_rate, 
              name = "Prev 5yr Avg",
              type = "scatter",
              mode="lines+markers",
              line = list(color = "blue",  dash='dash'),
              marker = list(color = "blue"),
              hovertemplate = paste( "Rate: %{y:.1f}<br>Month: %{x} </br>")) %>% # dash options include 'dash', 'dot', and 'dashdot') %>%
    
    add_lines(y = ~Min_rate, 
              type = "scatter",
              mode="lines+markers",
              line = list(color = "#6e7073"),
              marker = list(color = "#6e7073"),
              name = "Minimum rate in prev 5 yrs",
              hovertemplate = paste( "Rate: %{y:.1f}<br>Month: %{x} </br>")) %>% 
    
    add_lines(y = ~Max_rate, 
              type = "scatter",
              mode="lines+markers",
              line = list(color = "#6e7073"),
              marker = list(color = "#6e7073"),
              name = "Maximum rate in prev 5 yrs",
              hovertemplate = paste( "Rate: %{y:.1f}<br>Month: %{x} </br>")) %>% 
    
    add_ribbons(ymin = ~Min_rate, ymax=~Max_rate,
                mode="line",
                line = list(color = "#6e7073"),
                fillcolor = "rgba(7, 164, 181, 0.2)",
                name = "Range prev 5 years",
                hoverinfo = "none") %>% #hovertemplate included separately below
    
    layout(title = list(text = paste(str_wrap(glue("{CA_of_interest[[1]]} :Delayed Discharge Monthly Bed Days Rate with 
                                                   Previous 5-year Average {params$earliest_historical_fy} to {params$most_recent_historical_fy}) - {delay_reason} Delays 
                                                   ({params$Report_month} {params$Report_year}) "), width = 70), collapse = "\n"), x=0.05, pad = list(b=120)),
           xaxis = list(title = ""),
           yaxis = list(title = "Bed days rate (per 100,000 population)"),
           legend = list(x = 1, y = 0.9)
    ) 

}

create_current_FY_tables <- function(tibble_name = rates_permonth, index=NULL, delay_reason, CA_of_interest){
  
  fun_df <- tibble_name[[delay_reason]][[index]]
  
  #create row containing values for each month
  temp_df <- fun_df[[1]] %>%
    select(Month_labels, Current_Counts) %>%
    arrange(Month_labels) %>% # need to arrange order of column before using pivot_wider
    pivot_wider(names_from = "Month_labels", values_from = "Current_Counts") %>%
    mutate(across(Apr:Mar, ~(scales::label_comma(accuracy = 1)(.x))))
  

  current_fy <- select_current_fy(fun_df[[2]])
  
  temp_df %>%
    gt() %>%
    tab_header(
      title = glue("Number of Monthly Bed Days in {current_fy}")
    ) %>%
    cols_width(
      everything() ~ px(70)
    )
}


# get historical and current discharge rates for GGC Council Areas and Scotland -----

create_formatted_rate <- function(df){
  ### Extract Rate field row as single value
  ## used in create_bedrate_list
  value <- df %>%
    select(Rate) %>%
    pull()
  
  title <- df %>%
    select(Rate) %>%
    mutate(Rate = format(round(Rate,1))) %>%
    pull()
  
  list(value, title)
}


calculate_bed_rates <- function(tib, delay_reason, mon, FYTD_flag) {
  # calculate bed rates (per 100,000) for current year only
  # Rates dependent on pop of that specific year!
  # mon = params$month
  
  # calculates max value of fin_yr char vector in tib, 
  # which is current year
  current_year <- select_current_fy(tib)
  
    # filter by current year and delay_reason
  tib <- tib %>%
    filter(delay_reasons == delay_reason, fin_yr == current_year) 
    
  if (!is_true(FYTD_flag)){
    tib %>%
      filter(Month_labels %in% str_to_title(substr(mon, 1, 3))) %>% 
      # mutate(Months = factor(Months, levels = c(month_order, ordered = TRUE))) %>%
      left_join(LA_pop_18plus, by = c("geog" = "CouncilArea", "year")) %>%
      group_by(geog, fin_yr, Month_labels, GGC_Region) %>%
      drop_na() %>%  # removes unwanted geogs
      summarise(counts= sum(counts), Pop = mean(Pop), .groups = "drop") %>%
      mutate(Rate = counts/Pop *100000)  
  }
  else {
    
    left_join(tib, LA_pop_18plus, by = c("geog" = "CouncilArea", "year")) %>% 
      drop_na() %>% # removes unwanted geogs
      filter(Month_labels <= str_to_title(substr(mon, 1, 3))) %>%
      group_by(geog, fin_yr, GGC_Region) %>%
      summarise(counts= sum(counts), Pop = mean(Pop), .groups = "drop") %>%
      mutate(Rate = counts/Pop *100000) 
  }
}

create_bedrate_list <- function(...){
  # a list containing the scot rate, the ggc rate and the original tib  
  # without "Scotland" geog created.  both scot and ggc rates contain
  # a formatted entry used for display on charts
  
  arglist <- list(...)
  
  # save copy of orig tibble
  orig_tib <- arglist[[1]]
  
  # calculate avg scot rate
  arglist[[1]] <- 
    orig_tib %>% 
      filter(geog == "Scotland")
    
   scot_rate <-  do.call(calculate_bed_rates, arglist) %>% 
     create_formatted_rate()
      
    
  # calculate ggc rate
    arglist[[1]] <- orig_tib %>%
      filter(GGC_Region == TRUE)
    
  ggc_rate <-  do.call(calculate_bed_rates, arglist) %>%
            group_by(GGC_Region) %>%
            summarise(counts= sum(counts), Pop = sum(Pop), .groups = "drop") %>%
            mutate(Rate = counts/Pop *100000) %>% 
            create_formatted_rate()
    
  # calculate rates for each individual entry in the selected geog region
  # without the "Scotland" geog entry
  arglist[[1]] <- orig_tib
  geog_rate <- calculate_bed_rates(...) %>% 
    filter(geog != "Scotland")
  
  list(scot_rate, ggc_rate, geog_rate)
}




# ----



get_last_change <- function(tib, delay_reason, mon, FYTD_flag, pop_objfile = LA_pop_18plus) {
  # get bed rates (per 100,000) for each region, year or Month/year as well as member of CCGRegion.
  # Rates dependent on pop of that specific year!
  # enter tibble, and element of ComplexNeedsFlag
  
  # save current year to obj ie. save last value(ie max level) in levels vector
  current_year <- create_fctorder_finyr(tib) %>% 
    nth(., n=-1) 
  
  # create numeric field corresponding to factor level of month
  tib <- tib %>%
    filter(delay_reasons == delay_reason) %>% 
    mutate(Month_value = as.numeric(Month_labels))
  
  # current month value ie params$Report_month
 mon_int <- tib %>% 
   filter(Month_labels == str_to_title(substr(mon, 1, 3))) %>% 
   slice(1) %>% 
   select(Month_value) %>% 
   pull()
  
  #calculate previous month's bedrate if FYTD_flag = false ##
  if(!is_true(FYTD_flag)){
    #if April, then get March of previous FY
    if(mon_int == 1){
      preceeding_level = 12
      preceeding_level_FY = year
    } else if (mon_int == 10) {
      preceeding_level = mon_int - 1
      preceeding_level_FY = params$Report_year-1
    } else {
      preceeding_level = mon_int - 1
      preceeding_level_FY = params$Report_year
    }

    tib %>% filter(Month_value == preceeding_level,
                       year == preceeding_level_FY) %>%
      left_join(., pop_objfile, by = c("geog" = "CouncilArea", "year")) %>%
      mutate(Prev_counts= counts, Prev_Pop = Pop) %>% 
      select(geog, Prev_counts, Prev_Pop)  %>%
      mutate(Prev_Rate = 100000 * Prev_counts/Prev_Pop)
    
  } else #if FYTD_flag = FALSE, calculate rate for all prev months for last year's fin_yr
  {
    # prev year is considered "2020/21" unless current year != "2021/22"
    if(create_fctorder_finyr(tib) %>% nth(.,n=-1) == "2021/22"){
      prev_year <- "2020/21"
    } else 
      # prev year fin_yr = 2nd last entry of fin_yr level vector
      {
      prev_year <- create_fctorder_finyr(tib) %>% nth(.,n=-2)
    }

    
    tib <- tib %>%
      filter(fin_yr == prev_year)
    
    left_join(tib, pop_objfile, by = c("geog" = "CouncilArea", "year")) %>%
      filter(Month_value <= mon_int) %>%
      group_by(geog, fin_yr) %>%
      summarise(Prev_counts= sum(counts), Prev_Pop = mean(Pop), .groups = "drop") %>%
      mutate(Prev_Rate = 100000 * Prev_counts/Prev_Pop) %>% 
      select(-fin_yr)
  }
}


join_current_prec_tibbles <- function(tib, delay_reason, 
                                      mon, FYTD_flag, pop_objfile){
  
  left_join(calculate_bed_rates(tib, delay_reason,
                               mon, FYTD_flag), get_last_change(tib, delay_reason,
                                                                        mon, FYTD_flag, pop_objfile), by = "geog")
}

calculate_prev_pc_change <- function(tib){
  tib %>%
    mutate(pc_change_prev = ((Rate-Prev_Rate)/Prev_Rate))
}

# creates a tibble containing rows of all individual GGC entries, 
# the aggregate GGC and aggregate Scotland values.
# fields included are the location, current rate and % change from previous time point
get_pc_change_previous <- function(tib, delay_reason,
                                    mon, FYTD_flag = FALSE, pop_objfile = LA_pop_18plus) {
  #Individual HSCP regions
  tab_month1 <- join_current_prec_tibbles(tib, delay_reason,
                                         mon, FYTD_flag, pop_objfile) %>%
    filter(GGC_Region == TRUE) %>% # for GGC HSCP summary only
    calculate_prev_pc_change(.) %>%
    select(geog, Rate, pc_change_prev)
  
  # creates output for GGC HSCP only
  tab_month2 <- join_current_prec_tibbles(tib, delay_reason,
                                          mon, FYTD_flag, pop_objfile) %>%
    modify_ggc_join_tibbles(.) %>% # for GGC HSCP summary only
    calculate_prev_pc_change(.) %>%
    select(geog, Rate, pc_change_prev)
  # Scotland region
  tab_month3 <- join_current_prec_tibbles(tib, delay_reason,
                                           mon, FYTD_flag, pop_objfile) %>% 
    filter(geog == "Scotland") %>% # for GGC HSCP summary only
    calculate_prev_pc_change(.) %>%
    select(geog, Rate, pc_change_prev) 
  
  bind_rows(tab_month1, tab_month2, tab_month3)
}


# a tibble is created that ranks each HSCP - 
# lower rate =  lower rank
get_rank <- function(tib, delay_reason, mon, FYTD_flag){
  
  #Get rates for localities and rank them (includes Scotland)
  tib <- calculate_bed_rates(tib, delay_reason, mon, FYTD_flag) %>%
    mutate(Rank = min_rank(Rate))
  
  # remove "Scotland" rank
  tib$Rank[tib$geog=="Scotland"] <- NA
  
  #rerank without Scotland
  tib %>%  mutate(Rank = min_rank(Rank)) %>%
    select(geog, Rank)
}

# calculates % change of the aggregated GGC region vs previous 5yr period
aggregate_5yr_ggc <- function(tib=rates_permonth, delay_reason, CA_no_Scotland, mon) {
  # obtains previous historic avg for each GGC region
  map_df(CA_no_Scotland, ~get_historic_comparisons(tib, delay_reason, CA=.x, mon)) %>%
    bind_cols(geog = names(CA_no_Scotland)) %>%
    # aggregates counts and totals of prev and current years
    summarise(Prev_Counts = sum(Total_Counts), Prev_Pop = sum(Total_Pop),
              Current_Counts = sum(Current_Counts), Current_Pop = sum(Current_Pop)) %>%
    # and calculates % change vs prev 5yr period for aggregate GGC period
    mutate(geog = "GGC HSCPs",
           pc_change_Avg_5yr = ((Current_Counts/Current_Pop)- (Prev_Counts/Prev_Pop))/ (Prev_Counts/Prev_Pop)) %>%
    select(geog, pc_change_Avg_5yr)
}

# gets historic avg rates and current rates for a geographic region in 1 row of data
get_historic_comparisons <- function(tib, delay_reason, CA, mon) {
  
  calender_months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  
  #selects numeric value of month as per calender_months
  month_int <- 
    match(params$Report_month, calender_months) 
  
  tib[[delay_reason]][[CA]][[1]] %>% # tibble containing historic data
    filter(months == month_int) %>%
    mutate(pc_change_Avg_5yr = ((Current_Counts/Current_Pop) - (Total_Counts/Total_Pop))/(Total_Counts/Total_Pop)) %>%
    mutate(geog = names(CA)) 
}



# calculates % change of current vs prev 5 year avg for GGC regions and Scotland
# Excludes year 20/21
calculate_rate_change <- function(tib=rates_permonth, delay_reason, CA_list, CA_no_Scotland, mon) {
  map_df(CA_list, ~get_historic_comparisons(tib, delay_reason, CA=.x, mon)) %>% 
    bind_cols(geog = names(CA_list)) %>% 
    select(geog, pc_change_Avg_5yr) %>%
    bind_rows(., aggregate_5yr_ggc(tib=rates_permonth, delay_reason, CA_no_Scotland, mon))
  
}

get_summary_table <- function(tib=df_main, mon, delay_reason, FYTD_flag) {
  # get_pc_change_previous - tib containing rows of GGC HSCPs, aggregate GGC and Scot,
  #                           fields: geog, rate and % change from previous time period
  
  # get_rank - tib containing geog and rank
  
  # calculate_rate_change - 
  
  get_pc_change_previous(tib, delay_reason, mon, FYTD_flag, pop_objfile = LA_pop_18plus) %>%
    left_join(., get_rank(tib, delay_reason, mon, FYTD_flag), by = "geog") %>% 
     left_join(., calculate_rate_change(rates_permonth, delay_reason, CA_list = CA_of_interest, CA_no_Scotland, mon), by = "geog") %>%
     relocate(Rank, .after = "geog")
}



# Create data ready for display -----------------------------------------------
# access bedrate list, extract tibble from list, join with ranking tibble
create_appendix <- function(tib=df_main, delay_reason, mon, FYTD_flag) {
  create_bedrate_list(tib, delay_reason, mon, FYTD_flag)[[3]] %>% # select tibble from list
    filter(!geog %in% c("Scotland","Other")) %>%
    left_join(., get_rank(tib, delay_reason, mon, FYTD_flag), by = "geog") %>%
    arrange(Rank) %>%
    select(-c(fin_yr, GGC_Region))
}

# Create plots and tables for display -----------------------------------

create_CArate_plot <- function(df, delay_reason, mon, FYTD_flag, g_rate = ggc_rate, s_rate = scot_rate){
  ## plot of rates for all CAs

  df <- df %>%
    filter(geog != "Other")

  if (is_true(FYTD_flag)){
    
    #sub_str aquires first two digits from FY xxxx value
    mon_title <- glue("April {year_begin} to {params$Report_month} {params$Report_year}")
  } else {# CORRECT THIS WHERE MONTHS DONT EXiST!!!!!!!!!
    #mon_value <- df %>% slice(1) %>% select(Months) %>% pull() %>% as.character()
    mon_title <- glue("{params$Report_month} {params$Report_year}")
    #mon_title <- c(glue("{mon_value} "))
  }

  p <-   ggplot(df, aes(x=reorder(geog, Rate), y=Rate, fill = GGC_Region))+
    geom_col(show.legend = FALSE)+
    geom_text(aes(label = format(signif(as.integer(Rate,3)))), hjust = 1, colour = "white") +
    scale_fill_manual(name= "GGC Area", values =c("#3393dd", "#9cc951"))+
    guides(fill = guide_legend(reverse=TRUE))+
    geom_hline(yintercept=s_rate[[1]], color = "#3f3685")+
    geom_hline(yintercept=g_rate[[1]], color = "#83bb26")+
    coord_flip()+
    labs(title = str_wrap(glue("Delayed Discharge Bed Days Rate by HSCP - {delay_reason} Delays ({mon_title})"), width = 60),
         y = str_wrap("Bed days rate (per 100,000 population)"),
         x= "")+
    #guides(linetype = guide_legend(order = 2), fill = guide_legend(order = 1), color = guide_legend(order = 1))+
    theme_set(theme_minimal(base_size = 12, base_family = "Open Sans"))+
    theme_update(
      axis.ticks = element_line(color = "grey92"),
      axis.ticks.length = unit(.5, "lines"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      # legend.title = element_text(size = 12),
      #legend.text = element_text(color = "grey30"),
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "grey30"),
      plot.caption = element_text(size = 9, margin = margin(t = 15))
    )

  if (is_true(FYTD_flag)){
    #  p <-  p + annotate("text", label = str_wrap(glue("{s_rate[[2]]} Scotland"), width = 10), x = 2, y = (s_rate[[1]]-350), color = "#665e9d", size = 3.5)+
    #   annotate("text", label = str_wrap(glue("{g_rate[[2]]} GGC_HSCPs"), width = 10), x = 2, y = g_rate[[1]]+350, color = "#9cc951", size = 3.5)
    p <- p + geom_richtext(x =2.5, y = (s_rate[[1]]), aes(label = glue("**{s_rate[[2]]} <br> Scotland**")),  color = "white", fill = "#665e9d", size = 3.5) +
      geom_richtext(x = 6, aes(y = (g_rate[[1]]), label = glue("**{g_rate[[2]]} <br> GGC_HSCPs**")),  color = "black", fill = "#9cc951", size = 3.5)
  } else {
    p <- p + geom_richtext(x =6, y = (s_rate[[1]]), aes( label = glue("**{s_rate[[2]]} <br> Scotland**")), color = "white", fill = "#665e9d", size = 3.5) +
      geom_richtext(x = 2.5, aes(y = (g_rate[[1]]), label = glue("**{g_rate[[2]]} <br> GGC_HSCPs**")),  color = "black", fill = "#9cc951", size = 3.5)
  }
  p
}

# ---------------------------------------------------
get_written_summary_data <- function(tib) {
  ggc_higher_scot_rates <- tib[[3]]%>%
    filter(GGC_Region == TRUE) %>%
    filter(Rate > tib[[1]])
  
  count = nrow(ggc_higher_scot_rates)
  
  regions <- ggc_higher_scot_rates %>%
    select(geog) %>%
    pull()
  
  list(count, regions)
}

create_appendix1 <- function(tib, title_name){
  tib %>%
    gt(rowname_col = "geog") %>%
    tab_stubhead(label = "HSCP") %>%
    tab_header(title = md(glue(title_name))) %>%
    cols_label(Rate = md("Rate <br> (per 100,000)"),
               Pop = md("Population (18+)"),
               counts = md("Number of <br> Bed Days")
    ) %>%
    fmt_integer(columns = c(counts, Pop, Rank)) %>%
    fmt_number(columns = Rate, decimals = 1) %>%
    cols_width(
      Rank ~ px(50),
      Rate ~ px(120),
      counts ~ px(100),
      Pop ~ px(120),
      everything() ~ px(190)
    ) %>%
    tab_style(
      locations = cells_body(
        columns = everything(),
        rows = c(CA_no_Scotland)
      ),
      style = cell_fill(color = "#9cc951")
    ) %>%
    tab_source_note(html('<pre><span style="background-color: #9CC951;">
                         </span> = HSCP within the GGC Region</pre>'))
}

create_appendix2 <- function(tib, title_name){
  tib %>%
    select(-Month_labels) %>%
    gt(rowname_col = "geog") %>%
    tab_stubhead(label = md("HSCP")) %>%
    tab_header(title = md(glue(title_name))) %>%
    cols_label(Rate = md("Rate <br> (per 100,000)"),
               Pop = md("Population (18+)"),
               counts = md("Number of <br> Bed Days")
    ) %>%
    fmt_integer(columns = c(counts, Pop, Rank)) %>%
    fmt_number(columns = Rate, decimals = 1) %>%
    cols_width(
      Rank ~ px(50),
      Rate ~ px(120),
      counts ~ px(100),
      Pop ~ px(120),
      everything() ~ px(170)
    ) %>%
    tab_style(
      locations = cells_body(
        columns = everything(),
        rows = c(CA_no_Scotland)
      ),
      style = cell_fill(color = "#9cc951")
    ) %>%
    tab_source_note(html('<pre><span style="background-color: #9CC951;">
                         </span> = HSCP within the GGC Region</pre>'))
}

create_fytd_dd_table <- function(tib, title_name){
  tib %>%
    gt(rowname_col = "geog") %>%
    tab_header(title = md(glue(title_name))) %>%
    cols_label(Rate = md("Rate <br> (per 100,000)"),
               pc_change_prev = md("% Change on <br>previous <br>month")
    ) %>%
    fmt_number(columns = Rate, decimals = 1) %>%
    fmt_percent(columns = pc_change_prev, decimals = 1) %>%
    cols_width(
      Rank ~ px(50),
      Rate ~ px(120),
      pc_change_prev ~ px(120),
      everything() ~ px(250)
    ) %>%
    tab_style(
      locations = cells_body(
        columns = everything(),
        rows = "GGC HSCPs"
      ),
      style = cell_fill(color = "#9cc951")
    ) %>%
    tab_style(
      locations = cells_body(
        columns = everything(),
        rows = "Scotland"
      ),
      style = list(
        cell_fill(color = "#3f3685"),
        cell_text(color = "white")
      )
    ) %>%
    
    opt_align_table_header(align="left")
}

create_month_dd_table <- function(tib, title_name){
  tib %>%
    gt(rowname_col = "geog") %>%
    tab_header(title = md(glue(title_name))) %>%
    cols_label(Rate = md("Rate <br> (per 100,000)"),
               pc_change_prev = md("% Change on <br>previous <br>month"),
               pc_change_Avg_5yr = md("% Difference to 5-year average")
    ) %>%
    fmt_number(columns = Rate, decimals = 1) %>%
    fmt_percent(columns = c(pc_change_prev, pc_change_Avg_5yr), decimals = 1) %>%
    cols_width(
      Rank ~ px(50),
      Rate ~ px(120),
      pc_change_prev ~ px(120),
      pc_change_Avg_5yr ~ px(100),
      everything() ~ px(250)
    ) %>%
    tab_style(
      locations = cells_body(
        columns = everything(),
        rows = "GGC HSCPs"
      ),
      style = cell_fill(color = "#9cc951")
    ) %>%
    tab_style(
      locations = cells_body(
        columns = everything(),
        rows = "Scotland"
      ),
      style = list(
        cell_fill(color = "#3f3685"),
        cell_text(color = "white")
      )
    ) %>%
    opt_align_table_header(align="left")
}


# Other functions ------------------------------------------------------

get_sheetnames <- function(xl_file, sheet_names, pattern_string) {
  # extracts name of sheet from XL sheet
  sheet_names[str_detect(sheet_names, pattern = pattern_string)]
}


create_sheetname_vector <-   function(xl_file = xl_file) {
  # acquires all the sheetnames that begin with "LAD" or "HBD"
    sheet_names <- openxlsx::getSheetNames(file = xl_file)
    pattern_match <- c("LAData", "HBData")
    map(pattern_match, ~get_sheetnames(xl_file, sheet_names, .x)) %>%
      unlist()
}

convert_FY_to_calender <- function(tib){
  # takes first two digits of FY and converts it into calender year 20..
  tib %>%
    mutate (FY_begin = str_sub(FY, 1, 2)) %>%   # extracts last two digits from FY into <suffix year>
    mutate(FY_begin = as.numeric(glue("20{FY_begin}")) ) # adds "20" in front of <suffix year>. Converts to numeric
}

#inserts slash between years and
seperate_years_by_slash <- function(number){
  # adds / between years
  split_year <- number %>%
   str_sub(c(1,3), c(2,4))

  paste0(split_year, collapse = "/") %>%
    paste0("20",.)
}
# convert_calender_to_FY <- function(calyear){
#   first_year <- str_sub(calyear, -2, -1) %>%  as.numeric() # extract last 2 digits from cal year into <first year>
#   glue("{first_year}{first_year+1}")
# }





add_GGC_field <- function(df){
  # adds GGC_HSCP flag to tibble
  # Indicates if CA is in GGC Region (TRUE/FALSE)
  ggc_hcsps <- c("East Dunbartonshire", "East Renfrewshire", "Glasgow City", "Inverclyde",
                 "Renfrewshire", "West Dunbartonshire")

  df %>%
    mutate(GGC_Region = if_else(geog %in% ggc_hcsps, TRUE, FALSE))
}


get_FY_CA_data <- function(tib) {
  # converts monthly data into long format
  tib %>%
    select(-c("FYToDate")) %>%
    pivot_longer(cols = all_of(month_order), names_to = "Months", values_to = "Counts") %>%
    mutate(Months = factor(Months, levels = c(month_order, ordered = TRUE))) %>%
    group_by(geog, ComplexNeedsFlag, FY, FY_begin, Months) %>%
    summarise(Counts = sum(Counts), .groups = "drop") %>%
    ungroup()
}





## Select CAs from tibble ======================
get_council_area_names <- function(tib){
  tib %>%
    select(geog) %>%
    unique() %>%
    pull()
}

# gets historical counts for GGC region and for the prev 6 yrs excluding 2020/21

# get_current_year_tib <- function(tib, CA, CNeeds_string, fydate, pop_objfile=LA_pop_18plus){
#   tib %>%
#     filter(ComplexNeedsFlag == CNeeds_string) %>%
#     filter(FY %in% fydate) %>%
#     filter(geog == CA) %>%
#     drop_na(Counts) %>%
#     get_avg_rate(.,pop_objfile) %>%
#     rename(Current_rate = Avg_rate)
# }

create_arglist <- function(i, CNS, CA_of_interest){
  list(rates_permonth, delay_reason = CNS, CA_of_interest = CA_of_interest[i])
}

get_month_factor_level <- function(tib, mon) {
  tib %>%
    select(Months, Month_level) %>%
    distinct() %>%
    filter(Months == mon) %>%
    select(Month_level) %>%
    pull()
}


get_prev_5yr_avg_rate <- function(tib,  delay_reason,  mon, CA, pop_objfile=LA_pop_18plus){
  tib %>%
    filter(delay_reasons == delay_reason) %>%
  get_historic_counts(., CA) %>%
    get_avg_rate(., pop_objfile)
}


convert_year_into_FY_format <- function(cal_year){
  start_fy <- as.integer(str_sub(cal_year, 3, 4))
  #as.integer(str_sub(cal_year, 3, 4))
  end_fy <- start_fy+1
  as.integer(glue("{start_fy}{end_fy}"))
}

convert_calender_to_FY <- function(cal_year, month){
  if(month %in% c("January", "February", "March")){
    cal_year <- cal_year-1
  }
  convert_year_into_FY_format(cal_year)
}


# modifies join_current_prec_tibbles for GGC HSCP summary
modify_ggc_join_tibbles <- function(tib){
  tib %>%
  filter(GGC_Region==TRUE) %>%
  summarise(counts= sum(counts), Pop = sum(Pop), Prev_counts= sum(Prev_counts), Prev_Pop = sum(Prev_Pop)) %>%
  mutate(geog = "GGC HSCPs", counts, Pop, Rate = 100000 * counts/Pop, 
         Prev_counts, Prev_Pop, Prev_Rate = 100000 * Prev_counts/Prev_Pop )
}



 

# ---------------------------------------------------
get_all_5yr_stats <- function(tib=rates_permonth, delay_reason, CA_list, mon) {
  map_df(CA_list, ~get_historic_comparisons(tib, delay_reason, CA=.x, mon)) %>%
    bind_cols(geog = names(CA_list)) %>%
    select(geog, pc_change_Avg_5yr)

}







# --------------------------------------------------------------------
# determine range of FYs utilised in "prev 5 yr avg" - Example of East Dunbartonshire used
min_max_5yr_avg <- function() {

  range_5yr_avg <- get_historic_counts(tib, CA) %>%
    select(FY) %>%
    unique() %>%
    pull()

  min_FY <- min(range_5yr_avg) %>%
    seperate_years_by_slash()

  max_FY <- max(range_5yr_avg) %>%
    seperate_years_by_slash()

  list(min_FY, max_FY)

}

