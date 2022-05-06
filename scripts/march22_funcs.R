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
# functions in order of use ----------------------------------------
get_prev_7yr_rates <- function(df, CNeeds_list, CA_of_interest, fydate, pop_objfile=LA_pop_18plus){
  df_split <- df %>%
    filter(geog %in% CA_of_interest) %>% 
    filter(delay_reasons %in% CNeeds_list) %>%
    split(.$delay_reasons) 
  
  map(df_split, function(x) 
    map(CA_of_interest, function(y) get_rates(tib = x, CA= y, pop_objfile))%>% 
          set_names(CA_of_interest_named_vec)
  ) 
} 

get_rates <- function(tib, CA, pop_objfile=LA_pop_18plus){
  #get last 7yrs data (excluding 2021) in terms of rates
  #assumes tibble has unique ComplexNeedsFlag value
  
  #creates ordered factor fin_yr and joins with pop data
  tib_joined <- tib %>% 
    left_join(pop_objfile, by = c("geog" = "CouncilArea",  "year" )) %>% 
    mutate()
  
  #create tibble of pre current values- ignoring year "2020/21"  
  prev5_tib <- get_historic_counts(tib_joined, CA) 
  
  
  min_max_rate <- get_minmax_rates(prev5_tib)
  avg_rate <- get_avg_rate(prev5_tib)
  total_counts_pop <- get_sum_counts_pop(prev5_tib)
  
  #join min, max, avg tibbles together and rename to prev5_rate
  prev5_rate <- left_join(min_max_rate, avg_rate, by = "months") %>% 
    left_join(., total_counts_pop, by = "months") 
  
  
  #create current rates, join with prev5 year rates 
  current_rate <- get_current_rate(tib_joined, CA)  
  
  current_totals <- get_current_totals(tib_joined, CA)

  
  df <- left_join(prev5_rate, current_rate, by = "months") %>% 
    left_join(.,  current_totals, by = "months")  %>% 
    mutate(Month_labels = case_when(months == 1 ~ "Apr", 
                                   months == 2 ~ "May", 
                                   months == 3 ~ "Jun", 
                                   months == 4 ~ "Jul", 
                                   months == 5 ~ "Aug", 
                                   months == 6 ~ "Sep", 
                                   months == 7 ~ "Oct", 
                                   months == 8 ~ "Nov", 
                                   months == 9 ~ "Dec", 
                                   months == 10 ~ "Jan", 
                                   months == 11 ~"Feb", 
                                   TRUE ~ "Mar"))
  
  list(df, prev5_tib)
}

# functions of "get_rates" ------------------

 # vector with ordered levels of fin_yr
create_fctorder_finyr <- function(tib) {
  fin_yr_order_fct <- 
    tib %>% 
    mutate(fin_yr = factor(fin_yr)) %>% 
    select(fin_yr) %>% 
    pull() %>% 
    levels() %>% 
    rev()
  
  tib %>% 
    mutate(fin_yr = factor(fin_yr, levels = fin_yr_order_fct, ordered = TRUE)) %>% 
    select(fin_yr) %>% 
    pull() 
}

get_historic_counts <- function(tib, CA) {
  
  # level =1  is for most recent date
  # and remove yr 2020/21 for analysis
  tib1 <- tib %>% 
    filter(fin_yr != "2020/21")
  
  # remove "2020/21" from levels 
  tib1$fin_yr <- droplevels(tib1$fin_yr) 
  
  # filters the 2nd to 6th most recent year (prev 5 yrs) excluding 2020/21 year
  tib1 %>% 
    filter(as.integer(fin_yr)>1, as.integer(fin_yr)<=6) %>% 
    filter(geog %in% CA) 
}


get_minmax_rates <- function(tib){
  # Select min and max historical rates 
  
  tib %>% 
    mutate(Rate = 100000*obd/Pop) %>% 
    group_by(months) %>% 
    summarise(Min_rate = min(Rate), Max_rate = max(Rate))
}

get_avg_rate <- function(tib){
  ## Select avg historical rates
  #calculates Avg counts for each Month 
  tib %>% 
    group_by(months) %>% 
    summarise(Avg_rate = 100000*sum(obd)/sum(Pop), .groups = "drop") 
}

get_sum_counts_pop <- function(tib){
  tib %>% 
    group_by(months) %>% 
    summarise(Total_Counts = sum(obd), Total_Pop = sum(Pop)) %>% 
    ungroup()
}

#function used in get_current_rates and get_current_totals
get_pre_currents <- function(tib, CA) {
  tib %>% 
    filter(fin_yr == fydate_text) %>% 
    filter(geog == CA) %>% 
    drop_na(obd) %>% 
    mutate(Pop = if_else(is.na(Pop), lag(Pop, n=sum(is.na(Pop))), Pop)) 
}

get_current_rate <- function(tib, CA){
  get_pre_currents(tib, CA) %>% 
    get_avg_rate() %>% 
    rename(Current_rate = Avg_rate) 
}


get_current_totals <- function(tib, CA) {
  get_pre_currents(tib, CA) %>%  
    get_sum_counts_pop() %>% 
    rename(Current_Counts = Total_Counts, Current_Pop = Total_Pop)
  
}  

#  Create 7 yr plots for each region of interest -----------------
create_7yr_plot <- function(tibble_name = all_rates, index=NULL, CNeeds_string=NULL, CA_of_interest) {
  #get_prev5 selects last 6 years but excludes FY 20/21 
  #tibble_name = all_rates, index=NULL, CNeeds_string, CA_of_interest assumes earliest current year is 21
  #extract current FY from df
  
  fun_df <- tibble_name[[CNeeds_string]][[index]]
  
  #values for most recent and distant FY used in calculating prev 5 years data
  recent_prev_yrs <- create_fctorder_finyr(fun_df[[2]]) %>% 
    max()
  
  earliest_prev_yrs <- create_fctorder_finyr(fun_df[[2]]) %>% 
    min()
    

  
  # Plotly plot
  #sets margin values in mrg variable
  mrg <- list(l = 50, r = 50,
              b = 100, t = 100,
              pad = 20)
  
  plot_ly(fun_df[[1]], x= ~Month_labels) %>% 
    
    add_lines( y=~Current_rate, 
               name = glue("FY {fydate_text}"),
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
                                                   Previous 5-year Average {earliest_prev_yrs} to {recent_prev_yrs}) - {CNeeds_string} Delays 
                                                   ({params$Report_month} {params$Report_year}) "), width = 70), collapse = "\n"), x=0.05, pad = list(b=120)),
           xaxis = list(title = ""),
           yaxis = list(title = "Bed days rate (per 100,000 population)"),
           legend = list(x = 1, y = 0.9)
    ) 

}

# create 7r Tables ------------------
create_current_table <- function(tibble_name = all_rates, index=NULL, CNeeds_string, CA_of_interest){
  
  fun_df <- tibble_name[[CNeeds_string]][[index]]
  
  temp_df <- fun_df[[1]] %>%
    select(Month_labels, Current_Counts) %>%
    pivot_wider(names_from = "Month_labels", values_from = "Current_Counts") %>%
    mutate(across(Apr:Mar, ~(scales::label_comma(accuracy = 1)(.x))))
  
  #rename current_FY with slash
  Current_FY <- #need to get current FY
  
  temp_df %>%
    select(-Current_FY) %>%
    gt() %>%
    tab_header(
      title = glue("Number of Monthly Bed Days in 20{Current_FY_label}")
    ) %>%
    cols_width(
      everything() ~ px(70)
    )
  
}


# Create tibbles for analysis ----------------------------------------------------
get_CA_bed_rates <- function(tib, CNeeds_string, fydate, mon, FYTD_flag) {
  #get bed rates (per 100,000) for each region, year or Month/year as well as member of CCGRegion.
  # Rates dependent on pop of that specific year!
  # enter tibble, and element of ComplexNeedsFlag
  tib <- tib %>%
    filter(ComplexNeedsFlag == CNeeds_string) %>%
    filter(FY %in% fydate)
    # %>%
  #filter(CouncilArea != "Other")

  if (!is_true(FYTD_flag)){
    tib %>%
      filter(Months %in% mon) %>%
      # mutate(Months = factor(Months, levels = c(month_order, ordered = TRUE))) %>%
      left_join(LA_pop_18plus, by = c("CouncilArea", "FY_begin" = "year")) %>%
      group_by(CouncilArea, FY, Months, GGCRegion) %>%
      drop_na() %>%
      summarise(Counts = sum(Counts), Pop = mean(Pop), .groups = "drop") %>%
      mutate(Rate = Counts/Pop *100000) %>%
      ungroup()
  }
  else {
    tib <- tib %>%
      mutate(Month_level = as.numeric(Months)) #create numeric month field

    month_value <- get_month_factor_level(tib, mon) # convert mon into numeric

   left_join(tib, LA_pop_18plus, by = c("CouncilArea", "FY_begin" = "year")) %>%
     filter(Month_level <= month_value) %>%
     group_by(CouncilArea, FY, GGCRegion) %>%
     # filter by mon value
     summarise(Counts = sum(Counts), Pop = mean(Pop), .groups = "drop") %>%
     mutate(Rate = Counts/Pop *100000) %>%
     ungroup()
  }
}

get_rank <- function(tib, CNeeds_string, fydate, mon, FYTD_flag){
  #Ranks CAs according to Rate - lower rating for lower Rate

  #Get bedrates and rank (includes Scotland)
  tib <- get_CA_bed_rates(tib, CNeeds_string, fydate, mon, FYTD_flag) %>%
    mutate(Rank = min_rank(Rate))

    # assign Scotland rank as NA
    tib$Rank[tib$CouncilArea=="Scotland"] <- NA

    #rerank without Scotland
    tib %>%  mutate(Rank = min_rank(Rank)) %>%
      select(CouncilArea, Rank)
}


get_last_change <- function(tib, CNeeds_string, fydate, mon, FYTD_flag, pop_objfile = LA_pop_18plus) {
  #get bed rates (per 100,000) for each region, year or Month/year as well as member of CCGRegion.
  # Rates dependent on pop of that specific year!
  # enter tibble, and element of ComplexNeedsFlag

  #tib = tib_FY_CA
  #current bedrates
  #current_BR <- get_CA_bed_rates(tib, CNeeds_string, fydate, mon, FYTD_flag)


  #calculate previous month's bedrate ##

  # assign numeric value in tibble for month
  tib_new <- tib %>%
    filter(ComplexNeedsFlag == CNeeds_string) %>%
    mutate(Month_level = as.numeric(Months))

  # get month level as an integer
  mon_level <- get_month_factor_level(tib_new, mon)

  ## get previous month's value.

  if(!is_true(FYTD_flag)){
    #if April, then get March of previous FY
    if(mon_level == 1){
       preceeding_level = 12
       preceeding_level_FY = fydate-101
    } else {
       preceeding_level = mon_level-1
       preceeding_level_FY = fydate
    }
    #calculate previous months counts and store tibble
    tib_new %>% filter(Month_level == preceeding_level,
                                  FY == preceeding_level_FY) %>%
        left_join(., pop_objfile, by = c("CouncilArea", "FY_begin" = "year")) %>%
        mutate(Prev_Counts = Counts, Prev_Pop = Pop) %>%
        select(CouncilArea, Prev_Counts, Prev_Pop)  %>%
        mutate(Prev_Rate = 100000 * Prev_Counts/Prev_Pop)

    } else #if !is_true (FYTD_flag)
      {
        # Previous rate calculated by prev pop (and is also displayed)
        tib1 <- tib_new %>%
          filter(FY == fydate-101)

          left_join(tib1, pop_objfile, by = c("CouncilArea", "FY_begin" = "year")) %>%
            filter(Month_level <= mon_level) %>%
            group_by(CouncilArea, FY, FY_begin) %>%
            # filter by mon value
            summarise(Prev_Counts = sum(Counts), Prev_Pop = mean(Pop), .groups = "drop") %>%
            mutate(Prev_Rate = 100000 * Prev_Counts/Prev_Pop) %>%
            ungroup()
         }
}

get_group_bed_rates <- function(..., ggc_flag = FALSE, scot_flag = FALSE){
  ## Get bedrates independent of scot, ggc or no flags

  if (scot_flag == TRUE) {
    get_CA_bed_rates(...) %>%
      filter(CouncilArea == "Scotland")

  } else if (ggc_flag == TRUE){

    arglist <- list(...)

    arglist[[1]] <- arglist[[1]] %>%
      filter(GGCRegion == TRUE)

    #default value of mon = NULL for CA_bed_rate function, in case looking at upto FY rates
    # if(length(arglist) == 4){
    #   mon=arglist[[4]]
    # } else if (length(arglist) == 3) {
    #   mon=NULL
    # } else {
    #   #message("arglist in function get_group_bed_rates = ", length(arglist))
    # }


    do.call(get_CA_bed_rates, arglist) %>%
      group_by(GGCRegion) %>%
      summarise(Counts = sum(Counts), Pop = sum(Pop)) %>%
      mutate(Rate = Counts/Pop *100000) %>%
      ungroup()

  } else get_CA_bed_rates(...)
}



get_pc_change_prev_time <- function(tib, CNeeds_string,
                                    fydate, mon, FYTD_flag = FALSE, pop_objfile = LA_pop_18plus) {
  #Individual HSCP regions
  tab_month1 <- join_current_prec_tibbles(tib, CNeeds_string,
                                          fydate, mon, FYTD_flag, pop_objfile) %>%
    filter(GGCRegion == TRUE) %>% # for GGC HSCP summary only
    get_now_prev_rates(.) %>%
    select(CouncilArea, Rate, pc_change_prev)

  # creates output for GGC HSCP only
  tab_month2 <- join_current_prec_tibbles(tib, CNeeds_string,
                                          fydate, mon, FYTD_flag, pop_objfile) %>%
    modify_ggc_join_tibbles(.) %>% # for GGC HSCP summary only
    get_now_prev_rates(.) %>%
    select(CouncilArea, Rate, pc_change_prev)
  # Scotland region
  tab_month3 <- join_current_prec_tibbles(tib, CNeeds_string,
                                          fydate, mon, FYTD_flag, pop_objfile) %>%
    filter(CouncilArea == "Scotland") %>% # for GGC HSCP summary only
    get_now_prev_rates(.) %>%
    select(CouncilArea, Rate, pc_change_prev)

  bind_rows(tab_month1, tab_month2, tab_month3)
}

# join current and preceeding tibbles together
join_current_prec_tibbles <- function(tib, CNeeds_string,
                                      fydate, mon, FYTD_flag, pop_objfile){
  left_join(get_CA_bed_rates(tib, CNeeds_string,
                             fydate, mon, FYTD_flag), get_last_change(tib, CNeeds_string,
                                                                      fydate, mon, FYTD_flag, pop_objfile), by = "CouncilArea")
}

create_appendix <- function(tib=tib_FY_CA, CNeeds_string, fydate, mon, FYTD_flag) {
  get_group_bed_rates(tib, CNeeds_string, fydate, mon, FYTD_flag) %>%
    filter(!CouncilArea %in% c("Scotland","Other")) %>%
    left_join(., get_rank(tib, CNeeds_string, fydate, mon, FYTD_flag), by = "CouncilArea") %>%
    arrange(Rank) %>%
    select(-c(FY, GGCRegion))
}

# Create plots and tables for display -----------------------------------

create_CArate_plot <- function(df, CNeeds_string, fydate, mon, FYTD_flag, g_rate = ggc_rate, s_rate = scot_rate){
  ## plot of rates for all CAs

  df <- df %>%
    filter(CouncilArea != "Other")

  if (is_true(FYTD_flag)){
    #sub_str aquires first two digits from FY xxxx value
    mon_title <- glue("April 20{str_sub(fydate, 1, 2)} to {params$Report_month} {params$Report_year}")
  } else {# CORRECT THIS WHERE MONTHS DONT EXiST!!!!!!!!!
    #mon_value <- df %>% slice(1) %>% select(Months) %>% pull() %>% as.character()
    mon_title <- glue("{params$Report_month} {params$Report_year}")
    #mon_title <- c(glue("{mon_value} "))
  }

  p <-   ggplot(df, aes(x=reorder(CouncilArea, Rate), y=Rate, fill = GGCRegion))+
    geom_col(show.legend = FALSE)+
    geom_text(aes(label = format(signif(as.integer(Rate,3)))), hjust = 1, colour = "white") +
    scale_fill_manual(name= "GGC Area", values =c("#3393dd", "#9cc951"))+
    guides(fill = guide_legend(reverse=TRUE))+
    geom_hline(yintercept=s_rate[[1]], color = "#3f3685")+
    geom_hline(yintercept=g_rate[[1]], color = "#83bb26")+
    coord_flip()+
    labs(title = str_wrap(glue("Delayed Discharge Bed Days Rate by HSCP - {CNeeds_string} Delays ({mon_title})"), width = 60),
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


get_scot_ggc_CA_rates <- function(tib, CNeeds_string, fydate, mon, FYTD_flag=FALSE) {
  ## create CAs plot automatically according to flags

  #scot_rate and ggc_rate are lists containing the numeric value and the displayed char value to 1 dp
  #get scotland rate
  scot_rate <-   get_group_bed_rates(tib, CNeeds_string, fydate, mon, FYTD_flag, ggc_flag=FALSE, scot_flag=TRUE) %>%
    get_group_bedrate_value()


  #get ggc_rate

  ggc_rate <- get_group_bed_rates(tib, CNeeds_string, fydate, mon, FYTD_flag, ggc_flag=TRUE, scot_flag=FALSE) %>%
    get_group_bedrate_value()

  # ggc_rate <- get_group_bedrate_value(ggc_rate)
  #
  #     #get_all_CA_rates
  all_CA_rates <-  get_group_bed_rates(tib, CNeeds_string, fydate, mon, FYTD_flag) %>%
    filter(CouncilArea != "Scotland")

  list(scot_rate, ggc_rate, all_CA_rates)
  # # plot horizontal column plot
 # create_CArate_plot(df=all_CA_rates, CNeeds_string, fydate, mon, FYTD_flag, g_rate = ggc_rate, s_rate = scot_rate)
}


create_7yr_table <- function(tibble_name = all_rates, index=NULL, CNeeds_string, CA_of_interest){

  fun_df <- tibble_name[[CNeeds_string]][[index]]

  temp_df <- fun_df %>%
    select(Month_labels, Current_FY, Current_Counts) %>%
    pivot_wider(names_from = "Month_labels", values_from = "Current_Counts") %>%
    mutate(across(Apr:Mar, ~(scales::label_comma(accuracy = 1)(.x))))

  #rename current_FY with slash
  Current_FY_label <- seperate_years_by_slash(temp_df$Current_FY)

  temp_df %>%
    select(-Current_FY) %>%
    gt() %>%
    tab_header(
      title = glue("Number of Monthly Bed Days in 20{Current_FY_label}")
    ) %>%
      cols_width(
        everything() ~ px(70)
      )

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



create_FY_authority <- function(xl_file, sheet_name, delay_reason) {
  ## Creates tibble for each sheet
  # creates tibble for each sheet name, removes Qtr fields ,
  # adds Finanical Year field, creates type of authority field (LA or HB)
  # renames LA/health board fields and filters "All" for AgeGroup
  read_excel(xl_file, sheet = sheet_name) %>%
    select(-c(lookup, month_date)) %>%
    filter(age_groups == "18+", delay_reasons == delay_reason)
}


add_GGC_field <- function(df){
  # adds GGC_HSCP flag to tibble
  # Indicates if CA is in GGC Region (TRUE/FALSE)
  ggc_hcsps <- c("East Dunbartonshire", "East Renfrewshire", "Glasgow City", "Inverclyde",
                 "Renfrewshire", "West Dunbartonshire")

  df %>%
    mutate(GGCRegion = if_else(CouncilArea %in% ggc_hcsps, TRUE, FALSE))
}


get_FY_CA_data <- function(tib) {
  # converts monthly data into long format
  tib %>%
    select(-c("FYToDate")) %>%
    pivot_longer(cols = all_of(month_order), names_to = "Months", values_to = "Counts") %>%
    mutate(Months = factor(Months, levels = c(month_order, ordered = TRUE))) %>%
    group_by(CouncilArea, ComplexNeedsFlag, FY, FY_begin, Months) %>%
    summarise(Counts = sum(Counts), .groups = "drop") %>%
    ungroup()
}


get_group_bedrate_value <- function(df){
  ### Extract Rate field row as single value
  value <- df %>%
    select(Rate) %>%
    pull()

   title <- df %>%
    select(Rate) %>%
    mutate(Rate = format(round(Rate,1))) %>%
    pull()

  list(value, title)
}


## Select CAs from tibble ======================
get_council_area_names <- function(tib){
  tib %>%
    select(CouncilArea) %>%
    unique() %>%
    pull()
}

# gets historical counts for GGC region and for the prev 6 yrs excluding 2020/21

# get_current_year_tib <- function(tib, CA, CNeeds_string, fydate, pop_objfile=LA_pop_18plus){
#   tib %>%
#     filter(ComplexNeedsFlag == CNeeds_string) %>%
#     filter(FY %in% fydate) %>%
#     filter(CouncilArea == CA) %>%
#     drop_na(Counts) %>%
#     get_avg_rate(.,pop_objfile) %>%
#     rename(Current_rate = Avg_rate)
# }

create_arglist <- function(i, CNS, CA_of_interest){
  list(all_rates, CNeeds_string = CNS, CA_of_interest = CA_of_interest[i])
}

get_month_factor_level <- function(tib, mon) {
  tib %>%
    select(Months, Month_level) %>%
    distinct() %>%
    filter(Months == mon) %>%
    select(Month_level) %>%
    pull()
}


get_prev_5yr_avg_rate <- function(tib,  CNeeds_string,  mon, CA, pop_objfile=LA_pop_18plus){
  tib %>%
    filter(delay_reasons == CNeeds_string) %>%
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
  filter(GGCRegion==TRUE) %>%
  summarise(Counts = sum(Counts), Pop = sum(Pop), Prev_Counts = sum(Prev_Counts), Prev_Pop = sum(Prev_Pop)) %>%
  mutate(CouncilArea = "GGC HSCPs")
}


get_now_prev_rates <- function(tib){
  tib %>%
  mutate(Rate = 100000*Counts/Pop, Prev_Rate = 100000*Prev_Counts/Prev_Pop,
         pc_change_prev = ((Rate-Prev_Rate)/Prev_Rate))
}
  # create tibble of CA regions of interest
#map_df(CA_of_interest, ~get_prev_5yr_avg_rate(tib_FY_CA, .x, fydate, mon="January"))

get_5yr_comparisons <- function(tib = all_rates, CNeeds_string, CA, mon) {
  tib[[CNeeds_string]][[CA]]%>%
    filter(Months == mon) %>%
    mutate(pc_change_Avg_5yr = ((Current_Counts/Current_Pop) - (Total_Counts/Total_Pop))/(Total_Counts/Total_Pop)) %>%
    mutate(CouncilArea = names(CA)) #%>%
  # select(CouncilArea, pc_change_Avg_5yr)
}

# ---------------------------------------------------
get_all_5yr_stats <- function(tib=all_rates, CNeeds_string, CA_list, mon) {
  map_df(CA_list, ~get_5yr_comparisons(tib, CNeeds_string, CA=.x, mon)) %>%
    bind_cols(CouncilArea = names(CA_list)) %>%
    select(CouncilArea, pc_change_Avg_5yr)

}

# calculate % change of the combined GGC HSCPs of current value compared to the prev 5 year avg
# Excludes year 20/21
aggregate_5yr_ggc <- function(tib=all_rates, CNeeds_string, CA_no_Scotland, mon) {
  map_df(CA_no_Scotland, ~get_5yr_comparisons(tib, CNeeds_string, CA=.x, mon)) %>%
    bind_cols(CouncilArea = names(CA_no_Scotland)) %>%
    summarise(Prev_Counts = sum(Total_Counts), Prev_Pop = sum(Total_Pop),
              Current_Counts = sum(Current_Counts), Current_Pop = sum(Current_Pop)) %>%
    mutate(CouncilArea = "GGC HSCPs",
           pc_change_Avg_5yr = ((Current_Counts/Current_Pop)- (Prev_Counts/Prev_Pop))/ (Prev_Counts/Prev_Pop)) %>%
    select(CouncilArea, pc_change_Avg_5yr)
}

# calculates % change of current vs prev 5 year avg for GGC regions and Scotland
# Excludes year 20/21
get_all_5yr_rates <- function(tib=all_rates, CNeeds_string, CA_list, CA_no_Scotland, mon) {
   get_all_5yr_stats(tib=all_rates, CNeeds_string, CA_list, mon) %>%
    bind_rows(., aggregate_5yr_ggc(tib=all_rates, CNeeds_string, CA_no_Scotland, mon))

}

get_summary_table <- function(mon, CNeeds_string, FYTD_flag) {
get_pc_change_prev_time(tib_FY_CA, CNeeds_string, fydate, mon, FYTD_flag, pop_objfile = LA_pop_18plus) %>%
  left_join(., get_rank(tib_FY_CA, CNeeds_string, fydate, mon, FYTD_flag), by = "CouncilArea") %>%
  left_join(., get_all_5yr_rates(all_rates, CNeeds_string, CA_list = CA_of_interest, CA_no_Scotland, mon), by = "CouncilArea") %>%
  relocate(Rank, .after = "CouncilArea")
}

# --------------------------------------------------------------------
# determine range of FYs utilised in "prev 5 yr avg" - Example of East Dunbartonshire used
min_max_5yr_avg <- function() {

  range_5yr_avg <- get_historic_counts(tib CA) %>%
    select(FY) %>%
    unique() %>%
    pull()

  min_FY <- min(range_5yr_avg) %>%
    seperate_years_by_slash()

  max_FY <- max(range_5yr_avg) %>%
    seperate_years_by_slash()

  list(min_FY, max_FY)

}

# ---------------------------------------------------
get_written_summary_data <- function(tib) {
  ggc_higher_scot_rates <- tib[[3]]%>%
    filter(GGCRegion == TRUE) %>%
    filter(Rate > tib[[1]])

  count = nrow(ggc_higher_scot_rates)

  regions <- ggc_higher_scot_rates %>%
    select(CouncilArea) %>%
    pull()

  list(count, regions)
}

create_appendix1 <- function(tib, title_name){
  tib %>%
    gt(rowname_col = "CouncilArea") %>%
    tab_stubhead(label = "HSCP") %>%
    tab_header(title = md(glue(title_name))) %>%
    cols_label(Rate = md("Rate <br> (per 100,000)"),
               Pop = md("Population (18+)"),
               Counts = md("Number of <br> Bed Days")
    ) %>%
    fmt_integer(columns = c(Counts, Pop, Rank)) %>%
    fmt_number(columns = Rate, decimals = 1) %>%
    cols_width(
      Rank ~ px(50),
      Rate ~ px(120),
      Counts ~ px(100),
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
    select(-Months) %>%
    gt(rowname_col = "CouncilArea") %>%
    tab_stubhead(label = md("HSCP")) %>%
    tab_header(title = md(glue(title_name))) %>%
    cols_label(Rate = md("Rate <br> (per 100,000)"),
               Pop = md("Population (18+)"),
               Counts = md("Number of <br> Bed Days")
    ) %>%
    fmt_integer(columns = c(Counts, Pop, Rank)) %>%
    fmt_number(columns = Rate, decimals = 1) %>%
    cols_width(
      Rank ~ px(50),
      Rate ~ px(120),
      Counts ~ px(100),
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
    gt(rowname_col = "CouncilArea") %>%
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
    gt(rowname_col = "CouncilArea") %>%
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
