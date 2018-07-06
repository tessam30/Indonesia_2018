
# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl")

dir.create("Data")
datapath <- "Data"
ind_invest_data <- "All_FY15_18 copy.xlsx"

# Import the Excel data from -- 3 more rows than the excel file, so strip out th emissing
  df <- read_excel(file.path(datapath, ind_invest_data), sheet = "USAID Indonesia Investment Mapp")
  df <- df %>% 
    filter(!is.na(IM))
  
  Hmisc::describe(df) # -- check each variable for missing and uniqueness
  names(df)
  
  # Some of the dates are inconsistent and/or missing so the dates is coerced to a double instead of a date
  # Filed as an issue and will wait to hear back from the Mission
  table(df$StartDate) # -- some dates read in as years, some as Excel converted numbers. Need to standardize
  
# Ignore dates and move on to reshape of the Funding data
  
  df_long <- df %>% 
    gather(starts_with("FY"), 
           key = Fiscal_year, 
           value = "amount")


# Check that total estimated costs add up ---------------------------------

 df_long <- df_long %>% 
    group_by(IM) %>% 
    
    # Create a TEC variable to check the math from Excel
    mutate(total_amt = sum(amount, na.rm = "TRUE")) %>% 
    ungroup() %>% 
    
    # Create a tolerance range that marks if the new TEC is different from the old
    mutate(TEC_diff = ifelse(near(TEC, total_amt, tol = 2), 1, 0)) %>% 
    select(IM, amount, TEC, total_amt, TEC_diff, everything()) %>% 
    arrange(IM, Fiscal_year) 
  
    # Summarise result to show which IMs have problems
    # -- NOTES: all the DCA mechanisms are pre-tallied 
  options(digits = 7)
   df_long %>% 
     select(IM, TEC_diff, TEC, total_amt) %>% 
     filter(TEC_diff == 0) %>% 
     group_by(IM) %>% 
     summarise(
       TEC = mean(TEC), 
       FY_amount = mean(total_amt, na.rm = TRUE),
       diff = TEC - FY_amount
     ) %>% 
     arrange(desc(diff)) %>% 
     knitr::kable()
   
   
   
  

 
   


    