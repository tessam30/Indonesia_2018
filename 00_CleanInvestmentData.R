# Purpose: Process portfolio data to be joined to shapefile in Talbeau
# Author: Tim Essam, Ph.D. | USAID GeoCenter
# Date: 2018_07_19
# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl")

dir.create("Data")
datapath <- "Data"
ind_invest_data <- "USAID Indonesia Investment Mapping.xlsx"

df <- read_excel(file.path(datapath, ind_invest_data), sheet = "Location Coded")

# Checking for anything odd in the data
  Hmisc::describe(df) # -- check each variable for missing and uniqueness
  names(df)
  str(df)

# How many unique districts are there? 362 should match up to the original
  df %>% 
    group_by(Province, District) %>% 
    tally() %>%  
    dim()
  
  # How many unique Province + District combinations? 326
  # But there are three Districts that appear more than once, but mapped to a different Province
  # This appears to differ from the shapefile where there are 503 Districts, but 502 unique District names
  df_unique <- 
    df %>% 
    filter(!is.na(District)) %>% 
    group_by(Province, District) %>% 
    tally()
  
  # 35 are truly NA, looks like one has a backtick " ` " as a value;
  # Fixing these in the Excel spreadsheet -- recommend using IDs moving forward
  #df %>% filter(is.na(District)) %>% group_by(Province, District) %>% tally() %>% dim()
  #str(table(df$District)
  
# Read in the spatial data to check the district names
  geo_df <- sf::read_sf(file.path(datapath, "IDN_BPS_Adm2Boundary.shp"))

  # Create a crosswalk with the Kabkot codes, name, and province; Remove the geometry
  # 502 Unique District
  geo_cw <- geo_df %>% 
    select(PROVNO, KABKOTNO, KODE2010, PROVINSI, KABKOT, KabCode_Nu, KODE2010B) %>% 
    st_set_geometry(NULL) # this is extra baggage and we do not need it, removing.
  str(geo_cw)  
  

# Compare Province and District Names / Numbers ---------------------------
  # Two tasks to do: 1)Compare number and names of Provinces in each dataset
  # 2) Compare districts and how many potentially should match (326 per above)

  prov_sf <- 
    geo_cw %>% 
    group_by(PROVINSI) %>% 
    tally()
  prov_df <- 
    df %>% 
    filter(Province != "NATIONWIDE") %>% 
    group_by(Province) %>% tally()
# Compare the two dataframes - NANGGROE ACEH DARUSSALAM should map to ACEH, KALIMANTAN UTARA IS MISSING
  setdiff(prov_df$Province, prov_sf$PROVINSI)
  # It appears that KALIMANTAN UTARA(NORTH), as of 2012, was carved out from East Kalimantan (KALIMANTAN TIMUR
  
  
# Investigate and reshape loaded data -------------------------------------


  
  # How many unique Province + Districts?
  geo_cw %>% group_by(PROVINSI, KABKOT) %>% tally() %>% dim()
  geo_cw %>% group_by(KABKOT) %>% tally() %>% dim()
  # Ignore dates and move on to reshape of the Funding data
  df_long <- df %>% 
    gather(starts_with("FY"), 
           key = Fiscal_year, 
           value = "amount") %>% 
    # filter out all rows that contain no information
    filter(amount != 0)

  # Check that total estimated costs add up ---------------------------------
  
  df_long <- df_long %>% 
    group_by(IM) %>% 
    
    # Create a TEC variable to check the math from Excel
    mutate(total_amt = sum(amount, na.rm = "TRUE")) %>% 
    ungroup() %>% 
    
    # Create a tolerance range that marks if the new TEC is different from the old
    mutate(TEC_diff = ifelse(near(TEC, total_amt, tol = 2), 1, 0)) %>% 
    
    # Create dummy variables to filter the level of geography,
    mutate(prov = ifelse(Granularity == "Provincial", 1, 0),
           dist = ifelse(Granularity == "District", 1, 0), 
           national = ifelse(Granularity == "Nationwide", 1, 0)) %>% 
    
    select(IM, amount, TEC, total_amt, TEC_diff, everything()) %>% 
    arrange(IM, Fiscal_year) 

  # Summarise result to show which IMs have problems
  # -- NOTES: all the DCA mechanisms are pre-tallied 
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

 tmp <-  df_long %>% 
   mutate(Province )
    select(District, Province) %>% 
    group_by(District, Province) %>% 
    arrange(desc(Province, District)) %>% 
    unique(.) %>% 
    anti_join(x = ., y = geo_cw, by = c("District" = "KABKOT", "Province" = "PROVINSI")) 
  
  
  # Print the Unique districts in the long data
  # Used this for Mission to decide how they would like to recode the 66 problemsome districts
    df_long %>% 
    select(District, Province) %>% 
    group_by(District, Province) %>% 
    # filter(!is.na(District) | District != "NA ") %>% 
    arrange(desc(Province, District)) %>% 
    unique(.) %>% 
    anti_join(x = ., y = geo_cw, by = c("District" = "KABKOT")) 
  
