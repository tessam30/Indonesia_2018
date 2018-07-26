# Purpose: Process portfolio data to be joined to shapefile in Talbeau
# Author: Tim Essam, Ph.D. | USAID GeoCenter
# Date: 2018_07_19
# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl")

# TODO: turn into a package
  files <- c("compare_vars.R", "strip_geom.R", "fltr_func.R")  
  map(files, source)


  dir.create("Data")
  datapath <- "Data"
  ind_invest_data <- "USAID Indonesia Investment Mapping.xlsx"

df <- read_excel(file.path(datapath, ind_invest_data), sheet = "Location Coded")

# Checking for anything odd in the data
  Hmisc::describe(df) # -- check each variable for missing and uniqueness
  names(df)
  str(df)

# How many unique districts are there? 324 KABKOT_IDs but 325 Districts. But it appears that one of the District entries is invalid "`" --> linked to Province == "JAWA TENGAH"
  df <- 
    df %>% 
    mutate(District = ifelse(District != "`", 
                             District, 
                             NA_character_))
  
  # Now seems to be consistend across KABKOT_ID and District names
  # Count is equivalent to group_by() + tally()
  dist <- df %>% 
    filter(!is.na(District)) %>% 
    count(Province, District, KABKOT_ID)
  str(df)

# Read in the spatial data to check the district names
  admin2_df <- sf::read_sf(file.path(datapath, 
                                  "BPS_2013Adm 2_Boundary",
                                  "BPS_Admin2Boundary_2013.shp"))
  names(admin2_df)
  
  # Now the Admin1 data
  admin1_df <- sf::read_sf(file.path(datapath,
                                     "BPS_2013Adm1_Boundary",
                                     "BPS_Admin1Boundary_2013.shp"))
  names(admin1_df)
  
  # Create a crosswalk with the Kabkot codes, name, and province; Remove the geometry
  # 502 Unique District
  admin2_cw <- strip_geom(admin2_df, OBJECTID, KABKOT, KABKOT_ID, PROVINSI)
  admin1_cw <- strip_geom(admin1_df, OBJECTID, PROVINSI, Region)
  
# Compare Province and District Names / Numbers ---------------------------
  # Two tasks to do: 1)Compare number and names of Provinces in each dataset
  # 2) Compare districts and how many potentially should match (326 per above)

  prov_sf <- 
    admin2_cw %>% 
    # Use count to skip the group_by step
    count(PROVINSI)

  prov_df <- 
    df %>% 
    filter(Province != "NATIONWIDE") %>% 
    count(Province)
  
  # Compare the two dataframes - East and North Province issue resolved with new shapefile
  # There are no differences between the KABKOT IDs when joining, so things should be good to go
  compare_vars(prov_df$Province, prov_sf$PROVINSI)
  compare_vars(dist$KABKOT_ID, admin2_cw$KABKOT_ID)
  
  # How many Districts merge to the Admin2 shapefile data?
  dist_join <- 
    dist %>% 
    left_join(x = ., y = admin2_cw, by = c("KABKOT_ID"))
    
  
# Investigate and reshape loaded data -------------------------------------

  # Dates have been resolved by the Mission -- even POSIXct
  # Reshape the data based on Fiscal year dates
  df_long <- df %>% 
    gather(starts_with("FY"), 
           key = Fiscal_year, 
           value = "amount") %>% 
    # filter out all rows that contain no information
    filter(amount != 0)
  
  # Mission asked for 3 data sets, Nation-wide, Provincal and District
  unique(df_long$Granularity)


  # Check that total estimated costs add up ---------------------------------
  
  df_long <- 
      df_long %>% 
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

# Export different cuts of data
  exp_list <- c("IND_investments_dist.csv",
                "IND_investments_prov.csfv",
                "IND_investments_natl.csv",
                "IND_investments_all.csv")
  
  
  df_long_dist <- fltr_func(df_long, Granularity == "District") 
  df_long_prov <- fltr_func(df_long, Granularity == "Provincial")
  df_long_natl <- fltr_func(df_long, Granularity == "Nationwide")
  df_list <- c(df_)
  
  
  

  
