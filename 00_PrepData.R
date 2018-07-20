
# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl")

dir.create("Data")
datapath <- "Data"
ind_invest_data <- "All_FY15_18 copy.xlsx"

# Import the Excel data from -- 3 more rows than the excel file, so strip out th emissing
  df <- read_excel(file.path(datapath, ind_invest_data), 
                   sheet = "USAID Indonesia Investment Mapp")
  df <- df %>% 
    filter(!is.na(IM))
  
  Hmisc::describe(df) # -- check each variable for missing and uniqueness
  names(df)
  
  # Some of the dates are inconsistent and/or missing so the dates is coerced to a double instead of a date
  # Filed as an issue and will wait to hear back from the Mission
  # -- TODO: Need to standardize
  table(df$StartDate) # -- some dates read in as years, some as Excel converted numbers. 
 
  # How many unique districts are there? 362 should match up to the original
  df %>% group_by(Province, District) %>% tally() %>%  dim()
  
  
  # Read in the spatial data to check the district names
  geo_df <- sf::read_sf(file.path(datapath, "IDN_BPS_Adm2Boundary.shp"))

  # Create a crosswalk with the Kabkot codes, name, and province; Remove the geometry
  geo_cw <- geo_df %>% 
    select(PROVNO, KABKOTNO, KODE2010, PROVINSI, KABKOT, KabCode_Nu, KODE2010B) %>% 
    st_set_geometry(NULL) # this is extra baggage and we do not need it, removing.
  str(geo_cw)
  
  
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
  
   # Print the Unique districts in the long data
   # Used this for Mission to decide how they would like to recode the 66 problemsome districts
   df_long %>% 
     select(District, Province) %>% 
     group_by(District, Province) %>% 
     # filter(!is.na(District) | District != "NA ") %>% 
     arrange(desc(Province, District)) %>% 
     unique(.) %>% 
     full_join(x = ., y = geo_cw, by = c("District" = "KABKOT")) %>% 
    
    # exporting for Mission to view
     write_csv(., file.path(datapath, "District_Province_list.csv")) %>% 
     filter(is.na(PROVNO) & !is.na(District)) %>% 
     select(District, Province, PROVNO) %>% 
     arrange(desc(Province, District)) %>% 
     
     # print and store in a new data frame
     print(n = 66) -> mismatches
   
   geo_cw %>% 
     write_csv(., file.path(datapath, 
                            "Admin2_shapefile_atrributes.csv"))
     
   # How many districts have information compared to the full universe of districts
   # in a shapefile for the Admin2? 79 out of 503 do not join
   df %>% select(District, Province) %>% 
     group_by(District) %>% 
     tally() %>% 
     anti_join(x = ., y = geo_cw, by = c("District" = "KABKOT"))
 
  

# Create export for Tableau -----------------------------------------------
  write_csv(df_long, file.path(datapath, "IND_portfolio_2018_07.csv"))

# Merge geo data attributes with tabular data -----------------------------
   
   # Check that you can reproduce excel pivot table for BENGKULU
   # -- SUCCESS: These numbers add up; Tableau seems be double counting things
   df_long_dist %>% filter(Province == "BENGKULU") %>% 
     group_by(District) %>% 
     tally(amount)
   
   # Looking at the crosswalk between the shapefile and the Excel data we
   # realized that 66 districts were missing information
  df_long_geo <- df_long_dist %>% 
       left_join(x = ., y = geo_cw, by = c("District" = "KABKOT"))
  glimpse(df_long_geo)
  
  # Creates a shapefile of the data
  df_shapefile <- geo_df %>% 
    right_join(x = ., y = df_long_dist, by = c("KABKOT" = "District"))
  
  #NOTES: The PROVNO, KODE2010 and PROVINSI   
  write_csv(df_long_geo, file.path(datapath, "IND_portfolio_geojoin_2018_07.csv"))
  
  # st_write(df_shapefile, file.path(datapath, "IND_portfolio_geojoin_2018_07.shp"))
  
 
   


    