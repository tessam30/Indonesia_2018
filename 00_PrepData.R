
# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl")

dir.create("Data")
datapath <- "Data"
ind_invest_data <- "All_FY15_18 copy.xlsx"

# Import the Excel data from 
  df <- read_excel(file.path(datapath, ind_invest_data), sheet = "USAID Indonesia Investment Mapp")
  glimpse(df)
  
  # Some of the dates are inconsistent and/or missing so the dates is coerced to a double instead of a date
  

    