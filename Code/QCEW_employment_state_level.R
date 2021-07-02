### ----------------------------------
###                                                                          
### Extraction for annual US state level employment data from the Quarterly 
### Census of Employment and Wages (QCEW) by industry (NAICS)                                                             
###
### Author: Mirjam Nanko
### Email: m.nanko@exeter.ac.uk
### Date: July 1, 2021
###
### ----------------------------------

library(readxl)     # excel files
library(tidyverse)  # data handling
library(magrittr)   # advanced piping
library(stringr)    # text data handling
library(psych)      # describe data

#-------------------------------------------------------------------------------
# This script loads and aggregates the Quarterly Census of Employment and Wages 
# (QCEW) annual US state level employment data by NAICS industry.
# Data Source: CSVs By Industry - Annual Averages: 
# https://www.bls.gov/cew/downloadable-data-files.htm)
# 
# Specifically, the script
# (A) loads the state level annual average employment numbers for a specified 
#     range of years (in this example: 2003 to 2010) and industries classified
#     by NAICS industry codes (e.g. 10 Total, all industries; 21 Mining, 
#     quarrying, and oil and gas extraction; 211 Oil and gas extraction) and
#     NAICS codes: 
#     https://www.bls.gov/cew/classifications/industry/industry-titles.htm
# (B) aggregates and pivots the data to wide format.
# 
# NOTE: To use this script, it is only neccessary to specify the correct 
#       information or to verify the output at the six steps that are marked
#       with "### >> Step X". All the remaining code should run automatically. 
# 
#-------------------------------------------------------------------------------


### >> Step 1: Specify the path to data parent directory ###
# 
# Set to the folder containing the data folders used in this script. The folder 
# should contain a folder called QCEW that contains all the extracted zip files
# "CSVs By Industry - Annual Averages" for each required year (e.g. 
# 2009.annual.by_industry, 2010.annual.by_industry). These can be downloaded 
# here: https://www.bls.gov/cew/downloadable-data-files.htm
# 
setwd("~/Data/")
### <<

#-------------------------------------------------------------------------------
# (A) Load Quarterly Census of Employment and Wages (QCEW) annual US state 
#     level employment
#-------------------------------------------------------------------------------

### >> Step 2: Specify path to QCEW data folder ###
# 
# Set to directory where the individual Annual Averages by Industry folders 
# are saved (absolute filepath or relative to working directory)
# 
directory = "QCEW/"
### <<

### >> Step 3: Define relevant years ###
# 
years <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)
### <<

### >> Step 4: Define relevant industry NAICS codes ###
# 
# "NAICS is a 2- through 6-digit hierarchical classification system, 
# offering five levels of detail. Each digit in the code is part of a series 
# of progressively narrower categories, and the more digits in the code 
# signify greater classification detail. The first two digits designate the 
# economic sector, the third digit designates the subsector, the fourth 
# digit designates the industry group, the fifth digit designates the NAICS 
# industry, and the sixth digit designates the national industry."
# (Source: https://www.census.gov/naics/)
# 
# See list of "QCEW Industry Codes and Titles (For NAICS Coded Data)":
# https://www.bls.gov/cew/classifications/industry/industry-titles.htm
# 
# NOTE: There have been changes to some of the NAICS industry codes, i.e. NAICS 
# code with 4 or more digits. This means multiple codes may be required to load
# the same industry. Make sure to provide ALL required codes for the years 
# specified. Step 4 will provide an extra check to make sure that data has been 
# loaded for all years. A hierarchy crosswalk file to check the codes for all 
# years can be downloaded as a csv file here:
# https://www.bls.gov/cew/classifications/industry/2017-naics-hierarchy-crosswalk.htm
# 
NAICS <- 
  c("10",                                        # Total
    "211", "2121", "213111", "213112", "213113", # Fossil fuel mining
    "221112",                                    # Fossil fuel power generation
    "2212",                                      # Natural gas distribution
    "486"                                        # Pipeline transportation
    )
### <<


# Load the state data 
# Create empty data frame
QCEW.states = data.frame()
# Extract folder names for the specified years
folder.names <- dir(directory)[str_extract(dir(directory), "[:digit:]*") 
                               %in% years]
# Specify column types to save the FIPS county code as a character
# (preserves leading zeros) and to select relevant columns
cols <- c("character", "NULL", NA, "NULL", "NULL", NA, "NULL", "NULL",
          rep(NA, 4), "NULL", "NULL", NA, rep("NULL", 28))
# Append the csv data files depending on the selected years and industries
for (folder.name in folder.names) {
  folder.path = paste0(directory, folder.name)
  file.names = dir(folder.path)[str_extract(dir(folder.path), "[:digit:]*")
                                %in% years &
                                str_trim(str_extract(dir(folder.path), 
                                                     "\\s[:digit:]*\\s"))
                                %in% NAICS]
  for (file.name in file.names) {
    file = paste0(folder.path, "/", file.name)
    temp = read.csv(file, colClasses = cols)
    QCEW.states = rbind(QCEW.states, temp)
    }
  rm(temp)
}

# Filter the relevant observations
QCEW.states <- QCEW.states %>% 
  # Subset county level data 
  # See list of "QCEW Aggregation Level Codes":
  # https://www.bls.gov/cew/classifications/aggregation/agg-level-titles.htm
  filter(agglvl_title %in% 
           c("State, Total Covered", 
             "State, by Domain -- by ownership sector",
             "State, by Supersector -- by ownership sector",
             "State, NAICS Sector -- by ownership sector",
             "State, NAICS 3-digit -- by ownership sector",
             "State, NAICS 4-digit -- by ownership sector",
             "State, NAICS 5-digit -- by ownership sector",
             "State, NAICS 6-digit -- by ownership sector")) %>%
  # Remove observations that have "unknown or undefined" county information
  filter(str_detect(area_fips, "999$") != T) %>% 
  mutate(area_title = str_replace(area_title, " -- Statewide", ""))


# Save NAICS industry codes and titles
NAICS.codebook <- QCEW.states[,c("industry_code", "industry_title")] %>% 
  unique() %>% 
  mutate(industry_code_long = str_pad(industry_code, 6, "right", pad = "0")) %>%
  arrange(industry_code_long)


### >> Step 5: Check result and filter correct observations ###
# 
# Check that the required industries have been loaded
NAICS.codebook
# In case the NAICS code has changed for any of the required industries, some 
# years will have zero observations for the affected industry. The following
# table displays the number of loaded observations for each industry and year.
# If there are zero observations for some year, this is a strong indicator that 
# the NAICS code for this industry may have changed at some point. 
QCEW.states %>% 
  select(year, industry_code, industry_title, annual_avg_emplvl) %$% 
  table(year, industry_title)
### <<


#-------------------------------------------------------------------------------
# (B) Aggregate the data by to ignore ownership level and pivot it to wide
#     format
#-------------------------------------------------------------------------------

# Aggregate the state data
QCEW.states <- QCEW.states %>% 
  select(area_fips, area_title, year, industry_code, annual_avg_emplvl) %>%
  group_by(area_fips, area_title, year, industry_code) %>% 
  summarise(annual_avg_emplvl = sum(annual_avg_emplvl)) %>%
  # Pivot the data to wide format
  pivot_wider(names_from = industry_code, values_from = annual_avg_emplvl,
              names_prefix = "emp.", values_fill = 0) %>% 
  arrange(area_fips, area_title, year) %>% 
  mutate(state = substr(area_fips, 1, 2),
         congress = case_when(
         year %in% c(2003, 2004) ~ 108,
         year %in% c(2005, 2006) ~ 109,
         year %in% c(2007, 2008) ~ 110,
         year %in% c(2009, 2010) ~ 111,
         year %in% c(2011, 2012) ~ 112,
         year %in% c(2013, 2014) ~ 113,
         year %in% c(2015, 2016) ~ 114,
         year %in% c(2017, 2018) ~ 115)) %>% 
  relocate(c(area_fips, area_title), .after = last_col()) %>% 
  relocate(c(state), .before = year) %>% 
  relocate(c(congress), .after = year)

# Inspect data
head(QCEW.states, 10)

# Save the Congress districts data in the QCEW data directory
write.csv(QCEW.states,
          paste0(directory, "QCEW_states_employment.csv"))

# Save the NAICS codebook in the QCEW data directory
write.csv(NAICS.codebook, 
          paste0(directory, "QCEW_states_employment_NAICS_codebook.csv"))
