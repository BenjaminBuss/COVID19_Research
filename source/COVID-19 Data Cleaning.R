
# Benjamin Buss
# June 13th 2020
# COVID-19 Research Data Cleaning


# Load Packages ----------------------------------------------------------

library(readr)
library(tidyverse)
library(readxl)


# Load Data ---------------------------------------------------------------

census_data_url <- "https://raw.githubusercontent.com/BenjaminBuss/COVID19_Research/master/data/census_data.csv"
census_data <- read_csv(census_data_url)

current_weather_url <- "https://raw.githubusercontent.com/BenjaminBuss/COVID19_Research/master/data/current_weather.csv"
current_weather <- read_csv(current_weather_url)

dly_tavg_normal_url <- "https://raw.githubusercontent.com/BenjaminBuss/COVID19_Research/master/data/dly-tavg-normal.txt"
dly_tavg_normal <- read_table2(dly_tavg_normal_url, col_names = c("station", "month", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", 
                                                                  "d13", "d14", "d15", "d16", "d17", "d18", "d19", "d20", "d21", "d22", "d23", "d24", "d25", 
                                                                  "d26", "d27", "d28", "d29", "d30", "d31"))

geographic_area_url <- "https://raw.githubusercontent.com/BenjaminBuss/COVID19_Research/master/data/geographic_area.csv"
geographic_area <- read_csv(geographic_area_url)

zip_county_fips_url <- "https://raw.githubusercontent.com/BenjaminBuss/COVID19_Research/master/data/zip_county_fips.csv"
zip_county_fips <- read_csv(zip_county_fips_url)

zipcode_normals_url <- "https://raw.githubusercontent.com/BenjaminBuss/COVID19_Research/master/data/zipcode_normals.txt"
zipcode_normals <- read_table2(zipcode_normals_url, col_names = c("station", "zipcode", "name"),
                               col_types = cols(zipcode = col_integer()))

rm(census_data_url, current_weather_url, dly_tavg_normal_url, geographic_area_url, zip_county_fips_url, zipcode_normals_url)


# Clean Data --------------------------------------------------------------

census_data <- census_data %>% filter(SUMLEV != 50) %>% 
  select(STATE, COUNTY, REGION, DIVISION, CENSUS2010POP, "population" = POPESTIMATE2018) %>% 
  mutate(STATE = str_pad(STATE,  width = 2, side = "left", pad = "0"), 
         COUNTY = str_pad(COUNTY, width = 3, side = "left", pad = "0")) %>% 
  unite(STATE, COUNTY, col = "fips", sep = "")

historical_weather <- dly_tavg_normal %>% gather(3:33, key = day, value = temp) %>%
  filter(month %in% c('03', '04', '05'), !(temp %in% c('-9999', '-8888', '-7777', '-6666', '-5555'))) %>%
  mutate(day = sub(".", "", day), day = as.numeric(day), temp = str_sub(temp, end = -2),
         temp = as.numeric(temp) / 10)

zipcode_station <- inner_join(zip_county_fips, zipcode_normals, by = c("ZIP" = "zipcode")) %>% 
  select(station, zip = ZIP, fips = STCOUNTYFP, county = COUNTYNAME, state = STATE, name)


# Export Data -------------------------------------------------------------

write.csv(census_data, "census_data.csv")
write.csv(current_weather, "current_weather.csv")
write.csv(historical_weather, "historical_weather.csv")
write.csv(geographic_area, "geographic_area.csv")
write.csv(zipcode_station, "zipcode_station.csv")

