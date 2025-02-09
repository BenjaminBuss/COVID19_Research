
## Introduction

This markdown is a step by step walk through of the data cleaning, processing and preparation done for my COVID-19 Project that is hosted in this github repository: <https://github.com/BenjaminBuss/COVID19_Research>. All analysis was done using R and RStudio. 


## Description of Key Data Sets and Sources


### New York Times Case Counts




### Google Bigquery 2020 Weather Data

Weather data for 2020 was obtained fro the following publicly available Google BigQuery data set <https://console.cloud.google.com/marketplace/details/noaa-public/ghcn-d>. This data set is based on the NOAA's Global Historical Climatology Network. It is publicly available under these terms given by the NOAA <https://www.data.gov/privacy-policy#data_policy>. A dictionary of the data set can be accessed from the NOAA here <https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt>. 

Using the following query, we can pull the daily temperature averages from March 1st on for all stations based in the United States. Since the temperature values are in tenths of degrees Celsius, we convert it to Fahrenheit using the following formula:
$$Temperature = ( value / 10 ) * ( 9 / 5 ) + 32 $$. The results of the query are then exported to a csv for later use.

```
SELECT 
  id AS station, 
  date, 
  (value / 10)*(9/5) + 32 AS temp 
FROM 
  `bigquery-public-data.ghcn_d.ghcnd_2020` 
WHERE 
  element = 'TAVG' 
  AND date > '2020-03-01' 
  AND id LIKE 'US%'
```


### Census Data

Census data was obtained from a publicly available data set that can be found here: <https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/counties/totals/>. For a full dictionary of data provided by census.gov can be found here <https://www2.census.gov/programs-surveys/popest/datasets/2010-2018/counties/totals/co-est2018-alldata.pdf>. The original data set has 80 columns but only a few will be kept. The STATE and COUNTY columns will turn into the fips code, and the column containing the estimated population for 2018(POPESTIMATE2018). The region, division, and 2010 census population columns are being kept around for kicks and giggles.

```
raw_census_data %>% filter(SUMLEV == 50) %>% 
  select(STATE, COUNTY, REGION, DIVISION, CENSUS2010POP, "population" = POPESTIMATE2018) %>% 
  mutate(STATE = str_pad(STATE,  width = 2, side = "left", pad = "0"), 
         COUNTY = str_pad(COUNTY, width = 3, side = "left", pad = "0")) %>% 
  unite(STATE, COUNTY, col = "fips", sep = "")
```


### Historical NOAA Weather Data

Data set provided by NOAA here <ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/documentation/>, it contains 30 year daily normals per station. Data is homogenized to correct for changes in practices and inconsistencies in original data. The original data set contains station ids, month of year, then 31 column of daily observations. Daily observations contained a quality flag in the last position, as well as multiple possible "special characters" to represent missing data.

Since the original data comes in a rather unintuitive format more cleaning was required than most of the other data sets.  It was first manipulated from wide to long, observations from March to May were selected, then any special or missing observations were dropped. The quality flags removed and temperatures were changed from tenths of degrees Fahrenheit to Fahrenheit.

<ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/products/temperature/dly-tavg-normal.txt>

```
historical_weather %>% gather(3:33, key = day, value = temp) %>%
    filter(month %in% c('03', '04', '05'), !(temp %in% c('-9999', '-8888', '-7777', '-6666', '-5555'))) %>%
    mutate(day = sub(".", "", day), day = as.numeric(day), temp = str_sub(temp, end = -2),
         temp = as.numeric(temp) / 10)
```


### Zipcode Normals Stations

A data set from the same NOAA database the historical daily normals came from, it can be accessed here <ftp://ftp.ncdc.noaa.gov/pub/data/normals/1981-2010/station-inventories/>. Contains three columns, station, zip code, and name. It is used to help connect the historical and 2020 year weather data.


### Zip County FIPs

A publicly available data set from data.world that combines USPS, Census and US HUD sources to allow accurate match ups between FIPS codes and zip codes, it can be accessed here <https://data.world/niccolley/us-zipcode-to-county-state>. Used in conjunction with the [Zipcode Normals Stations](#Zipcode Normals Stations) data set to connect weather and population data.

```
inner_join(zip_fips, zipcodes_normals_stations, by = c("ZIP" = "zipcode")) %>% 
  select(station, zip = ZIP, fips = STCOUNTYFP, county = COUNTYNAME, state = STATE, name)
```

This section is the section of this document that is most likely to introduce error or bias. Especially later on, since there are multiple zip codes per a single FIPs code and so if there are multiple weather stations(with different observations) ********************************


### Geographic Area

Data provided by census.gov containing land size(in square miles) on a per county level. The only cleaning done was cutting it down to the needed columns and renaming them. <https://www.census.gov/library/publications/2011/compendia/usa-counties-2011.html#LND>

```
geographic_area %>% select(name = Areaname, fips = STCOU, land_size = LND010190D)
```
