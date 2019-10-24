## ---------------------------
## Script name: Dynamics of Collective Action Data
##
## Purpose of script: Import and clean DOCA data
## 
## Date Created: 2019-10-23
##
## Date Modified: 
## 
## Created by: rpm
##
## ---------------------------
## Notes:
##   Documentation & codebook in [doca_data] folder
##
## ---------------------------

library(tidyverse)

doca <- haven::read_dta(here::here("doca_data", "final_data_v10.dta"), encoding = "latin1")

doca <-
  doca %>% 
  mutate(
    where = ifelse(str_detect(where, "HARLME, NY"), "HARLEM, NY", where)
  )

racist <-
  doca %>% 
  filter(
    # Racist action
    (claim1 == 2504 & val1 == 1) | 
      (claim2 == 2504 & val2 == 1) | 
      (claim3 == 2504 & val3 == 1) | 
      (claim4 == 2504 & val4 == 1) |
    # Anti-civil rights action
    (str_detect(claim1, "^15") & val1 == 2) | 
      (str_detect(claim2, "^15") & val2 == 2) | 
      (str_detect(claim3, "^15") & val3 == 2) | 
      (str_detect(claim4, "^15") & val4 == 2)
    ) %>% 
  filter(city1 != "")

aa_civil_rights <-
  doca %>% 
  filter(
    # Anti-racist action
    (claim1 == 2504 & val1 == 2) | 
      (claim2 == 2504 & val2 == 2) | 
      (claim3 == 2504 & val3 == 2) | 
      (claim4 == 2504 & val4 == 2) |
    # Pro-civil rights action
    (str_detect(claim1, "^15") & val1 == 1) | 
      (str_detect(claim2, "^15") & val2 == 1) | 
      (str_detect(claim3, "^15") & val3 == 1) | 
      (str_detect(claim4, "^15") & val4 == 1)
    ) %>% 
  filter(city1 != "")

r_lat_lon <- geocode(paste(racist$city1, racist$state1))
c_lat_lon <- geocode(paste(aa_civil_rights$city1, aa_civil_rights$state1))

get_county_name <- function(lat, lon) {
  
  url <- paste0(
    "https://geo.fcc.gov/api/census/area?lat=",
    lat,
    "&lon=",
    lon,
    "&format=json"
  )
  
  parsed <- httr::parse_url(url)
  response <- httr::GET(httr::build_url(parsed))
  data_dot_json <- jsonlite::fromJSON(httr::content(response, "text"))
  data_dot_json <- 
    data_dot_json$results %>% 
    select(county_fips, county_name, state_fips, state_code)
  
  return(data_dot_json)
}

r_counties <-
  map2(
    r_lat_lon$lat,
    r_lat_lon$lon,
    ~ get_county_name(.x, .y)
  ) %>% 
  map_dfr(slice, 1)

safe_get <- safely(get_county_name)

c_counties <-
  map2(
    c_lat_lon$lat,
    c_lat_lon$lon,
    ~ safe_get(.x, .y)
  ) %>% 
  map_dfr(slice, 1)
