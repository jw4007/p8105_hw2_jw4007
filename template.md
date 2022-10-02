hw2 jw4007
================

``` r
library(tidyverse)
library(readxl)
```

### Prblem 1

Below we import and clean data from
`NYC_Transit_Subway_Entrance_And_Exit_Data.csv`. The process begins with
data import, updates variable names, and selects the columns that will
be used in later parts fo this problem. We update `entry` from `yes` /
`no` to a logical variable. As part of data import, we specify that
`Route` columns 8-11 should be character for consistency with 1-7.

``` r
trans_ent = 
  read_csv(
    "./NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
    col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c", Route11 = "c")) %>%
  janitor::clean_names() %>%
  select(
    line, station_name, station_latitude, station_longitude, 
    starts_with("route"), entry, exit_only, vending, entrance_type, 
    ada) %>% 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))

trans_ent
## # A tibble: 1,868 × 20
##    line     station_…¹ stati…² stati…³ route1 route2 route3 route4 route5 route6
##    <chr>    <chr>        <dbl>   <dbl> <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
##  1 4 Avenue 25th St       40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
##  2 4 Avenue 25th St       40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
##  3 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
##  4 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
##  5 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
##  6 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
##  7 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
##  8 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
##  9 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
## 10 4 Avenue 53rd St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
## # … with 1,858 more rows, 10 more variables: route7 <chr>, route8 <chr>,
## #   route9 <chr>, route10 <chr>, route11 <chr>, entry <lgl>, exit_only <chr>,
## #   vending <chr>, entrance_type <chr>, ada <lgl>, and abbreviated variable
## #   names ¹​station_name, ²​station_latitude, ³​station_longitude
```

As it stands, these data are not “tidy”: route number should be a
variable, as should route. That is, to obtain a tidy dataset we would
need to convert `route` variables from wide to long format. This will be
useful when focusing on specific routes, but may not be necessary when
considering questions that focus on station-level variables.

The following code chunk selects station name and line, and then uses
`distinct()` to obtain all unique combinations. As a result, the number
of rows in this dataset is the number of unique stations.

``` r
trans_ent %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 465 × 2
##    station_name             line    
##    <chr>                    <chr>   
##  1 25th St                  4 Avenue
##  2 36th St                  4 Avenue
##  3 45th St                  4 Avenue
##  4 53rd St                  4 Avenue
##  5 59th St                  4 Avenue
##  6 77th St                  4 Avenue
##  7 86th St                  4 Avenue
##  8 95th St                  4 Avenue
##  9 9th St                   4 Avenue
## 10 Atlantic Av-Barclays Ctr 4 Avenue
## # … with 455 more rows
```

The next code chunk is similar, but filters according to ADA compliance
as an initial step. This produces a dataframe in which the number of
rows is the number of ADA compliant stations.

``` r
trans_ent %>% 
  filter(ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 84 × 2
##    station_name                   line           
##    <chr>                          <chr>          
##  1 Atlantic Av-Barclays Ctr       4 Avenue       
##  2 DeKalb Av                      4 Avenue       
##  3 Pacific St                     4 Avenue       
##  4 Grand Central                  42nd St Shuttle
##  5 34th St                        6 Avenue       
##  6 47-50th Sts Rockefeller Center 6 Avenue       
##  7 Church Av                      6 Avenue       
##  8 21st St                        63rd Street    
##  9 Lexington Av                   63rd Street    
## 10 Roosevelt Island               63rd Street    
## # … with 74 more rows
```

To compute the proportion of station entrances / exits without vending
allow entrance, we first exclude station entrances that do not allow
vending. Then, we focus on the `entry` variable – this logical, so
taking the mean will produce the desired proportion (recall that R will
coerce logical to numeric in cases like this).

``` r
trans_ent %>% 
  filter(vending == "NO") %>% pull(entry) %>% 
  mean
## [1] 0.3770492
```

Lastly, we write a code chunk to identify stations that serve the A
train, and to assess how many of these are ADA compliant. As a first
step, we tidy the data as alluded to previously; that is, we convert
`route` from wide to long format. After this step, we can use tools from
previous parts of the question (filtering to focus on the A train, and
on ADA compliance; selecting and using `distinct` to obtain dataframes
with the required stations in rows).

``` r
trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A") %>%  
  select(station_name, line) %>% 
  distinct
## # A tibble: 60 × 2
##    station_name                  line           
##    <chr>                         <chr>          
##  1 Times Square                  42nd St Shuttle
##  2 125th St                      8 Avenue       
##  3 145th St                      8 Avenue       
##  4 14th St                       8 Avenue       
##  5 168th St - Washington Heights 8 Avenue       
##  6 175th St                      8 Avenue       
##  7 181st St                      8 Avenue       
##  8 190th St                      8 Avenue       
##  9 34th St                       8 Avenue       
## 10 42nd St                       8 Avenue       
## # … with 50 more rows
  
trans_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
## # A tibble: 17 × 2
##    station_name                  line            
##    <chr>                         <chr>           
##  1 14th St                       8 Avenue        
##  2 168th St - Washington Heights 8 Avenue        
##  3 175th St                      8 Avenue        
##  4 34th St                       8 Avenue        
##  5 42nd St                       8 Avenue        
##  6 59th St                       8 Avenue        
##  7 Inwood - 207th St             8 Avenue        
##  8 West 4th St                   8 Avenue        
##  9 World Trade Center            8 Avenue        
## 10 Times Square-42nd St          Broadway        
## 11 59th St-Columbus Circle       Broadway-7th Ave
## 12 Times Square                  Broadway-7th Ave
## 13 8th Av                        Canarsie        
## 14 Franklin Av                   Franklin        
## 15 Euclid Av                     Fulton          
## 16 Franklin Av                   Fulton          
## 17 Howard Beach                  Rockaway

trans_ent
## # A tibble: 1,868 × 20
##    line     station_…¹ stati…² stati…³ route1 route2 route3 route4 route5 route6
##    <chr>    <chr>        <dbl>   <dbl> <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
##  1 4 Avenue 25th St       40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
##  2 4 Avenue 25th St       40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
##  3 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
##  4 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
##  5 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
##  6 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
##  7 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
##  8 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
##  9 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
## 10 4 Avenue 53rd St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
## # … with 1,858 more rows, 10 more variables: route7 <chr>, route8 <chr>,
## #   route9 <chr>, route10 <chr>, route11 <chr>, entry <lgl>, exit_only <chr>,
## #   vending <chr>, entrance_type <chr>, ada <lgl>, and abbreviated variable
## #   names ¹​station_name, ²​station_latitude, ³​station_longitude
```

### Problem 2

#### 1) Read and clean the Mr. Trash Wheel sheet:

``` r
mr_trash = read_excel("./Trash-Wheel-Collection-Totals-7-2020-2.xlsx", sheet = "Mr. Trash Wheel", range = "A2:N533") %>%
  janitor::clean_names() %>%
  filter(dumpster != "NA") %>%
  mutate(sports_balls = as.integer(sports_balls), who = "mr")
```

#### 2) Read and clean the Professor Trash Wheel sheet:

``` r
prof_trash = read_excel("./Trash-Wheel-Collection-Totals-7-2020-2.xlsx", sheet = "Professor Trash Wheel", range = "A2:N115") %>%
  janitor::clean_names() %>%
  drop_na(dumpster) %>%
   mutate(sports_balls = as.integer(sports_balls), who = "prof")
```

#### 3) Combine the two to produce a single tidy dataset

``` r
mr_prof = full_join(mr_trash, prof_trash)
```

The final dataset has 524 observations with the following variables:
dumpster, month, year, date, weight_tons, volume_cubic_yards,
plastic_bottles, polystyrene, cigarette_butts, glass_bottles,
grocery_bags, chip_bags, sports_balls, homes_powered, who. The total
weight of trash collected by Professor Trash Wheel is 135.5. The total
number of sports balls collected by Mr. Trash Wheel in 2020 is 5313.

### Problem 3

#### 1) clean the pols-month:

``` r
pols_month = 
  read_csv(
    "./q3data/pols-month.csv") %>%
  janitor::clean_names() %>%
  separate(mon, into = c("year", "month", "day"), sep = "-") %>%
  mutate(month = as.numeric(month), month = month.abb[month], month = str_to_lower(month)) %>% 
  mutate(president = prez_gop + prez_dem) %>%
  mutate(year = as.numeric(year)) %>%
  select(-prez_gop, -prez_dem, -day) %>%
  arrange(year, month)
dim(pols_month)
## [1] 822   9
```

The pols_month dataset has 822 rows and 9 columns with the following
variables: year, month, gov_gop, sen_gop, rep_gop, gov_dem, sen_dem,
rep_dem, president. The years ranges from 1947 to 2015

#### 2) clean the snp:

``` r
snp = read_csv(
    "./q3data/snp.csv") %>%
  janitor::clean_names() %>%
  separate(date, into = c("month", "day", "year"), sep = "/") %>%
  mutate(month = as.numeric(month), month = month.abb[month], month = str_to_lower(month)) %>%
  mutate(year = as.numeric(year), year = 2000 + year) %>%
  select(-day) %>%
  arrange(year, month)
  
```

The snp dataset has 787 rows and 3 columns with the following variables:
month, year, close. The years ranges from 2000 to 2099

#### 3) tidy the unemployment:

``` r
unemploy = read_csv(
    "./q3data/unemployment.csv") %>%
  janitor::clean_names() %>%
  pivot_longer(jan:dec, names_to = "month", values_to = "percent_unemploy")
  
```

The unemploy dataset has 816 rows and 3 columns with the following
variables: year, month, percent_unemploy. The years ranges from 1948 to
2015

#### 4) joining the datatsets:

``` r
pols_snp = full_join(pols_month, snp, by = c("year" = "year", "month" = "month"))

unemploy_pols_snp = full_join(pols_snp, unemploy, by = c("year" = "year", "month" = "month"))
```

The final dataset combining unemploy, snp, and pols_month has 1428 rows
and 11 columns with the following variables: year, month, gov_gop,
sen_gop, rep_gop, gov_dem, sen_dem, rep_dem, president, close,
percent_unemploy (dimension:). The years ranges from 1947 to 2099
