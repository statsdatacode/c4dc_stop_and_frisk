options(scipen=999)
library('tidyr')
library('plyr')
library('dplyr')
library('ggplot2')
library('hexbin')
library('readr')
library('anytime')
library('lubridate')
library("maps")
library("maptools")
library("mapdata")
library("mgcv")
library("fiftystater")
library("caret")

sf_names <- c("type","date","time","ampm","year","data_type","race","sex","ethnicity","address","district","psa","age")
field_names <- c("date","time","ampm","year","address","district","psa","type","reason","data_type","race","ethnicity","sex","age")

mround <- function(x,base){ 
  base*round(x/base) 
} 

save_plot <- function(plot, filename) {
  
  ggsave(filename, plot=plot, device="png",path='./plots',width=14.38,height=9.26)
  
}

tract_polygons <- read_csv('./data/dc_census_tracts_polygons.csv')
neighborhood_polygons <- read_csv('./data/dc_neighborhood_polygons.csv')

find_tract <- function(blocks) {
  
  tracts <- unique(tract_polygons$CensusTract)
  locs <- blocks %>%
    select(long=X,lat=Y) %>%
    as.matrix()
  
  blocks$tract <- NA
  
  for (tract in tracts) {
    
    points <- tract_polygons %>%
      filter(CensusTract == tract) %>%
      select(long,lat) %>%
      as.matrix()
    
    blocks$tract <- ifelse(in.out(points,locs) == T, tract, blocks$tract)
    
  }
  
  return(blocks)
}

find_neighborhood <- function(blocks) {
  
  nbh <- unique(neighborhood_polygons$neighborhood)
  locs <- blocks %>%
    select(long=X,lat=Y) %>%
    as.matrix()
  
  blocks$Neighborhood <- NA
  
  for (nbhood in nbh) {
    
    points <- neighborhood_polygons %>%
      filter(neighborhood == nbhood) %>%
      select(long,lat) %>%
      as.matrix()
    
    blocks$Neighborhood <- ifelse(in.out(points,locs) == T, nbhood, blocks$Neighborhood)
    
  }
  
  return(blocks)
}

census_data <- read_csv('./data/dc_2015_census_tract_data.csv')

block_data <- read_csv('./data/Block_Centroids.csv') %>%
  filter(!is.na(STREETSEGID)) 

Xconverter <- train(X~CENTROIDX,data=block_data,method="lm")
Yconverter <- train(Y~CENTROIDY,data=block_data,method="lm")

block_data$clong <- predict(Xconverter,newdata=block_data)
block_data$clat <- predict(Yconverter,newdata=block_data)

block_data$slong <- predict(Xconverter,newdata=data.frame(CENTROIDX=block_data$EXTENTMINX))
block_data$slat <- predict(Yconverter,newdata=data.frame(CENTROIDY=block_data$EXTENTMINY))
block_data$elong <- predict(Xconverter,newdata=data.frame(CENTROIDX=block_data$EXTENTMAXX))
block_data$elat <- predict(Yconverter,newdata=data.frame(CENTROIDY=block_data$EXTENTMAXY))

block_data <- find_tract(block_data) %>%
  find_neighborhood

weather_data <- read_csv('./data/weather_dc_2012_17.csv') %>%
  mutate(
    date=as.Date(paste0(month,'/',day,'/',year),"%m/%d/%Y"),
    fog=ifelse(grepl('Fog',events),1,0),
    rain=ifelse(grepl('Rain',events),1,0),
    snow=ifelse(grepl('Snow',events),1,0),
    tstorm=ifelse(grepl('Thunderstorm',events),1,0)
  )

sf_data <- read_csv('./data/sf_data_10_17.csv',col_names = sf_names, skip=1) %>%
  bind_rows(read_csv('./data/sf_data_18.csv',col_names = sf_names, skip=1)) %>%
  separate(date,c("month","day","dateyear"),sep='/',remove = F) %>%
  separate(time,c("hour","min","sec"),sep=":",remove=F) %>%
  mutate(
    season=as.factor(ifelse(as.integer(month) == 12, 'Winter', ifelse(as.integer(month) <= 2, 'Winter', ifelse(as.integer(month) <= 5, 'Spring', ifelse(as.integer(month) <= 8, 'Summer', 'Fall') ) ) )),
    dateyear=year,
    date=as.Date(paste0(month,'/',day,'/',dateyear), "%m/%d/%Y"),
    year=as.integer(year),
    hour=as.integer(hour),
    min=as.integer(min),
    sec=as.integer(sec),
    race=ifelse(race == 'Unknown',NA,race),
    race=ifelse(!is.na(ethnicity) & ethnicity == 'Hispanic Or Latino','Latinx',race),
    race=ifelse(race == 'American Indian Or Alaska Native','Native',race),
    race=ifelse(race == 'Native Hawaiian Or Other Pacific Islander','Pacific',race),
    race=factor(race),
    sex=ifelse(sex == 'Unknown',NA,sex),
    sex=factor(sex),
    ethnicity=ifelse(ethnicity == 'Unknown',NA,ethnicity),
    ethnicity=factor(ethnicity),
    age=ifelse(age == 'Unknown',NA,ifelse(age == "Juvenile",17,age)),
    age=as.integer(age),
    weekday=wday(date, label = T),
    fmonth=month(date, label=T),
    hour=ifelse(ampm == 'PM', ifelse(hour != 12, hour + 12, hour), ifelse(ampm == 'AM', ifelse(hour == 12,0,hour),hour)),
    time_sec=paste0(hour,':',min,':',sec),
    time_min=paste0(hour,':',min,':00'),
    time_qtr_hour=paste0(hour,':',mround(min,15),':00'),
    time_hlf_hour=paste0(hour,':',mround(min,30),':00'),
    time_hour=paste0(hour,':00:00'),
    time_three_hour=paste0(mround(hour,3),':00:00'),
    time_six_hour=paste0(mround(hour,6),':00:00')
  ) %>%
  select(type,date,time,ampm,month,day,year,hour,min,sec,race:age,weekday,fmonth,season,time_sec,time_min,time_qtr_hour,time_hlf_hour,time_hour,time_three_hour,time_six_hour) %>%
  mutate(
    address=gsub("0F","OF",address),
    address=gsub("0 ST","O ST",address),
    address=gsub("ST ST","ST",address),
    block_num=gsub(" .*$","",address),
    block_name=sub(".* BLOCK OF |.* B/O |.* BLOCK OF / |.* B/O / |.* B/O & ","",address),
    block_name=sub(" ST "," STREET ",block_name),
    block_name=sub(" AVE "," AVENUE ",block_name),
    block_name=sub(" RD "," ROAD ",block_name),
    block_num=ifelse(is.na(block_num),1,block_num),
    block_num=ifelse(grepl('[a-zA-Z]',block_num),NA,block_num),
    id=1:n()
  )

geospatial_sf_data <- sf_data %>%
  mutate(
    block_name=ifelse(is.na(block_num),NA,block_name)
  ) %>%
  left_join(select(block_data,block_name=ONSTREETDISPLAY,low_range=LOWER_RANGE,high_range=HIGHER_RANGE,quad=ONSTQUAD,ward=WARD,lat=LATITUDE,long=LONGITUDE,CensusTract=tract,Neighborhood),by=c("block_name")) %>%
  filter((as.integer(block_num) >= as.integer(low_range) & as.integer(block_num) <= as.integer(high_range))) %>%
  distinct %>%
  group_by(block_num,block_name) %>%
  filter(low_range == block_num | (high_range-low_range) == min(high_range-low_range)) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(
    lat=mean(lat,na.rm=T),
    long=mean(long,na.rm=T),
    CensusTract=min(CensusTract,na.rm=T),
    count=n(),
    remove=0
  ) %>%
  ungroup() %>%
  mutate(
    remove=ifelse(id == 5200 & ward == 'Ward 2',1,remove),
    remove=ifelse(count > 1 & is.na(ward),1,remove),
    remove=ifelse(block_num == 300 & block_name == 'H STREET NW' & ward == 'Ward 2',1,remove),
    remove=ifelse(block_num == 500 & block_name == 'I STREET NW' & ward == 'Ward 2',1,remove),
    remove=ifelse(block_num == 700 & block_name == '8TH STREET NW' & ward == 'Ward 2',1,remove)
  ) %>%
  select(-low_range,-high_range) %>%
  unique() %>%
  filter(remove == 0)

unique(geospatial_sf_data$race)

write_csv(geospatial_sf_data, './data/combined_sf_data.csv')

city_sf_data <- geospatial_sf_data %>%
  summarize(
    total_city=n(),
    prop_men_city=mean(sex == 'Male',na.rm=T),
    prop_women_city=mean(sex == 'Female',na.rm=T),
    prop_juvenile_city=mean(age == 17,na.rm=T),
    median_age_city=median(age,na.rm=T),
    prop_latinx_city=mean(race == 'Latinx',na.rm=T),
    prop_white_city=mean(race == 'White',na.rm=T),
    prop_black_city=mean(race == 'Black',na.rm=T),
    prop_native_city=mean(race == 'Native',na.rm=T),
    prop_asian_city=mean(race == 'Asian',na.rm=T),
    prop_pacific_city=mean(race == 'Pacific',na.rm=T)
  )

city_demo_data <- census_data %>%
  mutate(
    Latinx=round((Hispanic/100)*TotalPop),
    White=round((White/100)*TotalPop),
    Black=round((Black/100)*TotalPop),
    Native=round((Native/100)*TotalPop),
    Asian=round((Asian/100)*TotalPop),
    Pacific=round((Pacific/100)*TotalPop),
    Poverty=round((Poverty/100)*TotalPop),
    Unemployment=round((Unemployment/100)*TotalPop)
  ) %>%
  mutate(
    Unknown=TotalPop-(Latinx+White+Black+Native+Asian+Pacific)
  ) %>%
  select(CensusTract:Women,Latinx,White:Pacific,Unknown,Citizen:Poverty,Employed,Unemployment)

write_csv(city_demo_data, './data/censustract_demo_data.csv')

city_sf_data <- city_sf_data %>%
  bind_cols(city_demo_data) %>%
  mutate(CensusTract=11001000000) %>%
  select(CensusTract,Population,total_city,Men,prop_men_city,Women,prop_women_city,Hispanic,prop_latinx_city,White,prop_white_city,Black,prop_black_city,Native,prop_native_city,Asian,prop_asian_city,Pacific,prop_pacific_city,Citizen,prop_juvenile_city,median_age_city)

tract_sf_data <- geospatial_sf_data %>%
  filter(!is.na(CensusTract)) %>%
  group_by(CensusTract) %>%
  summarize(
    total_tract=n(),
    prop_men_tract=mean(sex == 'Male',na.rm=T),
    prop_women_tract=mean(sex == 'Female',na.rm=T),
    prop_juvenile_tract=mean(age == 17,na.rm=T),
    median_age_tract=median(age,na.rm=T),
    prop_latinx_tract=mean(race == 'Latinx',na.rm=T),
    prop_white_tract=mean(race == 'White',na.rm=T),
    prop_black_tract=mean(race == 'Black',na.rm=T),
    prop_native_tract=mean(race == 'Native',na.rm=T),
    prop_asian_tract=mean(race == 'Asian',na.rm=T),
    prop_pacific_tract=mean(race == 'Pacific',na.rm=T)
  ) %>%
  left_join(select(census_data,-State,-County),by=c("CensusTract")) %>%
  select(CensusTract,TotalPop,total_tract,Men,prop_men_tract,Women,prop_women_tract,Hispanic,prop_latinx_tract,White,prop_white_tract,Black,prop_black_tract,Native,prop_native_tract,Asian,prop_asian_tract,Pacific,prop_pacific_tract,Citizen,prop_juvenile_tract,median_age_tract,Income:Unemployment)



