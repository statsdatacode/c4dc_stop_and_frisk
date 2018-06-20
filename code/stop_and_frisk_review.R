options(scipen=999)
library('tidyr')
library('plyr')
library('dplyr')
library('ggplot2')
library('gganimate')
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

block_data <- find_tract(block_data)

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
    race=ifelse(ethnicity == 'Hispanic Or Latino','Hispanic Or Latino',race),
    sex=ifelse(sex == 'Unknown',NA,sex),
    ethnicity=ifelse(ethnicity == 'Unknown',NA,ethnicity),
    age=ifelse(age == 'Unknown',NA,ifelse(age == "Juvenile",17,age)),
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
    block_name=sub(".* BLOCK OF |.* B/O |.* BLOCK OF / |.* B/O / |.* B/O & ","",address)
  )

geospatial_sf_data <- sf_data %>%
  left_join(select(block_data,block_name=ONSTREETDISPLAY,low_range=LOWER_RANGE,high_range=HIGHER_RANGE,quad=ONSTQUAD,ward=WARD,lat=LATITUDE,long=LONGITUDE,CensusTract=tract),by=c("block_name")) %>%
  as.data.frame() %>%
  mutate(
    block_num=ifelse(is.na(block_num),1,block_num),
    block_num=ifelse(block_num == 'UNIT',0,as.numeric(block_num)),
    race=factor(race)
  ) %>%
  filter(is.na(block_num) | (as.integer(block_num) >= as.integer(low_range) & as.integer(block_num) <= as.integer(high_range)))

write_csv(geospatial_sf_data,'./data/combined_sf_data.csv')