setwd("C:/Users/ckitchen/Downloads")
setwd("C:/Users/chris/Downloads")
library(tidyverse)
library(stringr)
library(sf)
source("C:/Users/chris/OneDrive/Desktop/GeoHealth/scripts/pullACS/pullACS.R")

#NHTSA FARS safety, pedestrians and impairment info: MAY INCLUDE DRUGS, VISION, DISTRACTED DRIVING, WEATHER EVENTS
for(i in rev(2010:2023)){
  url<-paste0("https://static.nhtsa.gov/nhtsa/downloads/FARS/",i,"/National/FARS",i,"NationalCSV.zip")#example call to FTP
  download.file(url,destfile="temp.zip")
  unzip("temp.zip")
  
  fars1<-read.csv(paste0("./FARS",i,"NationalCSV/accident.csv"),header=T)#scene and location by coordinates
  fars2<-read.csv(paste0("./FARS",i,"NationalCSV/vehicle.csv"),header=T)#vehicle characteristics
  fars3<-read.csv(paste0("./FARS",i,"NationalCSV/person.csv"),header=T)#victim characteristics
  names(fars1)<-str_remove_all(names(fars1),"ï..")
  names(fars2)<-str_remove_all(names(fars2),"ï..")
  names(fars3)<-str_remove_all(names(fars3),"ï..")
  
  fars1<-fars1[,c("STATE","COUNTY","STATENAME","COUNTYNAME","ST_CASE","HARM_EVNAME","FATALS","PEDS","VE_TOTAL","TYP_INT","MONTHNAME","YEAR","LATITUDE","LONGITUD")]
  fars2<-fars2[,c("ST_CASE","HIT_RUN","L_STATUSNAME","L_TYPENAME","CDL_STATNAME","VPICBODYCLASSNAME")]
  fars2$VPICBODYCLASSNAME<-ifelse(str_detect(fars2$VPICBODYCLASSNAME,"Bus"),"Bus",
                                  ifelse(str_detect(fars2$VPICBODYCLASSNAME,"Motorcycle"),"Motorcycle",
                                         ifelse(str_detect(fars2$VPICBODYCLASSNAME,"Off-road"),"Offroad",
                                                ifelse(str_detect(fars2$VPICBODYCLASSNAME,"SUV")|str_detect(fars2$VPICBODYCLASSNAME,"SUT")|str_detect(fars2$VPICBODYCLASSNAME,"CUV"),"SUV",
                                                       ifelse(str_detect(fars2$VPICBODYCLASSNAME,"Tractor"),"TractorTrailor",
                                                              ifelse(str_detect(fars2$VPICBODYCLASSNAME,"Truck")|str_detect(fars2$VPICBODYCLASSNAME,"Pickup")|str_detect(fars2$VPICBODYCLASSNAME,"Wagon"),"TruckPickup",
                                                                     ifelse(str_detect(fars2$VPICBODYCLASSNAME,"Van")|str_detect(fars2$VPICBODYCLASSNAME,"Minivan")|str_detect(fars2$VPICBODYCLASSNAME,"Motorhome"),"Van",
                                                                            ifelse(str_detect(fars2$VPICBODYCLASSNAME,"Sedan")|str_detect(fars2$VPICBODYCLASSNAME,"Roadster")|str_detect(fars2$VPICBODYCLASSNAME,"Convertible")|str_detect(fars2$VPICBODYCLASSNAME,"Coupe"),"PassengerCar","OtherVehicle"
                                                                            ))))))))
  fars2$license<-ifelse(fars2$L_TYPENAME=="Full Driver License"&fars2$L_STATUSNAME=="Valid"&fars2$CDL_STATNAME=="No (CDL)","FullRegular",
                        ifelse(fars2$L_TYPENAME=="Full Driver License"&fars2$L_STATUSNAME=="Valid"&fars2$CDL_STATNAME=="Valid","CDL","OtherSuspended"))
  
  fars2$value<-1
  fars2<-fars2[,c("ST_CASE","HIT_RUN","license","VPICBODYCLASSNAME","value")]
  fars2<-fars2[!duplicated(fars2),]
  ex<-spread(fars2,key=VPICBODYCLASSNAME,value=value,fill=0)
  ex$value<-1
  ex<-spread(ex,key=license,value=value,fill=0)
  fars2<-aggregate(data=ex,cbind(HIT_RUN,Bus,Motorcycle,Offroad,PassengerCar,SUV,TractorTrailor,TruckPickup,Van,CDL,FullRegular,OtherSuspended)~ST_CASE,FUN="sum")
  
  fars3<-fars3[,c("ST_CASE","VEH_NO","PER_NO","AGE","SEXNAME","PER_TYPNAME","INJ_SEVNAME","ALC_RES","ALC_RESNAME","DRUGSNAME")]
  fars3$driver_age<-ifelse(fars3$PER_TYPNAME %in% c("Driver of a Motor Vehicle In-Transport"),fars3$AGE,0)
  fars3$non_motor_peds<-ifelse(fars3$PER_TYPNAME %in% c("Bicyclist","Pedestrian","Person In/On Buildings","Person on Non-Motorized Personal Conveyance","Other Cyclist","Person on Motorized Personal Conveyance"),1,0)
  fars3$non_motor_peds<-ifelse(fars3$INJ_SEVNAME=="Fatal Injury (K)"&fars3$PER_TYPNAME %in% c("Bicyclist","Pedestrian","Person In/On Buildings","Person on Non-Motorized Personal Conveyance","Other Cyclist","Person on Motorized Personal Conveyance"),1,0)
  avg_age_in_vehicle<-aggregate(data=fars3[,c("ST_CASE","VEH_NO","AGE")],AGE~.,FUN="mean")
  names(avg_age_in_vehicle)<-c("ST_CASE","VEH_NO","avg_age_in_vehicle")
  fars3<-merge(fars3,avg_age_in_vehicle,by=c("ST_CASE","VEH_NO"))
  fars3$driver_sex<-ifelse(fars3$PER_TYPNAME=="Driver of a Motor Vehicle In-Transport",fars3$SEXNAME,0)
  fars3$value<-1
  sex<-fars3[,c("ST_CASE","VEH_NO","driver_sex","value")]
  sex<-spread(data=sex[!duplicated(sex),],key=driver_sex,value=value,fill=0)
  fars3<-merge(fars3,sex[,c("ST_CASE","VEH_NO","Female","Male")],by=c("ST_CASE","VEH_NO"),all.x=T)
  fars3$driver_bac<-ifelse(fars3$PER_TYPNAME=="Driver of a Motor Vehicle In-Transport"&fars3$ALC_RES>=996,fars3$ALC_RES,0)
  fars3$driver_fatal_bac<-ifelse(fars3$PER_TYPNAME=="Driver of a Motor Vehicle In-Transport"&fars3$ALC_RES>=996&fars3$INJ_SEVNAME=="Fatal Injury (K)",fars3$ALC_RES,0)
  fars3$driver_drug_detect<-ifelse(fars3$PER_TYPNAME=="Driver of a Motor Vehicle In-Transport"&fars3$DRUGSNAME=="Yes (drugs involved)",1,0)
  fars3$driver_fatal_drug_detect<-ifelse(fars3$PER_TYPNAME=="Driver of a Motor Vehicle In-Transport"&fars3$DRUGSNAME=="Yes (drugs involved)"&fars3$INJ_SEVNAME=="Fatal Injury (K)",1,0)
  fars3<-fars3[,c("ST_CASE","VEH_NO","non_motor_peds","driver_age","avg_age_in_vehicle","driver_bac","driver_fatal_bac","driver_drug_detect","driver_fatal_drug_detect","Female","Male")]
  age<-aggregate(data=fars3[fars3$driver_age!=0,],driver_age~ST_CASE,FUN="mean")
  names(age)<-c("ST_CASE","avg_driver_age")
  fars3<-merge(fars3,age,by="ST_CASE",all.x=T)
  fars3<-aggregate(data=fars3,cbind(avg_driver_age,non_motor_peds,driver_bac,driver_fatal_bac,driver_drug_detect,driver_fatal_drug_detect)~ST_CASE,FUN="max")
  
  fars<-merge(fars1,fars2,by="ST_CASE",all.x=T)
  fars<-merge(fars,fars3,by="ST_CASE",all.x=T)
  fars$STATE<-str_pad(as.character(fars$STATE),width=2,side="left",pad="0")
  fars$COUNTY<-str_pad(as.character(fars$COUNTY),width=3,side="left",pad="0")
  
  outs<-NULL
  for(j in unique(fars$ST_CASE)){
    ex<-fars[fars$ST_CASE==j,] %>%
      st_as_sf(coords = c("LATITUDE","LONGITUD"), crs = "NAD83") %>%
      group_by(ST_CASE) %>%
      summarise(geometry = st_combine(geometry)) 
    outs<-rbind(outs,ex)
    print(j)
  }
  outs<-merge(outs,fars,by="ST_CASE",all.x=T)
  saveRDS(outs,paste0("fars_shapes_",i,".rds"))
}


#FTA NTD transit access, expense, ridership and infrastructure data: MAY INCLUDE VEHICLE AGE AND WEAR (MILES) BY AGENCY
for(i in rev(2010:2023)){
  url<-paste0("https://data.transportation.gov/resource/ekg5-frzt.csv?$query=SELECT%0A%20%20%60agency%60%2C%0A%20%20%60city%60%2C%0A%20%20%60state%60%2C%0A%20%20%60ntd_id%60%2C%0A%20%20%60organization_type%60%2C%0A%20%20%60reporter_type%60%2C%0A%20%20%60report_year%60%2C%0A%20%20%60uace_code%60%2C%0A%20%20%60uza_name%60%2C%0A%20%20%60primary_uza_population%60%2C%0A%20%20%60agency_voms%60%2C%0A%20%20%60mode%60%2C%0A%20%20%60mode_name%60%2C%0A%20%20%60type_of_service%60%2C%0A%20%20%60mode_voms%60%2C%0A%20%20%60fare_revenues_per_unlinked%60%2C%0A%20%20%60fare_revenues_per_unlinked_1%60%2C%0A%20%20%60fare_revenues_per_total%60%2C%0A%20%20%60fare_revenues_per_total_1%60%2C%0A%20%20%60cost_per_hour%60%2C%0A%20%20%60cost_per_hour_questionable%60%2C%0A%20%20%60passengers_per_hour%60%2C%0A%20%20%60passengers_per_hour_1%60%2C%0A%20%20%60cost_per_passenger%60%2C%0A%20%20%60cost_per_passenger_1%60%2C%0A%20%20%60cost_per_passenger_mile%60%2C%0A%20%20%60cost_per_passenger_mile_1%60%2C%0A%20%20%60fare_revenues_earned%60%2C%0A%20%20%60fare_revenues_earned_1%60%2C%0A%20%20%60total_operating_expenses%60%2C%0A%20%20%60total_operating_expenses_1%60%2C%0A%20%20%60unlinked_passenger_trips%60%2C%0A%20%20%60unlinked_passenger_trips_1%60%2C%0A%20%20%60vehicle_revenue_hours%60%2C%0A%20%20%60vehicle_revenue_hours_1%60%2C%0A%20%20%60passenger_miles%60%2C%0A%20%20%60passenger_miles_questionable%60%2C%0A%20%20%60vehicle_revenue_miles%60%2C%0A%20%20%60vehicle_revenue_miles_1%60%0AWHERE%20caseless_one_of(%60report_year%60%2C%20%22",i,"%22)%20limit%205000")
  download.file(url,destfile="temp.csv")#MUST LOOP THROUGH THE NDT ASSET NAMES WHICH VARY BY YEAR
  ntd1<-read.csv("./temp.csv",header=T)#multiple transit systems to uace. needs mapping to fips codes and zcta5
  url<-paste0("https://data.transportation.gov/resource/wwdp-t4re.csv?$query=SELECT%0A%20%20%60agency%60%2C%0A%20%20%60_5_digit_ntd_id%60%2C%0A%20%20%60reporter_type%60%2C%0A%20%20%60organization_type%60%2C%0A%20%20%60city%60%2C%0A%20%20%60state%60%2C%0A%20%20%60report_year%60%2C%0A%20%20%60agency_voms%60%2C%0A%20%20%60mode%60%2C%0A%20%20%60mode_name%60%2C%0A%20%20%60type_of_service%60%2C%0A%20%20%60mode_voms%60%2C%0A%20%20%60mode_voms_questionable%60%2C%0A%20%20%60primary_uza_code%60%2C%0A%20%20%60primary_uza_name%60%2C%0A%20%20%60primary_uza_area_sq_miles%60%2C%0A%20%20%60primary_uza_population%60%2C%0A%20%20%60service_area_sq_miles%60%2C%0A%20%20%60service_area_population%60%2C%0A%20%20%60time_period%60%2C%0A%20%20%60time_service_begins%60%2C%0A%20%20%60time_service_ends%60%2C%0A%20%20%60actual_vehicles_passenger_car_miles%60%2C%0A%20%20%60vehicle_miles_questionable%60%2C%0A%20%20%60actual_vehicles_passenger_car_revenue_miles%60%2C%0A%20%20%60vehicle_revenue_miles_questionable%60%2C%0A%20%20%60actual_vehicles_passenger_deadhead_miles%60%2C%0A%20%20%60deadhead_miles_questionable%60%2C%0A%20%20%60scheduled_vehicles_passenger_car_revenue_miles%60%2C%0A%20%20%60scheduled_revenue_miles_questionable%60%2C%0A%20%20%60actual_vehicles_passenger_car_hours%60%2C%0A%20%20%60vehicle_hours_questionable%60%2C%0A%20%20%60actual_vehicles_passenger_car_revenue_hours%60%2C%0A%20%20%60vehicle_revenue_hours_questionable%60%2C%0A%20%20%60actual_vehicles_passenger_car_deadhead_hours%60%2C%0A%20%20%60deadhead_hours_questionable%60%2C%0A%20%20%60charter_service_hours%60%2C%0A%20%20%60school_bus_hours%60%2C%0A%20%20%60trains_in_operation%60%2C%0A%20%20%60trains_in_operation_questionable%60%2C%0A%20%20%60train_miles%60%2C%0A%20%20%60train_miles_questionable%60%2C%0A%20%20%60train_revenue_miles%60%2C%0A%20%20%60train_revenue_miles_questionable%60%2C%0A%20%20%60train_deadhead_miles%60%2C%0A%20%20%60train_hours%60%2C%0A%20%20%60train_hours_questionable%60%2C%0A%20%20%60train_revenue_hours%60%2C%0A%20%20%60train_revenue_hours_questionable%60%2C%0A%20%20%60train_deadhead_hours%60%2C%0A%20%20%60unlinked_passenger_trips_upt%60%2C%0A%20%20%60unlinked_passenger_trips_questionable%60%2C%0A%20%20%60ada_upt%60%2C%0A%20%20%60sponsored_service_upt%60%2C%0A%20%20%60passenger_miles%60%2C%0A%20%20%60passenger_miles_questionable%60%2C%0A%20%20%60directional_route_miles%60%2C%0A%20%20%60directional_route_miles_questionable%60%2C%0A%20%20%60brt_non_statutory_mixed_traffic%60%2C%0A%20%20%60mixed_traffic_right_of_way%60%2C%0A%20%20%60days_of_service_operated%60%2C%0A%20%20%60days_not_operated_strikes%60%2C%0A%20%20%60days_not_operated_emergencies%60%2C%0A%20%20%60average_speed%60%2C%0A%20%20%60average_speed_questionable%60%2C%0A%20%20%60average_passenger_trip_length_aptl_%60%2C%0A%20%20%60aptl_questionable%60%2C%0A%20%20%60passengers_per_hour%60%2C%0A%20%20%60passengers_per_hour_questionable%60%0AWHERE%20caseless_one_of(%60report_year%60%2C%20%22",i,"%22)%20limit%2015000")
  download.file(url,destfil="temp.csv")
  ntd2<-read.csv("./temp.csv",header=T)#additional details and metrics for services available by NTD_ID and UACE
  url<-paste0("https://data.transportation.gov/resource/wfz2-eft6.csv?$query=SELECT%0A%20%20%60agency%60%2C%0A%20%20%60city%60%2C%0A%20%20%60state%60%2C%0A%20%20%60ntd_id%60%2C%0A%20%20%60organization_type%60%2C%0A%20%20%60reporter_type%60%2C%0A%20%20%60report_year%60%2C%0A%20%20%60uace_code%60%2C%0A%20%20%60uza_name%60%2C%0A%20%20%60primary_uza_population%60%2C%0A%20%20%60agency_voms%60%2C%0A%20%20%60modes%60%2C%0A%20%20%60mode_names%60%2C%0A%20%20%60facility_type%60%2C%0A%20%20%60pre1940%60%2C%0A%20%20%60_1940s%60%2C%0A%20%20%60_1950s%60%2C%0A%20%20%60_1960s%60%2C%0A%20%20%60_1970s%60%2C%0A%20%20%60_1980s%60%2C%0A%20%20%60_1990s%60%2C%0A%20%20%60_2000s%60%2C%0A%20%20%60_2010s%60%2C%0A%20%20%60_2020s%60%2C%0A%20%20%60total_facilities%60%0AWHERE%20caseless_one_of(%60report_year%60%2C%20%22",i,"%22)%20limit%2010000")
  download.file(url,destfil="temp.csv")
  ntd3<-read.csv("./temp.csv",header=T)#additional details and metrics for services available by NTD_ID and UACE
  
  ntd1<-ntd1[,c("agency","city","state","ntd_id","report_year","uace_code","uza_name",
                "primary_uza_population","mode","mode_name","type_of_service",
                "fare_revenues_per_total","fare_revenues_earned",
                "cost_per_passenger","cost_per_hour","cost_per_passenger_mile",
                #"passengers_per_hour","passenger_miles",
                "total_operating_expenses")]
  ntd1$ntd_id<-str_pad(ntd1$ntd_id,width=5,side="left",pad="0")
  ntd2<-ntd2[,c("agency","X_5_digit_ntd_id","city","state","report_year","mode","mode_name",
                "primary_uza_area_sq_miles","service_area_sq_miles","service_area_population",
                "actual_vehicles_passenger_car_miles","train_miles","passenger_miles",
                "passengers_per_hour","actual_vehicles_passenger_car_hours","train_hours")]
  names(ntd2)<-ifelse(str_detect(names(ntd2),"ntd_id"),"ntd_id",names(ntd2))
  ntd2$ntd_id<-str_pad(ntd2$ntd_id,width=5,side="left",pad="0")
  ntd3<-ntd3[,c("agency","ntd_id","city","state","report_year","modes","mode_names",
                "facility_type","total_facilities","pre1940","X_1940s","X_1950s","X_1960s","X_1970s","X_1980s","X_1990s","X_2000s","X_2010s","X_2020s")]
  names(ntd3)<-ifelse(str_detect(names(ntd3),"modes"),"mode",ifelse(str_detect(names(ntd3),"names"),"mode_name",names(ntd3)))
  ntd3$ntd_id<-str_pad(ntd3$ntd_id,width=5,side="left",pad="0")
  
  ntd<-merge(ntd1,ntd2,by=c("agency","ntd_id","city","state","report_year","mode","mode_name"),all=T)
  ntd<-ntd[!duplicated(ntd),]
  ntd<-merge(ntd,ntd3,by=c("agency","ntd_id","city","state","report_year","mode","mode_name"),all=T)
  ntd<-ntd[!duplicated(ntd),]
  ntd$uace_code<-str_pad(ntd$uace_code,width=5,side="left",pad="0")
  uace<-tigris::urban_areas(year=2022)#only 2022 should be used
  ntd<-merge(uace[,c("UACE10","GEOID10","ALAND10","geometry")],ntd,by.x="UACE10",by.y="uace_code",all.x=T)
  saveRDS(ntd,paste0("shape_ntd_",i,".rds"))
}

#Google's MobilityData and GTFS statistics for NTD linked routes
#url<-"https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-11/2023%20GTFS%20Weblinks.xlsx"
#read.table("https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-11/2023%20GTFS%20Weblinks.xlsx")
#download.file(url,destfile="temp.xlsx",mode="wb")#MAY NEED TO MANUALLY DOWNLOAD EACH YEAR!!! THIS IS NOT WORKING!
#httr::GET(url,write("./temp.xlsx"))
#curl::curl_download(url,destfile="./temp.xlsx")

gtfs<-readxl::read_xlsx("2023 GTFS Weblinks.xlsx")#from DOT directly in NTD
library(plyr)
outs<-list()
for(i in unique(gtfs[!is.na(gtfs$Weblink),]$Weblink)){
  if(tryCatch(
    expr={
      download.file(i,destfile="temp.zip")
      },
    error=function(e){
      return("unable to download")
      }
    )!="unable to download"){
    download.file(i,destfile="temp.zip")
    unzip("temp.zip")
    input<-NULL
    for(j in c("trips","fares","stops","routes","shapes","calendar")){
      input<-ifelse(j=="trips"&file.size("trips.txt")!=0,"trips.txt",
                    ifelse(j=="fares"&file.size("fare_attributes.txt")!=0,"fare_attributes.txt",
                           ifelse(j=="stops"&file.size("stops.txt")!=0,"stops.txt",
                                  ifelse(j=="routes"&file.size("routes.txt")!=0,"routes.txt",
                                         ifelse(j=="shapes"&file.size("shapes.txt")!=0,"shapes.txt",
                                                ifelse(j=="calendar"&file.size("calendar.txt")!=0,"calendar.txt","agency.txt"
                                         ))))))
      if(file.size(input)!=0&input!="agency.txt"&nrow(read.csv(input,header=T))>0){
        outs[j][[1]]<-rbind.fill(outs[j][[1]],cbind(master_url=i,read.csv(input,header=T)))
      }
    }
  }
  print(i)
}
outs$gtfs<-gtfs
saveRDS(outs,"gtfs_selected_2023.rds")
gtfs<-readRDS("gtfs_selected_2023.rds")
for(i in names(gtfs)){
  d<-gtfs[i][[1]]
  if(i=="trips"){
    d<-d[,c("master_url","route_id","service_id","trip_id","ï..trip_id","shape_id","fare_id","trip_headsign","peak_flag","peak_offpeak","wheelchair_accessible")]
    d$trip_id<-ifelse(is.na(d$trip_id),d$ï..trip_id,d$trip_id)
    d$peak_flag<-ifelse(is.na(d$peak_flag),d$peak_offpeak,d$peak_flag)
    d[is.na(d)]<-0
    gtfs[i][[1]]<-d[,c("master_url","route_id","service_id","trip_id","shape_id","fare_id","trip_headsign","peak_flag","wheelchair_accessible")]
  }
  if(i=="fares"){
    d<-d[,c("master_url","fare_id","ï..fare_id","agency_id","price","currency_type","payment_method")]
    d$fare_id<-ifelse(is.na(d$fare_id),d$ï..fare_id,d$fare_id)
    d[is.na(d)]<-0
    gtfs[i][[1]]<-d[,c("master_url","fare_id","agency_id","price","currency_type","payment_method")]
  }
  if(i=="stops"){
    d<-d[,c("master_url","stop_id","ï..stop_id","stop_name","stop_lat","stop_lon","wheelchair_boarding")]
    d$stop_id<-ifelse(is.na(d$stop_id),d$ï..stop_id,d$stop_id)
    d[is.na(d)]<-0
    gtfs[i][[1]]<-d[,c("master_url","stop_id","stop_name","stop_lat","stop_lon","wheelchair_boarding")]
  }
  if(i=="routes"){
    d<-d[,c("master_url","route_id","ï..route_id","agency_id","route_desc","route_color","route_type","alt_route_type","route_long_name")]
    d$route_id<-ifelse(is.na(d$route_id),d$ï..route_id,d$route_id)
    d[is.na(d)]<-0
    gtfs[i][[1]]<-d[,c("master_url","route_id","agency_id","route_desc","route_color","route_type","alt_route_type","route_long_name")]
  }
  if(i=="calendar"){
    d<-d[,c("master_url","service_id","ï..service_id","start_date","end_date","monday","tuesday","wednesday","thursday","friday","saturday","sunday")]
    d$service_id<-ifelse(is.na(d$service_id),d$ï..service_id,d$service_id)
    d[is.na(d)]<-0
    gtfs[i][[1]]<-d[,c("master_url","service_id","start_date","end_date","monday","tuesday","wednesday","thursday","friday","saturday","sunday")]
  }
  if(i=="shapes"){
    d<-d[,c("master_url","shape_id","ï..shape_id","shape_pt_lat","shape_pt_lon","shape_pt_sequence","shape_dist_traveled")]
    d$shape_id<-ifelse(is.na(d$shape_id),d$ï..shape_id,d$shape_id)
    d[is.na(d)]<-0
    gtfs[i][[1]]<-d[,c("master_url","shape_id","shape_pt_lat","shape_pt_lon","shape_pt_sequence","shape_dist_traveled")]
  }
  if(i=="gtfs"){
    trip<-aggregate(data=gtfs$trips[,c("master_url","shape_id","route_id","service_id","fare_id","peak_flag","wheelchair_accessible")],cbind(peak_flag,wheelchair_accessible)~.,FUN="max")#agg trip info by route and shape
    trip<-merge(trip,gtfs$routes[,c("master_url","route_id","route_desc","route_type","alt_route_type")],by=c("master_url","route_id"),all.x=T)
    trip<-merge(trip,gtfs$calendar[,c("master_url","service_id","start_date","end_date","monday","tuesday","wednesday","thursday","friday","saturday","sunday")],by=c("master_url","service_id"),all.x=T)
    
    fare<-aggregate(data=gtfs$fares[,c("master_url","fare_id","agency_id","price","currency_type")],price~.,FUN="mean")
    fare<-merge(fare,aggregate(data=gtfs$fares[,c("master_url","fare_id","agency_id","price","currency_type")],price~.,FUN="sd"),by=c("master_url","fare_id","agency_id","currency_type"),all.x=T)
    names(fare)<-c("master_url","fare_id","agency_id","currency_type","mean_price","std_price")
    trip<-merge(trip,fare,by=c("master_url","fare_id"),all.x=T)
    
    shape<-aggregate(data=gtfs$shapes[,c("master_url","shape_id","shape_dist_traveled")],shape_dist_traveled~.,FUN="max")
    names(shape)<-c("master_url","shape_id","max_dist_traveled")
    trip<-merge(trip,shape,by=c("master_url","shape_id"),all.x=T)
    
    trip<-trip[!duplicated(trip),]
    trip$days_per_week<-trip$monday+trip$tuesday+trip$wednesday+trip$thursday+trip$friday+trip$saturday+trip$sunday
    trip$full_weekdays<-ifelse(trip$monday+trip$tuesday+trip$wednesday+trip$thursday+trip$friday==5,1,0)
    trip$full_weekends<-ifelse(trip$saturday+trip$sunday==2,1,0)

    #now aggregate to shape id level price, day of week, route types, peak flag and wheelchair access
    shape<-trip[,c("master_url","shape_id","peak_flag","wheelchair_accessible","route_type","alt_route_type","mean_price","std_price","currency_type","days_per_week","full_weekdays","full_weekends","max_dist_traveled")]
    shape<-shape[!duplicated(shape),]
    shape<-aggregate(data=shape,cbind(peak_flag,wheelchair_accessible,days_per_week,full_weekdays,full_weekends,max_dist_traveled)~master_url+shape_id,FUN="max")
    outs<-list(trip=trip,map=shape,stop=gtfs$stops,shape=gtfs$shapes)
  }
}
gtfs<-outs
remove(shape,shape1,trip,fare,outs,d)
gc()
shape<-gtfs$shape
#ex<-shape[shape$shape_id=="10002005",]
library(tidyverse)
library(sf)
outs<-NULL
for(i in unique(shape$shape_id)){
  ex<-shape[shape$shape_id==i,] %>%
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = "NAD83") %>%
    group_by(shape_id) %>%
    summarise(geometry = st_combine(geometry)) 
  outs<-rbind(outs,ex)
  print(i)
}
saveRDS(outs,"gtfs_shapes_2023.rds")
stop<-gtfs$stop
outs<-NULL
for(i in unique(stop$stop_id)){
  ex<-shape[stop$stop_id==i,] %>%
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = "NAD83") %>%
    group_by(stop_id) %>%
    summarise(geometry = st_combine(geometry)) 
  outs<-rbind(outs,ex)
  print(i)
}
saveRDS(outs,"gtfs_shapes_2023.rds")
outs<-readRDS("gtfs_shapes_2023.rds")

plot(outs[1,"geometry"])
counties<-tigris::counties()
#locate stops within geos
counties[st_intersection(st_as_sfc(counties),outs[1,"geometry"]),]

#map files to existing geographies----
years<-rev(2010:2023)
geos<-c("county","zcta","tract")
outs<-NULL
for(y in years){
  for(g in geos){
    acs<-pullACS(geography=g,geometry=T,year=y)
    acs<-acs[,c("GEOID","geometry")]
    ntd<-readRDS(paste0("shape_ntd_",y,".rds"))
    ntd<-ntd[!is.na(ntd$agency),]
    ntd<-ntd[,c("UACE10","ntd_id","geometry")]
    uace<-tigris::urban_areas()
    uace$state<-substr(uace$NAME10,nchar(uace$NAME10)-1,nchar(uace$NAME10))
    uace<-merge(uace,fips_codes[!duplicated(fips_codes[,c("state","state_code")]),],by.x="state",by.y="state",all.x=T)
    uace<-as.data.frame(uace[,c("UACE10","state","state_code")])
    ntd<-merge(ntd,uace[,c("UACE10","state","state_code")],by.x="UACE10",by.y="UACE10",all.x=T)
    remove(uace)
    fars<-readRDS(paste0("fars_shapes_",y,".rds"))
    fars<-fars[!is.na(fars$HARM_EVNAME),]
    fars<-fars[,c("STATE","COUNTY","ST_CASE","geometry")]
    gtfs<-readRDS(paste0("gtfs_shapes_2023.rds"))
    nout<-NULL
    fout<-NULL
    nouts<-NULL
    fouts<-NULL
    state<-unique(substr(acs$GEOID,1,2))
    for(s in state){
      print(s)
      d<-acs[substr(acs$GEOID,1,2)==s,]
      n<-ntd[ntd$state_code==s,c("UACE10","ntd_id","state_code","geometry")]
      f<-fars[fars$STATE==s,]
      for(i in 1:nrow(d)){
        print(i)
        for(j in 1:nrow(ntd)){
          if(nrow(st_intersection(d[i,],n[j,]))>0){
            nout<-cbind(n,ACSGEOID=d[i,]$GEOID)
          }else{
            nout<-NULL
          }
          nouts<-rbind(nouts,nout)
        }
        for(j in 1:nrow(f)){
          if(nrow(st_intersection(d[i,],f[j,]))>0){
            fout<-cbind(f,ACSGEOID=d[i,]$GEOID)
          }else{
            fout<-NULL
          }
          fouts<-rbind(fouts,fout)
        }
      }
      gc()
    }
  }
}

#map----
outs<-NULL#IF THIS WORKS DO IT NATIONALLY AND USING BGs FOR ALL CAs, NESTED BY STATE, IF INTERSECTS-TRACT AND WITHIN BG (store bgs within tracts that intersect ca, by state)
map<-read_csv(paste0("mapping_file_uace_bg_fips_2022.csv"))
map<-map[nchar(map$geoid)==12,!names(map) %in% c("geometry")]
outs<-map
for(s in state.abb){
  co<-tigris::counties(state=s,year="2022")
  tr<-tigris::tracts(state=s,year="2022")
  bg<-tigris::block_groups(state=s,year="2022")#this year may change
  ca<-tigris::urban_areas(year="2022")#year on ca cannot change
  ca<-ca[str_detect(ca$NAME10,s),]
  for(a in row.names(ca[!ca$UACE10 %in% unique(map$uace),])){
    for(o in 1:nrow(co)){
      if(length(st_intersection(co[o,],ca[a,])[1][[1]])>0){
        print(paste0("found ",co[o,]$NAME," on ",ca[a,]$NAME10))
        tr1<-tr[str_detect(tr$GEOID,co[o,]$GEOID),]
        for(t in 1:nrow(tr1)){
          if(length(st_intersection(tr1[t,],ca[a,])[1][[1]])>0){
            print(paste0("found ",tr1[t,]$NAME," on ",ca[a,]$NAME10))
            bg1<-bg[str_detect(bg$GEOID,tr1[t,]$GEOID),]
            for(b in 1:nrow(bg1)){
              if(length(st_within(bg1[b,],ca[a,])[1][[1]])>0){
                out<-data.frame(geoid=bg1[b,]$GEOID,
                                name=bg1[b,]$NAME,
                                lat=bg1[b,]$INTPTLAT,
                                lon=bg1[b,]$INTPTLON,
                                parse="within",
                                uace=ca[a,]$UACE10,
                                uaNAME=ca[a,]$NAME10,
                                uaNAMELSAD10=ca[a,]$NAMELSAD10#,bg1[b,]$geometry
                                )
                write.table(out,"mapping_file_uace_bg_fips_2022.csv",sep=",",col.names=T,row.names=F,append=T)
                outs<-rbind(outs,out)
              }
            }
          }
        }
      }
    }
  }
}
map<-outs#maps geoid to uace
write.table(map[!duplicated(map),],"mapping_file_uace_bg_fips_2022.csv",sep=",",col.names=T,row.names=F)
map<-read_csv(paste0("mapping_file_uace_bg_fips_2022.csv"))
