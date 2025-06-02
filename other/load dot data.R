#setwd("C:/Users/ckitchen/Downloads")
setwd("C:/Users/chris/Downloads")
library(tidyverse)
library(stringr)
library(sf)
library(readxl)
source("C:/Users/chris/OneDrive/Desktop/GeoHealth/scripts/pullACS/pullACS.R")

#NHTSA FARS safety, pedestrians and impairment info: MAY INCLUDE DRUGS, VISION, DISTRACTED DRIVING, WEATHER EVENTS
for(i in rev(2012:2023)){
  url<-paste0("https://static.nhtsa.gov/nhtsa/downloads/FARS/",i,"/National/FARS",i,"NationalCSV.zip")#example call to FTP
  download.file(url,destfile="temp.zip")
  unzip("temp.zip")
  if(i>=2020){
    fars1<-read.csv(paste0("./FARS",i,"NationalCSV/accident.csv"),header=T)#scene and location by coordinates
    fars2<-read.csv(paste0("./FARS",i,"NationalCSV/vehicle.csv"),header=T)#vehicle characteristics
    fars3<-read.csv(paste0("./FARS",i,"NationalCSV/person.csv"),header=T)#victim characteristics
    names(fars1)<-str_remove_all(names(fars1),"ï..")
    names(fars2)<-str_remove_all(names(fars2),"ï..")
    names(fars3)<-str_remove_all(names(fars3),"ï..")
  }else{
    fars1<-read.csv(paste0("./accident.csv"),header=T)#scene and location by coordinates
    fars2<-read.csv(paste0("./vehicle.csv"),header=T)#vehicle characteristics
    fars3<-read.csv(paste0("./person.csv"),header=T)#victim characteristics
  }
  
  fars1<-fars1[,c("STATE","COUNTY","STATENAME","COUNTYNAME","ST_CASE","HARM_EVNAME","FATALS","PEDS","VE_TOTAL","TYP_INT","MONTHNAME","YEAR","LATITUDE","LONGITUD")]
  fars2<-fars2[,c("ST_CASE","HIT_RUN","L_STATUSNAME","L_TYPENAME","CDL_STATNAME",ifelse(i>=2020,"VPICBODYCLASSNAME","BODY_TYPNAME"))]
  names(fars2)<-c("ST_CASE","HIT_RUN","L_STATUSNAME","L_TYPENAME","CDL_STATNAME","VPICBODYCLASSNAME")
  fars2$VPICBODYCLASSNAME<-ifelse(str_detect(fars2$VPICBODYCLASSNAME,"Bus"),"Bus",
                                  ifelse(str_detect(fars2$VPICBODYCLASSNAME,"Motorcycle"),"Motorcycle",
                                         ifelse(str_detect(fars2$VPICBODYCLASSNAME,"Off-road"),"Offroad",
                                                ifelse(str_detect(fars2$VPICBODYCLASSNAME,"SUV")|str_detect(fars2$VPICBODYCLASSNAME,"SUT")|str_detect(fars2$VPICBODYCLASSNAME,"CUV"),"SUV",
                                                       ifelse(str_detect(fars2$VPICBODYCLASSNAME,"Tractor")|str_detect(fars2$VPICBODYCLASSNAME,"heavy truck")|str_detect(fars2$VPICBODYCLASSNAME,"single-unit")|str_detect(fars2$VPICBODYCLASSNAME,"tractor"),"TractorTrailor",
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
  fars2<-aggregate(data=ex,cbind(HIT_RUN,Bus,Motorcycle,PassengerCar,TractorTrailor,TruckPickup,Van,CDL,FullRegular,OtherSuspended)~ST_CASE,FUN="sum")
  
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
  fars3$driver_bac<-ifelse(fars3$PER_TYPNAME=="Driver of a Motor Vehicle In-Transport"&fars3$ALC_RES<995,fars3$ALC_RES,0)
  fars3$driver_fatal_bac<-ifelse(fars3$PER_TYPNAME=="Driver of a Motor Vehicle In-Transport"&fars3$ALC_RES<995&fars3$INJ_SEVNAME=="Fatal Injury (K)",fars3$ALC_RES,0)
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
    ex<-fars[fars$ST_CASE==j,]
    if(ex$LATITUDE[1]>(-180)&
       ex$LATITUDE[1]<(180)&
       ex$LONGITUD[1]>(-180)&
       ex$LONGITUD[1]<(180)
    ){
      ex<-censusxy::cxy_geography(lat=as.numeric(as.character(ex$LATITUDE[1])),lon=as.numeric(as.character(ex$LONGITUD[1])),
                                  benchmark="Public_AR_Current")
      if(!is.null(ex)){
        ex<-data.frame(GEOID=ex$X2020.Census.Blocks.GEOID,ST_CASE=j)
        outs<-plyr::rbind.fill(outs,ex)
      }
    }
    print(j)
  }
  outs<-merge(outs,fars,by="ST_CASE",all.x=T)
  write.table(outs,paste0("fars_shapes_",i,".csv"),sep=",",col.names=T,row.names=F)
  
}

#FTA NTD transit access, expense, ridership and infrastructure data: MAY INCLUDE VEHICLE AGE AND WEAR (MILES) BY AGENCY
for(i in rev(2012:2023)){
  m<-read.csv("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/mapping_file_uace_bg_fips_2022.csv",header=T)
  ntd1<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/raw/refs/heads/main/",i," Metrics.csv"),header=T)
  ntd2<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/raw/refs/heads/main/",i," Service.csv"),header=T)
  ntd3<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/raw/refs/heads/main/",i," Facilities and Stations.csv"),header=T)

  if(i>=2022){
    #start process here with split at 2022
    ntd1<-ntd1[,c("agency","city","state","ntd_id","report_year","uace_code",
                  "mode","mode_name","type_of_service","fare_revenues_earned","cost_per_hour","passengers_per_hour","passenger_miles","total_operating_expenses")]
    ntd1$ntd_id<-str_pad(ntd1$ntd_id,width=5,side="left",pad="0")
    ntd2<-ntd2[ntd2$time_period=="Annual Total",c("agency","X_5_digit_ntd_id","city","state","report_year",
                                                  "mode","mode_name","agency_voms","mode_voms","service_area_sq_miles","service_area_population","train_miles","train_hours")]
    names(ntd2)<-ifelse(str_detect(names(ntd2),"ntd_id"),"ntd_id",names(ntd2))
    ntd2$ntd_id<-str_pad(ntd2$ntd_id,width=5,side="left",pad="0")
    ntd2<-ntd2[!duplicated(ntd2),]
    ntd3<-ntd3[,c("agency","ntd_id","city","state","report_year","modes","mode_names",
                  "facility_type","total_facilities","pre1940","X_1940s","X_1950s","X_1960s","X_1970s","X_1980s","X_1990s","X_2000s","X_2010s","X_2020s")]
    names(ntd3)<-ifelse(str_detect(names(ntd3),"modes"),"mode",ifelse(str_detect(names(ntd3),"names"),"mode_name",names(ntd3)))
    ntd3$ntd_id<-str_pad(ntd3$ntd_id,width=5,side="left",pad="0")
  }else{
    uas<-read.csv(paste0("https://raw.githubusercontent.com/grimnr14/raw/refs/heads/main/",2022," Metrics.csv"),header=T)[,c("ntd_id","uace_code")]
    uas<-uas[!duplicated(uas),]

    summary(as.factor(str_detect(ntd1$NTD.ID,"-")))
    ntd1$NTD.ID<-ifelse(str_detect(ntd1$NTD.ID,"-")&!is.na(ntd1$NTD.ID),
                        substr(ntd1$NTD.ID,str_locate(ntd1$NTD.ID,"-")[[1]]+1,nchar(ntd1$NTD.ID)),
                        ntd1$NTD.ID)
    names(ntd1)<-tolower(names(ntd1))
    names(ntd1)<-str_replace_all(names(ntd1),"[.]","_")
    names(ntd1)<-str_replace_all(names(ntd1),"__","_")
    names(ntd1)<-str_replace_all(names(ntd1),"__","_")
    ntd1$type_of_service<-ntd1$tos
    ntd1$mode_name<-ifelse(ntd1$mode %in% c("CB","MB","TB","RB","DB"),"Bus",
                           ifelse(ntd1$mode %in% c("DR","VP"),"Demand Response",
                                  ifelse(ntd1$mode %in% c("CR","LR","MG","TR","SR","HR"),"Commuter Rail",
                                         ifelse(ntd1$mode %in% c("FB"),"Ferryboat","Other"
                           ))))
    ntd1<-merge(ntd1,uas,by="ntd_id",all.x=T)
    ntd1<-ntd1[!duplicated(ntd1),]
    #"report_year",
    ntd1<-ntd1[,c("agency","city","state","ntd_id","uace_code",
                  "type_of_service","mode","mode_name","fare_revenues_earned","cost_per_hour","passengers_per_hour","passenger_miles","total_operating_expenses")]
    names(ntd2)<-tolower(names(ntd2))
    names(ntd2)<-str_replace_all(names(ntd2),"[.]","_")
    names(ntd2)<-str_replace_all(names(ntd2),"__","_")
    names(ntd2)<-str_replace_all(names(ntd2),"__","_")
    ntd2$mode_name<-ifelse(ntd2$mode %in% c("CB","MB","TB","RB","DB"),"Bus",
                           ifelse(ntd2$mode %in% c("DR","VP"),"Demand Response",
                                  ifelse(ntd2$mode %in% c("CR","LR","MG","TR","SR","HR"),"Commuter Rail",
                                         ifelse(ntd2$mode %in% c("FB"),"Ferryboat","Other"
                                         ))))
    #"report_year","service_area_sq_miles","service_area_population",
    ntd2<-ntd2[,c("agency","ntd_id","city","state",
                  "mode","mode_name","agency_voms","mode_voms","train_miles","train_hours")]
    names(ntd3)<-tolower(names(ntd3))
    names(ntd3)<-str_replace_all(names(ntd3),"[.]","_")
    names(ntd3)<-str_replace_all(names(ntd3),"__","_")
    names(ntd3)<-str_replace_all(names(ntd3),"__","_")
    ntd3$mode_name<-ifelse(ntd3$mode %in% c("CB","MB","TB","RB","DB"),"Bus",
                           ifelse(ntd3$mode %in% c("DR","VP"),"Demand Response",
                                  ifelse(ntd3$mode %in% c("CR","LR","MG","TR","SR","HR"),"Commuter Rail",
                                         ifelse(ntd3$mode %in% c("FB"),"Ferryboat","Other"
                                         ))))
    #"report_year"
    ntd3<-ntd3[,c("agency","ntd_id","city","state","modes","mode_name",
                  "facility_type","total_facilities","pre1940","x1940_s","x1950_s","x1960_s","x1970_s","x1980_s","x1990_s","x2000_s","x2010_s")]
    names(ntd3)<-c("agency","ntd_id","city","state","mode","mode_name","facility_type","total_facilities","pre1940","X_1940s","X_1950s","X_1960s","X_1970s","X_1980s","X_1990s","X_2000s","X_2010s")
  }
  
  ntd<-merge(ntd1,ntd2,by=c("agency","ntd_id","city","state","mode","mode_name"),all=T)
  ntd<-ntd[!duplicated(ntd),]
  ntd<-merge(ntd,ntd3,by=c("agency","ntd_id","city","state","mode","mode_name"),all=T)
  ntd<-ntd[!duplicated(ntd),]
  ntd$uace_code<-str_pad(ntd$uace_code,width=5,side="left",pad="0")
  miss<-ntd[is.na(ntd$uace_code)&!is.na(ntd$city),]
  outs<-NULL
  for(j in 1:nrow(miss)){
    city<-miss[j,"city"]
    state<-miss[j,"state"]
    city<-ifelse(city=="","@@",city)
    city<-city[!is.na(city)]
    for(k in unique(na.omit(m$uaNAME))){
      if(str_detect(k,city)&str_detect(k,state)){
        print(j)
        out<-data.frame(uace=m[m$uaNAME==k,]$uace,
                        city=miss[j,"city"],
                        state=miss[j,"state"],
                        uaNAME=m[m$uaNAME==k,]$uaNAME,
                        state=substr(m[m$uaNAME==k,]$geoid,1,2))
        outs<-rbind(outs,out)
        outs<-outs[!is.na(outs$uace),]
        outs<-outs[!duplicated(outs),]
      }
    }
  }#recovered ~ 1080/4747 uace
  miss<-merge(miss,outs,by=c("city","state"),all.x=T)
  miss<-miss[!is.na(miss$uace),]
  miss<-miss[!duplicated(miss),]
  miss$uace_code<-miss$uace
  miss<-miss[,c(1:(ncol(miss)-3))]
  ntd<-rbind(ntd,miss)
  
  ntd<-ntd[!is.na(ntd$uace_code),]
  facilities<-ntd[!is.na(ntd$total_facilities),names(ntd) %in% c("ntd_id","total_facilities","pre1940","X_1940s","X_1950s","X_1960s","X_1970s","X_1980s","X_1990s","X_2000s","X_2010s","X_2020s")]

  facilities[is.na(facilities)]<-0
  facilities<-facilities%>%
    dplyr::group_by(ntd_id)%>%
    summarise_each(funs=c("sum"))
  facilities$per_facilities_prior2000<-100*rowSums(facilities[,3:9])/facilities$total_facilities
  facilities$per_facilities_prior1980<-100*rowSums(facilities[,3:7])/facilities$total_facilities
  
  ntd<-ntd[,!names(ntd) %in% c("agency","city","total_facilities","pre1940","X_1940s","X_1950s","X_1960s","X_1970s","X_1980s","X_1990s","X_2000s","X_2010s","X_2020s")]
  ntd<-ntd[!duplicated(ntd),]
  ntd<-merge(ntd,facilities[,c("ntd_id","total_facilities","per_facilities_prior1980","per_facilities_prior2000")],by="ntd_id",all.x=T)
  
  ntd$bin_bus<-ifelse(str_detect(ntd$mode_name,"Bus")|
                        str_detect(ntd$mode_name,"Trollybus"),1,0)
  ntd$bin_demandresponse<-ifelse(str_detect(ntd$mode_name,"Demand Response")|
                                   str_detect(ntd$mode_name,"Vanpool"),1,0)
  ntd$bin_light_commuterail<-ifelse(str_detect(ntd$mode_name,"Commuter Rail")|
                                      str_detect(ntd$mode_name,"Light Rail")|
                                      str_detect(ntd$mode_name,"Monorail")|
                                      str_detect(ntd$mode_name,"Streetcar Rail")|
                                      str_detect(ntd$mode_name,"Hybrid Rail")|
                                      str_detect(ntd$mode_name,"Tramway"),1,0)
  ntd$bin_ferry<-ifelse(str_detect(ntd$mode_name,"Ferryboat"),1,0)
  ntd$bin_directly_operated<-ifelse(ntd$type_of_service=="DO"&!is.na(ntd$type_of_service),1,0)
  ntd$bin_purchased_transportation<-ifelse(ntd$type_of_service=="PT"&!is.na(ntd$type_of_service),1,0)
  
  ntd<-ntd[,!names(ntd) %in% c("mode","mode_name","type_of_service","facility_type")]
  ntd<-ntd[!duplicated(ntd),]
  finalsum<-ntd[,c("ntd_id","uace_code",
                   "bin_bus","bin_demandresponse","bin_light_commuterail","bin_ferry","bin_directly_operated","bin_purchased_transportation",
                   "fare_revenues_earned","total_operating_expenses","cost_per_hour","train_miles","train_hours","passengers_per_hour","passenger_miles")]
  finalsum<-finalsum[!duplicated(finalsum),]
  finalsum[is.na(finalsum)]<-0
  finalsum<-finalsum[,!names(finalsum) %in% c("ntd_id")]%>%
    dplyr::group_by(uace_code)%>%
    summarise_each(funs=c(sum))
  ntd<-merge(ntd[,!names(ntd) %in% c("bin_bus","bin_demandresponse","bin_light_commuterail","bin_ferry",
                                     "bin_directly_operated","bin_purchased_transportation",
                                     "fare_revenues_earned","total_operating_expenses","cost_per_hour",
                                     "train_miles","train_hours","passengers_per_hour","passenger_miles")],
             finalsum,by="uace_code",all.x=T)
  ntd<-ntd[!duplicated(ntd)&!is.na(ntd$uace_code),]
  
  uace<-tigris::urban_areas(year=2022)#only 2022 should be used
  ntd<-merge(as.data.frame(uace[,c("UACE10","GEOID10","ALAND10","NAME10")]),ntd,by.x="UACE10",by.y="uace_code",all.x=T)
  ntd<-ntd[,!names(ntd) %in% c("geometry")]
#  ntd<-merge(ntd,m[,c("geoid","uace","name")],by.x="UACE10",by.y="uace",all.x=T)
  ntd<-ntd[!is.na(ntd$ntd_id),]
  ntd[is.na(ntd)]<-0
  ntd<-ntd[,!names(ntd) %in% c("ntd_id")]
  ntd<-ntd%>%
    group_by(UACE10,GEOID10,NAME10,state)%>%
    summarize_each(funs=c("max"))
  
  write.table(as.data.frame(ntd),paste0("shape_ntd_",i,".csv"),sep=",",col.names=T,row.names=F)
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
    map<-read.csv("https://raw.githubusercontent.com/grimnr14/geohealthdb/refs/heads/main/mapping_file_uace_bg_fips_2022.csv",header=T)
    uace<-merge(uace,map[,c("uace","geoid")],by.x="GEOID10",by.y="uace",all.x=T)
    uace$state_code<-substr(uace$geoid,1,2)
#    uace$state<-substr(uace$NAME10,nchar(uace$NAME10)-1,nchar(uace$NAME10))
#    uace<-merge(uace,fips_codes[!duplicated(fips_codes[,c("state","state_code")]),],by.x="state",by.y="state",all.x=T)
#    uace<-as.data.frame(uace[,c("UACE10","state","state_code")])
    ntd<-merge(ntd,uace[,c("UACE10","state_code")],by.x="UACE10",by.y="UACE10",all.x=T)
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
