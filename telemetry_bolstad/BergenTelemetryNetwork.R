#setwd("C:/Users/role/OneDrive - NORCE/BTN/detections")

getwd()
detections <- read.csv("./data/raw_data/detections_bolstadfjord.csv")
str(detections)


library(dplyr)
det<-dplyr::select(detections, Date.and.Time..UTC., ID, Data, Protocol, Receiver) %>% 
  dplyr::rename(dt='Date.and.Time..UTC.') %>% 
  mutate(dt=ymd_hms(dt)) %>% 
  mutate(dt=with_tz(dt, "Europe/Oslo")) %>% 
  dplyr::select(dt, ID, Data, Protocol, Receiver) %>% 
  mutate(Receiver=as.integer(Receiver)) %>% 
  as_tibble


require(tidyverse); require(lubridate); #require(rgdal)
library(tidyverse)
library(lubridate)
#library(rgdal)
# load in shapefiles around Vosso
#setwd("./data/Shapes")
library(sf)
bp<-st_read(".", "Boniteringspolygon")
ost<-gdal_read(".", "StudySite")
bp<-spTransform(bp, proj4string(ost))

# pull receiver metadata

library(gsheet)
rec<-gsheet2tbl('https://docs.google.com/spreadsheets/d/18mUpHQkSBs5PKN2XqoOZEUXXjzUTJwyDx7qvU-0kgqs/edit#gid=1414140311') %>% 
  as_tibble %>% 
  dplyr::filter(!is.na(lon))

meta<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1ht4dk480HDm5a6Eop1eGKOnQNeGXJnKb7FnFBc9dMGA/edit#gid=0') 
str(meta)
meta_trout <- meta[!is.na(meta$vial), ]
summary(as.factor(meta_trout$Spp))
meta_trout <- meta_trout[meta_trout$Spp=='Salmo trutta'|meta_trout$Spp=='Salmo salar', ]
meta_trout <- meta_trout[meta_trout$Year>2019, ]
summary(as.factor(meta_trout$System))
summary(as.factor(meta$ID))
summary(as.factor(meta_trout$`
                  Project`))
saveRDS(meta_trout, "./data/modified_data/fishdata_PACE_NORCE.RDS")
write.csv(meta_trout, "./data/modified_data/PACE_bergen_samples_2021_2022.csv")


meta<-meta %>%
  mutate(ID=as.numeric(ID)) %>% 
  distinct(Vendor, ID, n_ID) %>% 
  mutate(n_ID=n_ID-1) %>% 
  mutate(ID2=case_when(n_ID==0 ~ NA_real_,
                       n_ID>0 ~ ID+1)) %>% 
  mutate(ID3=case_when(n_ID==2 ~ ID+2,
                       T~NA_real_)) %>% 
  mutate(oid=ID) %>% 
  dplyr::select(-n_ID) %>% 
  gather(key, value, -oid, -Vendor) %>% 
  dplyr::rename(ID=oid) %>% 
  right_join(meta %>% mutate(ID=as.numeric(ID)))  %>% 
  mutate(key=case_when(grepl("-AT", Transmitter) &
                         value-ID==0 ~ "accel",
                       grepl("-AT", Transmitter) &
                         value-ID==1 ~ "temp",
                       grepl("-DT", Transmitter) &
                         value-ID==0 ~ "depth",
                       grepl("-DT", Transmitter) &
                         value-ID==1 ~ "temp",
                       grepl("-T", Transmitter) &
                         value-ID==0 ~ "temp",
                       grepl("-P", Transmitter) &
                         value-ID==0 ~ "not eaten",
                       grepl("-P", Transmitter) &
                         value-ID==1 ~ "eaten",
                       grepl("-AT", Transmitter) &
                         value-ID==1 ~ "eaten",
                       grepl("-ADT", Transmitter) &
                         value-ID==0 ~ "accel",
                       grepl("-ADT", Transmitter) &
                         value-ID==1 ~ "depth",
                       grepl("-ADT", Transmitter) &
                         value-ID==2 ~ "temp",
                       grepl("-D", Transmitter) &
                         value-ID==0 ~ "depth")) %>% 
  dplyr::filter(!is.na(key) | Vendor=="Vemco") %>% 
  dplyr::rename(sensor=key, oid=ID, ID=value)

str(meta)
meta$gill_sample <- meta$`Vial ID`
meta$gill_sample
#meta[meta$gill_sample=='BF53']
# get coordinates right for receivers (UTM 33)
# summary(as.factor(meta$Project))
meta <- meta[which(meta$Spp=='Salmo salar'|meta$Spp=='Salmo salar hatchery'), ]
meta <- meta[which(meta$gill_sample!=''), ]
meta <- meta[which(meta$Year=='2021'|meta$Year=='2022'), ]
meta <- meta[which(meta$Project!='COD'), ]
write.csv(meta, "./data/modified_data/PACE_Salmon_samples_2021.csv")

# summary(as.factor(meta$Spp))
# 
# str(meta)


coordinates(rec)<-~lon+lat
proj4string(rec)<-CRS('+init=epsg:4326')
rec<-spTransform(rec, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))
rec<-rec %>% as_tibble

# get daily receiver locations to account for moves and additions

receiver_locations<-seq.Date(as.Date("2012-01-01"), 
                             as.Date(Sys.Date()), by="day") %>% 
  as_tibble %>% 
  expand_grid(., rec) %>% 
  mutate(end=case_when(end=="" |
                         is.na(end)~ "01.01.2023", T~end)) %>% 
  dplyr::filter(value>dmy(start) & value<dmy(end)) %>% 
  distinct(value, Receiver, lon, lat) %>% 
  dplyr::rename(dti=value)

f<-function(x) {
  x %>% 
    left_join(meta %>% 
                dplyr::rename(TL='TL (mm)') %>% 
                mutate(ID=as.integer(ID)) %>% 
                dplyr::select(ID, oid, sensor, Spp, TL, Project, Transmitter), 
              by="ID")}

dets<-receiver_locations %>% 
  right_join(det %>% 
               mutate(dti=date(dt)), by=c("Receiver", "dti")) %>% 
  split(.$ID) %>% 
  purrr::map(~f(.)) %>% 
  bind_rows()

# range test tag is 166

dets<-dets %>% 
  mutate(dth=round_date(dt, "30 min")) %>% # false detections.. time period
  group_by(ID, Receiver, dth) %>% 
  dplyr::summarise(n=n()) %>% 
  right_join(dets %>% 
               mutate(dth=round_date(dt, "30 min")),
             by=c("ID", "dth", "Receiver")) %>% 
  dplyr::filter(n>1) %>%  # only want detections on receivers where there is >1 in an hour
  ungroup()
# plot the fjord

fjo<-ggplot()+
  geom_polygon(data=ost %>% fortify,
               aes(long, lat, group=group), fill="white")+
  geom_polygon(data=bp %>% fortify,
               aes(long, lat, group=group), fill="royalblue")+
  theme_classic()+
  coord_fixed(ratio=.75,
              xlim=c(-95000,35000),
              ylim=c(6735000,6774000))+
  theme(panel.background = element_rect(fill = "darkblue"))

fjo+
  geom_point(data=receiver_locations %>% 
               dplyr::filter(dti==Sys.Date()), # plot active receivers on this date
             aes(lon, lat), size=2, colour="red")

dets %>% 
  arrange(dt) %>% 
  dplyr::filter(oid==4379) %>% 
  dplyr::filter(!is.na(Spp)) %>% 
  dplyr::filter(year(dt)>2019) %>% 
  # dplyr::filter(ID>2000, ID<3000) %>% 
  group_by(ID) %>% 
  #slice_sample(n=1000) %>% 
  arrange(dt) %>% 
  ggplot(aes(dt, lon))+
  scale_colour_manual(values=c("darkblue", "darkblue",
                               "lightblue", "royalblue"))+
  geom_path()+
  facet_wrap(~paste(oid, Spp))+
  theme_classic()+
  geom_rect(aes(xmin=as.POSIXct("2020-01-01 01:01:00 CEST"), 
                xmax=as.POSIXct("2022-01-01 01:01:00 CEST"), 
                ymin=30463.27, ymax=23796.1770),
            fill="yellow", alpha=0.005, inherit.aes=F, colour=NA)+
  geom_rect(aes(xmin=as.POSIXct("2020-01-01 01:01:00 CEST"), 
                xmax=as.POSIXct("2022-01-01 01:01:00 CEST"), 
                ymin=9888, ymax=14494),
            fill="green", alpha=0.005, inherit.aes=F, colour=NA)+
  geom_hline(yintercept=6149.844, lty=2)

fjo+
  geom_point(data=dets %>% 
               dplyr::filter(date(dt)==as.Date("2013-06-20")) %>% 
               distinct(lon, lat), 
             aes(lon, lat), inherit.aes=F, colour="red",
             size=2)

fjo+
  geom_path(data=dets %>% 
              dplyr::filter(ID==148) %>% 
              dplyr::filter(!is.na(Spp)) %>% 
              #dplyr::filter(Project=="Evanger2020") %>% 
              #dplyr::filter(ID<136) %>% 
              group_by(ID) %>% 
              #slice_sample(n=500) %>% 
              arrange(desc(dt)),
            aes(lon, lat), inherit.aes=F, colour="red",
            size=1.4)

fjo+
  geom_label(data=receiver_locations %>% 
               dplyr::filter(dti==as.Date("2020-06-01")),
             aes(lon, lat, label=Receiver), size=2, colour="red")


ev20<-dets %>% 
  dplyr::filter(ID<136 & Protocol=="S256-69kHz")

ev21<-dets %>% 
  dplyr::filter(ID>3000 & ID<4000 &
                  Protocol=="S64K-71kHz")

pace<-dets %>% 
  dplyr::filter(Protocol=="S64K-69kHz" &
                  ID>2500 & ID<3000)

lakes21<-dets %>% 
  dplyr::filter(ID>4000 & ID<5000 &
                  Protocol=="S64K-71kHz") %>% 
  dplyr::select(ID, dt, Receiver, lon, lat, Data, sensor, Spp)

ext <- extent(bp)
r <- raster(bp, res=50)  
r <- rasterize(bp, r, field=1)
values(r)[values(r) ==1] = 1
values(r)[is.na(values(r))] = 0

CP <- as(raster::extent(-10000, 20000, 6750000, 6765000), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(ost))
ost2<-ost
ost2@bbox <- as.matrix(extent(CP))

ext<-extent(ost)
q<-raster(ost, res=50)
q<-rasterize(ost, q, field=1)
values(q)[values(q) ==1] = 0
values(q)[is.na(values(q))] = 1

origin(r)=origin(q)
mosaic(r, q, fun=max) %>% plot

origin(s)=origin(q)

vosso<-mosaic(r, q, fun=max) # stack up all the rasters together
values(vosso)[values(vosso) %>% is.na()]=0 # get rid of NAs in vosso

require(gdistance)

trans <- transition(vosso, mean, directions = 8)

pacef<-pace %>% 
  dplyr::filter(Receiver!=1328 &
                  Receiver!=1173 &
                  Receiver!=1312 &
                  Receiver!=437 &
                  Receiver!=1160 &
                  Receiver!=1316 &
                  Receiver!=1343 &
                  Receiver!=1303 &
                  Receiver!=1326 &
                  Receiver!=1320 &
                  Receiver!=1342 &
                  Receiver!=1322) 

distance_matrix<-function(df){
  dis<-df %>% 
    dplyr::select(ID, dt, lon, lat) %>% 
    arrange(dt) %>% 
    group_by(ID) %>% 
    mutate(llon=dplyr::lag(lon, default=first(lon)), 
           llat=dplyr::lag(lat, default=first(lat))) %>% 
    ungroup() %>% 
    distinct(lon, lat, llon, llat) %>% 
    mutate(i=1:nrow(.)) %>% 
    split(.$i) %>% 
    purrr::map(possibly(~shortestPath(trans,
                                      c(.$llon, .$llat), c(.$lon, .$lat),
                                      output="SpatialLines"), NA)) %>% 
    purrr::map(possibly(~sf::st_as_sf(.), NA)) %>% 
    purrr::map(possibly(~sf::st_length(.), NA)) %>%     
    bind_rows() %>% 
    t()}

a<-pacef %>% 
  distance_matrix(.)

bind_cols(df %>% 
            dplyr::select(ID, dt, lon, lat) %>% 
            arrange(dt) %>% 
            group_by(ID) %>% 
            mutate(llon=dplyr::lag(lon, default=first(lon)), 
                   llat=dplyr::lag(lat, default=first(lat))) %>% 
            ungroup() %>% 
            distinct(lon, lat, llon, llat)) %>% 
  dplyr::rename(distance_metres=V1) 
}

# plot to check distances.. removing NA (if receivers are beyond shape area)
# segments are straight, but distances are calculated around land (least cost)
# 
dis<-pace %>% 
  distance_matrix(.) %>% 
  right_join(pace %>% 
               dplyr::filter(Receiver!=1328 &
                               Receiver!=1173 &
                               Receiver!=1312 &
                               Receiver!=437 &
                               Receiver!=1160 &
                               Receiver!=1316 &
                               Receiver!=1343 &
                               Receiver!=1303 &
                               Receiver!=1326 &
                               Receiver!=1320 &
                               Receiver!=1342 &
                               Receiver!=1322) %>% 
               arrange(dt) %>% 
               group_by(ID) %>% 
               mutate(llon=dplyr::lag(lon, default=first(lon)), 
                      llat=dplyr::lag(lat, default=first(lat))) %>% 
               ungroup(),
             by=c("llon", "lon", "llat", "lat"))

dis %>% 
  dplyr::filter(distance_metres>0) %>% 
  distinct(lon, lat, llon, llat, distance_metres) %>% 
  ggplot(aes(x=llon, xend=lon, y=llat, yend=lat, colour=distance_metres))+
  geom_segment(size=1.3)+
  scale_colour_gradientn(colours=rev(rainbow(7)))

bp %>% 
  fortify %>% 
  ggplot(aes(long, lat, group=group))+
  geom_polygon()+
  geom_segment(data=dis %>% 
                 dplyr::filter(distance_metres<5000) %>% 
                 dplyr::filter(distance_metres>0) %>% 
                 distinct(lon, lat, llon, llat, distance_metres),
               aes(x=llon, xend=lon, y=llat, yend=lat, 
                   colour=distance_metres), inherit.aes=F)+
  scale_colour_gradientn(colours=rev(rainbow(7)))

dis %>% 
  dplyr::filter(distance_metres<500) %>% 
  dplyr::filter(distance_metres>0) %>% 
  distinct(lon, lat, llon, llat, Receiver, distance_metres)

dis %>% 
  #dplyr::filter(distance_metres>0) %>% 
  dplyr::filter(ID==2625) %>% 
  arrange(dt) %>% 
  mutate(cd=cumsum(distance_metres)) %>% 
  ggplot(aes(dt, lon, colour=cd))+
  geom_point()

dis %>% 
  dplyr::filter(distance_metres>0) %>% 
  slice_sample(n=100) %>% 
  ggplot(aes(Receiver,x=llon, xend=lon, y=llat, yend=lat, colour=distance_metres))+
  geom_segment()

dis %>% 
  dplyr::filter(distance_metres>0) %>% 
  arrange(dt) %>% 
  group_by(ID) %>% 
  mutate(dis=cumsum(distance_metres)) %>% 
  dplyr::select(ID, dt, dis) %>% 
  ggplot(aes(dt, dis, colour=ID %>% factor))+
  geom_step()

bp %>% 
  fortify %>% 
  ggplot(aes(long, lat, group=group))+
  geom_polygon(fill="darkblue")+
  geom_segment(data=dis %>% 
                 dplyr::filter(distance_metres<1000) %>% 
                 distinct(lon,llon, lat, llat, distance_metres) %>% 
                 slice_sample(n=100),
               aes(x=llon, xend=lon, y=llat, yend=lat, 
                   colour=distance_metres),
               size=2,
               inherit.aes=F)+
  scale_colour_gradientn(colours=rev(rainbow(7)))+
  facet_wrap(~llon)

evTrout<-dets %>% 
  dplyr::filter(Protocol=="S256-69kHz",
                ID>135 & ID<166)

evTrout %>% 
  group_by(ID, Receiver, lon, lat, dti=round_date(dt, "5 min")) %>% 
  dplyr::summarise(n=n()) %>% 
  right_join(evTrout) %>% 
  dplyr::filter(n>2) %>% 
  group_by(ID) %>% 
  #slice_sample(n=1000) %>% 
  arrange(dti) %>% 
  ggplot(aes(dti, lon))+
  facet_wrap(~ID)+
  theme_classic()+
  geom_rect(aes(xmin=as.POSIXct("2020-01-01 01:01:00 CEST"), 
                xmax=as.POSIXct("2022-01-01 01:01:00 CEST"), 
                ymin=30463.27, ymax=23796.1770),
            fill="yellow", alpha=0.005, inherit.aes=F, colour=NA)+
  geom_rect(aes(xmin=as.POSIXct("2020-01-01 01:01:00 CEST"), 
                xmax=as.POSIXct("2022-01-01 01:01:00 CEST"), 
                ymin=9888, ymax=14494),
            fill="green", alpha=0.005, inherit.aes=F, colour=NA)+
  geom_hline(yintercept=6149.844, lty=2)+
  geom_path()

fjo+
  geom_point(data=lakes21 %>% 
               dplyr::filter(sensor=="accel") %>% 
               group_by(ID, Receiver, lon, lat) %>% 
               dplyr::summarise(m=mean(Data)),
             aes(lon, lat, fill=m, size=m),
             inherit.aes=F, pch=21)+
  scale_fill_viridis_c(option="plasma")+
  guides(size=F)+
  labs(fill="Acceleration")+
  theme(legend.position="top",
        legend.key.width=unit(3,"cm"))

pace  

#NB! Set working directory to project folder!
setwd("C:/Rdata/Git/PACE_WP2_Bolstadfjord")
write.csv(pace, "./data/modified_data/pace2020_filtered_detections.csv", row.names = FALSE)

str(pace)

str(meta)
summary(as.factor(meta$Sytem))
meta_lakes <- meta[meta$Project=='PACE2020', ]
str(meta_lakes)
write.csv(meta_lakes, "./data/modified_data/pace2020_bolstad_tagging_data.csv", row.names = FALSE)