#file to find things to fix in StationMeta
stations<-read.csv("data/StationMetaData_20190813.csv")
stations$lat<-as.numeric(as.character(stations$lat))
stations$lon<-as.numeric(as.character(stations$lon))
stations$lat<-jitter(stations$lat,amount=.005)
stations$lon<-jitter(stations$lon,amount=.005)

write.csv(stations,file="outputs/jitter_stations.csv")
