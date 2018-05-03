




###################################################################################################################
###################################################################################################################
library(sf)

DATA = data.frame("COUNTY" = rep(NA,100))

demo(nc, ask = FALSE, echo = FALSE)
nc = st_transform(nc,2264)
DATA$COUNTY = toupper(nc$NAME)
DATA$X = st_coordinates(st_centroid(nc))[,1]
DATA$Y = st_coordinates(st_centroid(nc))[,2]

###################################################################################################################
########### Read 2000 Data
results = read.csv("results_2000.csv",header = FALSE)
results = results[grepl("US HOUSE OF REP. DISTRICT",results[,5]),]

Y = rep(NA,100)
for(K in 1:100){
results.county = results[which(results[,1] == DATA$COUNTY[K]),]
votes.total = sum(results.county[,8])
votes.repub = sum(results.county[which(results.county[,7]=="REP"),8])
votes.dem = sum(results.county[which(results.county[,7]=="DEM"),8])
Y[K] = (votes.repub - votes.dem) / votes.total}
DATA$Y_1 = Y

###################################################################################################################
########### Read 2002 Data
results = read.csv("results_2002.csv",header = FALSE)
results = results[grepl("US HOUSE",results[,5]),]

Y = rep(NA,100)
for(K in 1:100){
results.county = results[which(results[,1] == DATA$COUNTY[K]),]
votes.total = sum(results.county[,8])
votes.repub = sum(results.county[which(results.county[,7]=="REP"),8])
votes.dem = sum(results.county[which(results.county[,7]=="DEM"),8])
Y[K] = (votes.repub - votes.dem) / votes.total}
DATA$Y_2 = Y

###################################################################################################################
########### Read 2004 Data
results = read.csv("results_2004.csv",header = FALSE)
results = results[grepl("US CONGRESS",results[,5]),] ; results[,8] = as.numeric(results[,8])

Y = rep(NA,100)
for(K in 1:100){
results.county = results[which(results[,1] == DATA$COUNTY[K]),]
votes.total = sum(results.county[,8])
votes.repub = sum(results.county[which(results.county[,7]=="REP"),8])
votes.dem = sum(results.county[which(results.county[,7]=="DEM"),8])
Y[K] = (votes.repub - votes.dem) / votes.total}
DATA$Y_3 = Y

###################################################################################################################
########### Read 2006 Data
results = read.csv("results_2006.csv",header = FALSE)
results = results[grepl("US CONGRESS",results[,5]),] ; results[,8] = as.numeric(results[,8])

Y = rep(NA,100)
for(K in 1:100){
results.county = results[which(results[,1] == DATA$COUNTY[K]),]
votes.total = sum(results.county[,8])
votes.repub = sum(results.county[which(results.county[,7]=="REP"),8])
votes.dem = sum(results.county[which(results.county[,7]=="DEM"),8])
Y[K] = (votes.repub - votes.dem) / votes.total}
DATA$Y_4 = Y

###################################################################################################################
########### Read 2008 Data
results = read.csv("results_2008.csv",header = FALSE)
results = results[grepl("US HOUSE",results[,6]),] ; results[,13] = as.numeric(results[,13])

Y = rep(NA,100)
for(K in 1:100){
results.county = results[which(results[,1] == DATA$COUNTY[K]),]
votes.total = sum(results.county[,13])
votes.repub = sum(results.county[which(results.county[,9]=="REP"),13])
votes.dem = sum(results.county[which(results.county[,9]=="DEM"),13])
Y[K] = (votes.repub - votes.dem) / votes.total}
DATA$Y_5 = Y

###################################################################################################################
########### Read 2010 Data
results = read.csv("results_2010.csv",header = FALSE)
results = results[grepl("US HOUSE",results[,6]),] ; results[,14] = as.numeric(results[,14])

Y = rep(NA,100)
for(K in 1:100){
results.county = results[which(results[,1] == DATA$COUNTY[K]),]
votes.total = sum(results.county[,14])
votes.repub = sum(results.county[which(results.county[,9]=="REP"),14])
votes.dem = sum(results.county[which(results.county[,9]=="DEM"),14])
Y[K] = (votes.repub - votes.dem) / votes.total}
DATA$Y_6 = Y

###################################################################################################################
########### Read 2012 Data
results = read.csv("results_2012.csv",header = FALSE)
results = results[grepl("US HOUSE",results[,6]),] ; results[,14] = as.numeric(results[,14])

Y = rep(NA,100)
for(K in 1:100){
results.county = results[which(results[,1] == DATA$COUNTY[K]),]
votes.total = sum(results.county[,14])
votes.repub = sum(results.county[which(results.county[,9]=="REP"),14])
votes.dem = sum(results.county[which(results.county[,9]=="DEM"),14])
Y[K] = (votes.repub - votes.dem) / votes.total}
DATA$Y_7 = Y

###################################################################################################################
########### Read 2014 Data
results = read.csv("results_2014.csv",header = FALSE)
results = results[grepl("US HOUSE",results[,6]),] ; results[,14] = as.numeric(results[,14])

Y = rep(NA,100)
for(K in 1:100){
results.county = results[which(results[,1] == DATA$COUNTY[K]),]
votes.total = sum(results.county[,14])
votes.repub = sum(results.county[which(results.county[,8]=="REP"),14])
votes.dem = sum(results.county[which(results.county[,8]=="DEM"),14])
Y[K] = (votes.repub - votes.dem) / votes.total}
DATA$Y_8 = Y

###################################################################################################################
########### Read 2016 Data
results = read.csv("results_2016.csv",header = FALSE)
results = results[grepl("US HOUSE",results[,6]),] ; results[,14] = as.numeric(results[,14])

Y = rep(NA,100)
for(K in 1:100){
results.county = results[which(results[,1] == DATA$COUNTY[K]),]
votes.total = sum(results.county[,14])
votes.repub = sum(results.county[which(results.county[,8]=="REP"),14])
votes.dem = sum(results.county[which(results.county[,8]=="DEM"),14])
Y[K] = (votes.repub - votes.dem) / votes.total}
DATA$Y_9 = Y


