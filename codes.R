#The codes below demenstrate all processes that have been mentioned in the report.
#### If you encounter any problem relavant to reproducibility. Please contact me with e-mail: yaseru2003@gmal.com. This is the emalil I always use.
#author: Ning Zhang
######

#Crime Data Preparations Pipeline
library(ggplot2)
library(zoo)
library(forecast)
source("support_functions.R")
############################  DATA PREPARATION  ############################

#Load in all crime data
load("~/Dropbox/URclasses/Practicum/RPDBlueLightProject/RPDBlueLight/data/dbofinal2005.RDATA")
load("~/Dropbox/URclasses/Practicum/RPDBlueLightProject/RPDBlueLight/data/dbofinal2006.RDATA")
load("~/Dropbox/URclasses/Practicum/RPDBlueLightProject/RPDBlueLight/data/dbofinal2007.RDATA")
load("~/Dropbox/URclasses/Practicum/RPDBlueLightProject/RPDBlueLight/data/dbofinal2008.RDATA")
load("~/Dropbox/URclasses/Practicum/RPDBlueLightProject/RPDBlueLight/data/lerms2009.RDATA")
load("~/Dropbox/URclasses/Practicum/RPDBlueLightProject/RPDBlueLight/data/lerms2010.RDATA")
load("~/Dropbox/URclasses/Practicum/RPDBlueLightProject/RPDBlueLight/data/lerms2011.RDATA")
load("~/Dropbox/URclasses/Practicum/RPDBlueLightProject/RPDBlueLight/data/lerms2012.RDATA")
load("~/Dropbox/URclasses/Practicum/RPDBlueLightProject/RPDBlueLight/data/lerms2013.RDATA")
load("~/Dropbox/URclasses/Practicum/RPDBlueLightProject/RPDBlueLight/data/lerms2014.RDATA")
load("~/Dropbox/URclasses/Practicum/RPDBlueLightProject/RPDBlueLight/data/lerms2015.RDATA")
load("~/Dropbox/URclasses/Practicum/RPDBlueLightProject/RPDBlueLight/data/lerms2016.RDATA")

#column number: 37243+38128+32986+34091+31248+33330+32032+29625+26517+22985+22461+22569

#clean 05 to 08 data
####convert geo coordinates in data 2005 to 2008 into lon-lat formats #####
# #support function to strip off unrelated symbols
get_geocoords <- function(data){
    names(data) <- c('Lat','Lon')
    data$Lat <- gsub('[(]', '', data$Lat)
    data$Lon <- gsub('[)]','', data$Lon)
    return(data)
}

##load in location informations
loc2005 <- read.table('data/dbofinal2005.txt',header = F, sep = ",")
loc2006 <- read.table('data/dbofinal2006.txt',header = F, sep = ",")
loc2007 <- read.table('data/dbofinal2007.txt',header = F, sep = ",")
loc2008 <- read.table('data/dbofinal2008.txt',header = F, sep = ",")
loc2005 <- get_geocoords(loc2005)
loc2006 <- get_geocoords(loc2006)
loc2007 <- get_geocoords(loc2007)
loc2008 <- get_geocoords(loc2008)
#incorporated converted location information back to the data frames
dbo.final.2005$Lat <- loc2005$Lat
dbo.final.2005$Lon <- loc2005$Lon
dbo.final.2006$Lat <- loc2006$Lat
dbo.final.2006$Lon <- loc2006$Lon
dbo.final.2007$Lat <- loc2007$Lat
dbo.final.2007$Lon <- loc2007$Lon
dbo.final.2008$Lat <- loc2008$Lat
dbo.final.2008$Lon <- loc2008$Lon
####function used to clean data from 05 to 08 ####
clean_old_data <- function(dataset,columnNames){
    dataset <- dataset[,columnNames]
    dataset$division <- gsub('SEC', '',dataset$division)
    weekday <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday','Friday','Saturday', 'Sunday')
    month <- c("January", "February", "March", "April", "May", "June", "July", "August", "September",
               "October", "November", "December")
    dow <- c()
    for (i in 1:length(dataset$dow)){
        dow <- c(dow, weekday[dataset$dow[i]])
    }
    dataset$dow <- dow
    
    offenseMonth <- dataset$month
    m <- c()
    for (i in 1:length(offenseMonth)){
        m <- c(m, month[offenseMonth[i]])
    }
    dataset$month <- m
    return(dataset)
}
####clean the data from 05 to 08 ####
dboNames <- c("CR", "address", "location_type", "Lon", "Lat", "ucr_desc", "psa",
              "division", "descrip", "occur_time", "occur_to_time", "occur_date",
              "dow", "month", "quarter_calend", "year", "platoon", "title", "artsect",
              "case_status", "attempt")
new2005 <- clean_old_data(dbo.final.2005,dboNames)
new2006 <- clean_old_data(dbo.final.2006,dboNames)
new2007 <- clean_old_data(dbo.final.2007,dboNames)
new2008 <- clean_old_data(dbo.final.2008,dboNames)
#merge
crime05to08 <- rbind.data.frame(new2005,new2006,new2007,new2008)
#export the merged data
#export(crime05to08,"~/Dropbox/URclasses/Practicum/Final Project/data/crime05to08.rds")

#clean 09 to 16 data

clean_data_after2008 <- function(data, columnNames){
    subData <- data[,columnNames]
    subData$OffenseQuarter <- gsub('Q','',subData$OffenseQuarter)
    statute <- subData$Statute
    
    st <- c()
    for (i in 1:length(statute)){
        st <- c(st,strsplit(statute[i],' ')[[1]][2])
    }
    
    subData$Statute <- st
    return(subData)
}

lermsNames <- c("CaseNumber", "FullAddress", "LocationSceneDescription",
                "Lon","Lat", "CrimeType", "GEOBeat", "Section", "Description",
                "OccurFromTime", "OccurToTime", "OccurToDate", "OffenseDOW",
                "OffenseMonth", "OffenseQuarter", "OffenseYear", "RPDPlatoon",
                "Title", "Statute", "CaseStatusValue", "AttemptedValue")

new2009 <- clean_data_after2008(lerms2009,lermsNames)
new2010 <- clean_data_after2008(lerms2010,lermsNames)
new2011 <- clean_data_after2008(lerms2011,lermsNames)
new2012 <- clean_data_after2008(lerms2012,lermsNames)
new2013 <- clean_data_after2008(lerms2013,lermsNames)
new2014 <- clean_data_after2008(lerms2014,lermsNames)
new2015 <- clean_data_after2008(lerms2015,lermsNames)
new2016 <- clean_data_after2008(lerms2016,lermsNames)

crime09to16 <- rbind(new2009, new2010, new2011, new2012, new2013, new2014, new2015, new2016)

names(crime05to08) <- names(crime09to16)
crime <- rbind.data.frame(crime05to08,crime09to16)
#clean unused data
rm(loc2005,loc2006,loc2007,loc2008)
#filter out records before 2005
crime <- crime %>%
    filter(OffenseYear > 2004)
#correct lon lat data type
crime$Lat <- as.numeric(crime$Lat)
crime$Lon <- as.numeric(crime$Lon)

#arrange crime data
crime <- crime %>%
    arrange(OffenseYear,OffenseQuarter,OffenseMonth)
month <- c("January", "February", "March", "April", "May", "June", "July", "August", "September",
           "October", "November", "December")
digitMonth <- 1:12
m <- crime$OffenseMonth
mm <- sapply(m,function(x){which(month == x)})
mm <- as.vector(mm)
crime$Month <- mm
## clean invalid lon-lat values
summary(crime$Lat)
crime <- crime %>% filter(Lat > 40)
#as a result, a total of 356,696 observations will be used through following processes.

####clean feature values in CrimeType ####
crimetype <- crime$CrimeType
crimetype <- tolower(crimetype)
crimetype <- gsub("all other offenses [(]except traffic[)]*", "all other offenses", crimetype)
crimetype <- gsub("all other offenses", "all other offenses (except traffic)", crimetype)
crimetype <- gsub("controlled substance possession: marijuana", "controlled substances", crimetype)
crimetype <- gsub("controlled substance possession: opium, cocaine, or derivatives", "controlled substances", crimetype)
crimetype <- gsub("controlled substance possession: other", "controlled substances", crimetype)
crimetype <- gsub("controlled substance sale: marijuana", "controlled substances", crimetype)
crimetype <- gsub("controlled substance sale: other", "controlled substances", crimetype)
crimetype <- gsub("forgery and counterfeitin", "forgery&counterfeiting", crimetype)
crimetype <- gsub("gambling - other", "gambling",crimetype)
crimetype <- gsub("loitering [(]vagrancy[)]*", "loitering", crimetype)
crimetype <- gsub("negligent manslaughter|non-negligent manslaughter", "manslaughter", crimetype)
crimetype <- gsub("offenses against the fami|offenses against the family", "offenses against the family", crimetype)
crimetype <- gsub("offenses against public o|offenses against public order", "offenses against public order", crimetype)
crimetype <- gsub("possession of burglar's t", "possession of burglars tools", crimetype)
crimetype <- gsub("patronizing prostitutes|promoting prostitution", "prostitution", crimetype)
crimetype <- gsub("rape, forcible", "rape",crimetype)
crimetype <- gsub("unauthorized use of vehic|unauthorized use of vehicle", "unauthorized use of vehicle", crimetype)
crimetype <- gsub("sex offense [(]except forcible rape or prostitution[)]*", "sex offense", crimetype)
crimetype[is.na(crimetype)] <- "unknown"
crimetype[crimetype == ""] <- "unknown"
crimetype[grep("n/a",crimetype)] <- "unknown"
crime$CrimeType <- crimetype
####export crime data ####
export(crime,"crime.rds")
#############
########after getting feedback from RPD, redo exploring process ####
##Part One Crime homicide, rape, robbery, aggravated assault, burglary, motor vehicle theft, larceny, and arson
##keep Part One Crime, for larceny, filter out petit larceny
PartOneCrime <- c('aggravated assault', 'arson', 'burglary', 'dangerous weapons', 'homicide', 'kidnapping', 'larceny', 'manslaughter', 'murder', 'mv theft', 'rape', 'robbery', 'stolen property', 'unknown')

felony <- crime %>% filter(CrimeType %in% PartOneCrime)
test <- felony[!grepl('Petit|PETIT', felony$Description),]
test <- test %>% filter(CrimeType != 'unknown' | Statute == '155.35' | Statute == '215.52')
felony <- test
export(crime,'fenoly.rds')


##########load in raw intersections data #################
intersections <- read.table("intersections.txt",header = F,sep=",")
names(intersections) <- c('lat','lon')
intGeo <- cbind(intersections$lon, intersections$lat)
intersections$id <- 1:dim(intersections)[1]
city_intersections <- readRDS('data/processedData/city_intersections_geoCoords.rds')

final_intersections <- readRDS('data/processedData/final_control_geoCoords.rds')
m <- leaflet() %>%
    addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
    setView(lng = -77.610924, lat = 43.161030, zoom = 12)
m %>% addCircles(lng = final_intersections$lon, lat = final_intersections$lat, color = "blue", stroke = T, radius = 25, fillOpacity = 1) %>% addCircles(lng = camera$X, lat=camera$Y, stroke = T, radius = 25, color = "red", fillOpacity = 1)



############################  EXPLORATORY ANALYSIS   ############################

#barplot for top 10 of all crimes
crimeSummary <- crime %>% group_by(CrimeType) %>% summarise(count = n())
crimeSummary <- crimeSummary %>% arrange(desc(count))
crimeSummary <- crimeSummary[1:10,]
barplot(crimeSummary)
positions <- crimeSummary$CrimeType
p<-ggplot(data=crimeSummary, aes(x = CrimeType, y = count)) +
    geom_bar(stat="identity")+ scale_x_discrete(limits = positions) +theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + labs(title = "Top 10 Crime Types in the City of Rochester (2005-2016)", size = 15)
p
#barplot for top 10 felony types
felonySummary <- felony %>% group_by(CrimeType) %>% summarise(count = n())
felonySummary <- felonySummary %>% arrange(desc(count))
felonySummary <- felonySummary[1:10,]
felonySummary$CrimeType[2] <- "larceny (non-petit)"
positions <- felonySummary$CrimeType
p<-ggplot(data=felonySummary, aes(x = CrimeType, y = count)) +
    geom_bar(stat="identity")+ scale_x_discrete(limits = positions) +theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + labs(title = "Top 10 Felonies in the City of Rochester (2005-2016)", size = 15)
p
#
#time series plot and data summary
summaryData <- crime %>%
    group_by(OffenseYear,Month) %>%
    summarise(crime_count = n())
#average count of crime activities each year
meanByYear <- summaryData %>%
    group_by(OffenseYear) %>%
    summarise(mean = mean(crime_count),variance = var(crime_count))
#barplot crime count by month
barplot(summaryData$crime_count)

crime.forecast = ma(summaryData$crime_count, order = 12, centre = T)
plot(as.ts(summaryData$crime_count))
lines(crime.forecast)

plot(crime.ts,main = "Rochester Monthly Criminal Activity", ylab = "Frequency", col = "blue")

dates <- c()
for(i in dim(Dataset)[1]){
    d <- paste(Dataset$OffenseYear[i],"-", Dataset$Month[i], sep="")
}

dates <- paste(summaryData$OffenseYear, "-",summaryData$Month, "-1", sep="")
newData <- cbind(dates, Dataset$crime_count)
newData <- as.data.frame(newData)
names(newData) <- c("Date", "Count")
####functions for drawing crime trend ####
draw_crime_trend <- function(Dataset, titleName = "Monthly Frequency of Crime Activities in the City of Rochester", legPosition = "none"){

    crime.forecast = ma(Dataset$crime_count, order = 12, centre = T)
    testData <- fortify.zoo(as.zoo(crime.forecast))
    names(testData) <- c("Index", "count")
    crime.ts = ts(Dataset$crime_count, frequency=12, start=c(2005,1), end=c(2016,12))
    newData <- fortify.zoo(as.zoo(crime.ts))
    names(newData) <- c("Index", "count")
    newData$Index <- 1:144
    ggplot(data = testData, aes(x = Index, y = count, color = "Trend"))+geom_line(linetype = 1, size = 2) + geom_line(data=newData,aes(x = Index, y=count, color = "Monthly Count"),size = 2) + labs(x = "Year.Month", y = "Monthly Frequency", title = titleName) + theme(plot.title = element_text(hjust = 0.5), legend.position=legPosition)
}

####ggplot2 for crime trend
draw_crime_trend(summaryData, titleName = "Monthly Frequency of All-Crime Activities", legPosition = "right")
##ggplot2 for felony trend
#time series plot and data summary for felony data
summaryDataFelony <- felony %>%
    group_by(OffenseYear,Month) %>%
    summarise(crime_count = n())
draw_crime_trend(summaryDataFelony, titleName = "Monthly Frequency of Felony Activities", legPosition = "none")

######contour map ####
camGeo <- cbind(camera$X,camera$Y)
Dist50 <- c()
for (i in 1:dim(camGeo)[1]) {
    distance <- distMeeus(camGeo[i,], crime[,c('Lon','Lat')])
    Dist50 <- cbind(Dist50,as.integer(distance <= 50))
}
Dist50 <- as.data.frame(Dist50)

#count monthly frequency of crime activities within 50 meters around each camera
names(Dist50) <- sapply(camera$OBJECTID,FUN = function(x){paste("Camera",x, sep="")})
crime_camera_50 <- cbind(crime, Dist50)
crime_camera_50 <- tbl_df(crime_camera_50)
crime_nearby50 <- crime_camera_50[,c('OffenseYear','Month',names(Dist50))]
crime_nearby50 <- crime_nearby50 %>%
    group_by(OffenseYear, Month) %>%
    summarise_all(sum)

crimeBefore2009 <- crime_camera_50 %>% filter(OffenseYear < 2009)

crimeAfter09to12 <- crime_camera_50 %>% filter(OffenseYear >= 2009 & OffenseYear < 2013)


####contour mapping
AllNearbyCrimesBefore2009 <- fetchNearbyCrimes(crimeBefore2009)
AllNearbyCrimesBefore2009 <- AllNearbyCrimesBefore2009[!duplicated(AllNearbyCrimesBefore2009), ]
#crime ranks before 2009
crimeTypeDist50Before2009 <- crimeTypeCounting(AllNearbyCrimesBefore2009, start_column = 23)
crimeTypeDist50Before2009 <- renameColmunsAndIDs(camera,crimeTypeDist50Before2009)
#####
#after 2009
AllNearbyCrimes09to12 <- fetchNearbyCrimes(crimeAfter09to12)
AllNearbyCrimes09to12 <- AllNearbyCrimes09to12[!duplicated(AllNearbyCrimes09to12), ]
#crime ranks after 2009
crimeTypeDist50From09to12 <- crimeTypeCounting(AllNearbyCrimes09to12,start_column = 23)
crimeTypeDist50From09to12 <- renameColmunsAndIDs(camera,crimeTypeDist50From09to12)
#######
##
#crimeBefore2009 <- crimeBefore2009 %>% filter(Lat > 41)

cameraBefore2009 <- camera[camera$Year == 2009 & !is.na(camera$Year), ]
draw_contour(crimeBefore2009,cameraBefore2009, crimeTypeDist50Before2009)
draw_contour(crimeAfter09to12, cameraBefore2009, crimeTypeDist50From09to12)

###repeat above drawing codes on felony data
camGeo <- cbind(camera$X,camera$Y)
Dist50 <- c()
for (i in 1:dim(camGeo)[1]) {
    distance <- distMeeus(camGeo[i,], felony[,c('Lon','Lat')])
    Dist50 <- cbind(Dist50,as.integer(distance <= 50))
}
Dist50 <- as.data.frame(Dist50)

#count monthly frequency of crime activities within 50 meters around each camera
names(Dist50) <- sapply(camera$OBJECTID,FUN = function(x){paste("Camera",x, sep="")})
felony_camera_50 <- cbind(felony, Dist50)
felony_camera_50 <- tbl_df(felony_camera_50)
felony_nearby50 <- felony_camera_50[,c('OffenseYear','Month',names(Dist50))]
felony_nearby50 <- felony_nearby50 %>%
    group_by(OffenseYear, Month) %>%
    summarise_all(sum)

felonyBefore2009 <- felony_camera_50 %>% filter(OffenseYear < 2009)

felonyAfter09to12 <- felony_camera_50 %>% filter(OffenseYear >= 2009 & OffenseYear < 2013)


####contour mapping
AllNearbyfelonysBefore2009 <- fetchNearbyCrimes(felonyBefore2009)
AllNearbyfelonysBefore2009 <- AllNearbyfelonysBefore2009[!duplicated(AllNearbyfelonysBefore2009), ]
#felony ranks before 2009
felonyTypeDist50Before2009 <- crimeTypeCounting(AllNearbyfelonysBefore2009, start_column = 23)
felonyTypeDist50Before2009 <- renameColmunsAndIDs(camera,felonyTypeDist50Before2009)
#####
#after 2009
AllNearbyfelonys09to12 <- fetchNearbyCrimes(felonyAfter09to12)
AllNearbyfelonys09to12 <- AllNearbyfelonys09to12[!duplicated(AllNearbyfelonys09to12), ]
#felony ranks after 2009
felonyTypeDist50From09to12 <- crimeTypeCounting(AllNearbyfelonys09to12,start_column = 23)
felonyTypeDist50From09to12 <- renameColmunsAndIDs(camera,felonyTypeDist50From09to12)
#######
##
#felonyBefore2009 <- felonyBefore2009 %>% filter(Lat > 41)

cameraBefore2009 <- camera[camera$Year == 2009 & !is.na(camera$Year), ]
draw_contour(felonyBefore2009,cameraBefore2009, felonyTypeDist50Before2009)
draw_contour(felonyAfter09to12, cameraBefore2009, felonyTypeDist50From09to12)









############## Effective Analysis ###############
#####FUNCTION: generalize the wilcoxon test into a function ########
wilcoxon_test <- function(data){
    data <- data %>% arrange(OffenseYear,Month)
    indx = which(data$OffenseYear == 2009 & data$Month == 1)
    dataPart1 <- data[1:indx-1,]
    dataPart2 <- data[indx:dim(data)[1],]
    ControlwxPv2009 <- c()
    for (i in 3:dim(data)[2]){
        #columnName = paste('Camera',i,sep="")
        #print(i)
        t = wilcox.test(dataPart1[[i]], dataPart2[[i]], alternative = 'greater', exact = FALSE)
        ControlwxPv2009 <- c(ControlwxPv2009, t$p.value)
    }
    return(ControlwxPv2009)
    
}

####CLEANED VERSION: COMPARISON BTW 2009 CAMERAS AND CONTROL INTERSECTIONS ###########
##
#load in final control intersections
FinalControlCrime_nearby50 <- readRDS('data/processedData/Final_control_intersections_crime_count_dist50.rds')
final_control_geo <- readRDS('data/processedData/final_control_geoCoords.rds')
#load in monthly crime counts around cameras
crime_nearby50 <- readRDS('data/processedData/near_camera_crime_count_dist50.rds')
#filter out those installed in years other than 2009
cam2009 <- crime_nearby50[,c(1,2, which(camera$Year == 2009) + 2)]


####COMAPRISON 1: quarterly crime counts comparison using t-tests#############
####funcitons that conduct t-tests ####
compareBySeasonSum <- function(data, before = 2008, after = 2009, month=c(1,2,3)){
    dataBefore <- data %>% filter(OffenseYear == before & Month %in% month)
    dataAfter <- data %>% filter(OffenseYear == after & Month %in% month)
    sumBefore <- apply(dataBefore[,3:dim(dataBefore)[2]], 2, FUN = function(x){sum(x, na.rm = T)})
    sumAfter <- apply(dataAfter[,3:dim(dataAfter)[2]], 2, FUN = function(x){sum(x, na.rm = T)})
    result = (sumBefore - sumAfter)
    return(result)
}

#on batch 2008
#test on cameras
resultCam = compareBySeasonSum(cam2009,before = 2008, after = 2009, month = c(1,2,3))
t.test(resultCam, alternative = "greater")
#teston control group
#controlAfterOneYear <- FinalControlCrime_nearby50 %>% filter(OffenseYear < 2010)
resultControl = compareBySeasonSum(FinalControlCrime_nearby50,before = 2008, after = 2009, month = c(1,2,3))
t.test(resultControl, alternative = "greater")

#on batch 2016
#pick out cameras installed in 2016, compare crime counts in 6 months after installation with the counts in
#same period of last year.
#filter out those installed in years other than 2016
cam2016 <- crime_nearby50[,c(1,2, which(camera$Year == 2016) + 2)]
resultCam2016 = compareBySeasonSum(cam2016, before = 2015, after = 2016, month = c(7,8,9))
summary(resultCam2016)
t.test(resultCam2016, alternative = "greater")
#as a result, control values have significant change, while cameras do not.
resultControl = compareBySeasonSum(FinalControlCrime_nearby50, before = 2015, after = 2016, month = c(7,8,9))
summary(resultControl)
t.test(resultControl, alternative = "greater")

#### COMPARISON 2: six-month crime counts comparison using t-tests ####
#on batch 2008
#test on cameras
resultCam = compareBySeasonSum(cam2009,before = 2008, after = 2009, month = c(1,2,3,4,5,6))
t.test(resultCam, alternative = "greater")
#teston control group
resultControl = compareBySeasonSum(FinalControlCrime_nearby50,before = 2008, after = 2009, month = c(1,2,3,4,5,6))
t.test(resultControl, alternative = "greater")

#on batch 2016
#pick out cameras installed in 2016, compare crime counts in 6 months after installation with the counts in
#same period of last year.
#filter out those installed in years other than 2016
cam2016 <- crime_nearby50[,c(1,2, which(camera$Year == 2016) + 2)]
resultCam2016 = compareBySeasonSum(cam2016, before = 2015, after = 2016, month = c(7,8,9,10,11,12))
summary(resultCam2016)
t.test(resultCam2016, alternative = "greater")
#as a result, control values have significant change, while cameras do not.
resultControl = compareBySeasonSum(FinalControlCrime_nearby50, before = 2015, after = 2016, month = c(7,8,9,10,11,12))
summary(resultControl)
t.test(resultControl, alternative = "greater")

####COMPARISON 3: ONE YEAR BEFORE AND AFTER INSTALLATION, T TEST#########
##sum up all crime activities that happened in one year before or after installation and then
#### function that implement t-tests on yearly crime counts ####
compareByYearSum <- function(data, before = 2008, after = 2009){
    dataBefore <- data %>% filter(OffenseYear == 2008)
    dataAfter <- data %>% filter(OffenseYear == 2009)
    sumBefore <- apply(dataBefore[,3:dim(dataBefore)[2]], 2, FUN = function(x){sum(x, na.rm = T)})
    sumAfter <- apply(dataAfter[,3:dim(dataAfter)[2]], 2, FUN = function(x){sum(x, na.rm = T)})
    result = (sumBefore - sumAfter)
    return(result)
}

#t-test on batch 2008
resultCam = compareByYearSum(cam2009)
t.test(resultCam)
#t-test on control group
resultControl = compareByYearSum(FinalControlCrime_nearby50)
t.test(resultControl)


####SERIES OF WILCOXON TESTS ####
wilTestCamResults <- c()
wilTestConResults <- c()
#### ONE YEAR COMPARISON: WILCOXON TEST, ONE YEAR BEFORE AND AFTER INSTALLATION #######
#### on batch 2008
cam2009AfterOneYear <- cam2009 %>% filter(OffenseYear < 2010)
wxPv2009AfterOneYear <- wilcoxon_test(cam2009AfterOneYear)
resCam <- sum(wxPv2009AfterOneYear < 0.025)/length(wxPv2009AfterOneYear)
wilTestCamResults <- c(wilTestCamResults,resCam)
##control
controlAfterOneYear <- FinalControlCrime_nearby50 %>% filter(OffenseYear < 2010)
ControlwxPvAfterOneYear <- wilcoxon_test(controlAfterOneYear)
resCon <- sum(ControlwxPvAfterOneYear < 0.025)/length(ControlwxPvAfterOneYear)
wilTestConResults <- c(wilTestConResults,resCon)
#### TWO YEAR COMPARISON: WILCOXON TEST, TWO YEAR BEFORE AND AFTER INSTALLATION #######

#### on batch 2008
cam2009AfterTwoYear <- cam2009 %>% filter(OffenseYear < 2011)
wxPv2009AfterTwoYear <- wilcoxon_test(cam2009AfterTwoYear)
resCam <- sum(wxPv2009AfterTwoYear < 0.025)/length(wxPv2009AfterTwoYear)
wilTestCamResults <- c(wilTestCamResults,resCam)
##control
controlAfterTwoYear <- FinalControlCrime_nearby50 %>% filter(OffenseYear < 2011)
ControlwxPvAfterTwoYear <- wilcoxon_test(controlAfterTwoYear)
resCon <- sum(ControlwxPvAfterTwoYear < 0.025)/length(ControlwxPvAfterTwoYear)
wilTestConResults <- c(wilTestConResults,resCon)
#### THREE YEAR COMPARISON: WILCOXON TEST, THREE YEAR BEFORE AND AFTER INSTALLATION #######

####cameras
cam2009AfterThreeYear <- cam2009 %>% filter(OffenseYear < 2012)
wxPv2009AfterThreeYear <- wilcoxon_test(cam2009AfterThreeYear)
resCam <- sum(wxPv2009AfterThreeYear < 0.025)/length(wxPv2009AfterThreeYear)
wilTestCamResults <- c(wilTestCamResults,resCam)
##control
controlAfterThreeYear <- FinalControlCrime_nearby50 %>% filter(OffenseYear < 2012)
ControlwxPvAfterThreeYear <- wilcoxon_test(controlAfterThreeYear)
resCon <- sum(ControlwxPvAfterThreeYear < 0.025)/length(ControlwxPvAfterThreeYear)
wilTestConResults <- c(wilTestConResults,resCon)
####FOUR YEARS COMPARISON: BEFORE AND AFTER INSTALLATION, WILCOXON TEST####
#cameras
cam2009BAfourYears <- cam2009 %>% filter(OffenseYear < 2013)
wxPv2009BAfourYears <- wilcoxon_test(cam2009BAfourYears)
resCam <- sum(wxPv2009BAfourYears < 0.025)/length(wxPv2009BAfourYears)
wilTestCamResults <- c(wilTestCamResults,resCam)
#control
controlBAfourYears <- FinalControlCrime_nearby50 %>% filter(OffenseYear < 2013)
ControlwxPvBAfourYears <- wilcoxon_test(controlBAfourYears)
resCon <- sum(ControlwxPvBAfourYears < 0.025)/length(ControlwxPvBAfourYears)
wilTestConResults <- c(wilTestConResults,resCon)
#### FIVE YEAR COMPARISON: WILCOXON TEST, THREE YEAR BEFORE AND AFTER INSTALLATION #######
####cameras
cam2009AfterFiveYear <- cam2009 %>% filter(OffenseYear < 2014)
wxPv2009AfterFiveYear <- wilcoxon_test(cam2009AfterFiveYear)
resCam <- sum(wxPv2009AfterFiveYear < 0.025)/length(wxPv2009AfterFiveYear)
wilTestCamResults <- c(wilTestCamResults,resCam)
##control
controlAfterFiveYear <- FinalControlCrime_nearby50 %>% filter(OffenseYear < 2014)
ControlwxPvAfterFiveYear <- wilcoxon_test(controlAfterFiveYear)
recCon <- sum(ControlwxPvAfterFiveYear < 0.025)/length(ControlwxPvAfterFiveYear)
wilTestConResults <- c(wilTestConResults,resCon)
#### SIX YEAR COMPARISON: WILCOXON TEST, THREE YEAR BEFORE AND AFTER INSTALLATION #######

####cameras
cam2009AfterSixYear <- cam2009 %>% filter(OffenseYear < 2015)
wxPv2009AfterSixYear <- wilcoxon_test(cam2009AfterSixYear)
resCam <- sum(wxPv2009AfterSixYear < 0.025)/length(wxPv2009AfterSixYear)
wilTestCamResults <- c(wilTestCamResults,resCam)
##control
controlAfterSixYear <- FinalControlCrime_nearby50 %>% filter(OffenseYear < 2015)
ControlwxPvAfterSixYear <- wilcoxon_test(controlAfterSixYear)
resCon <- sum(ControlwxPvAfterSixYear < 0.025)/length(ControlwxPvAfterSixYear)
wilTestConResults <- c(wilTestConResults,resCon)
#### SEVEN YEAR COMPARISON: WILCOXON TEST, THREE YEAR BEFORE AND AFTER INSTALLATION #######
####cameras
cam2009AfterSevenYear <- cam2009 %>% filter(OffenseYear < 2016)
wxPv2009AfterSevenYear <- wilcoxon_test(cam2009AfterSevenYear)
resCam <- sum(wxPv2009AfterSevenYear < 0.025)/length(wxPv2009AfterSevenYear)
wilTestCamResults <- c(wilTestCamResults,resCam)
##control
controlAfterSevenYear <- FinalControlCrime_nearby50 %>% filter(OffenseYear < 2016)
ControlwxPvAfterSevenYear <- wilcoxon_test(controlAfterSevenYear)
resCon <- sum(ControlwxPvAfterSevenYear < 0.025)/length(ControlwxPvAfterSevenYear)
wilTestConResults <- c(wilTestConResults,resCon)
####FINAL COMPARISON: USING ALL YEARS OF DATA, WILCOXOON TEST
#wilcoxon test on cameras installed before 2009
wxPv2009 <- wilcoxon_test(cam2009)
#fraction of the total that passes wilcoxon test
resCam <- sum(wxPv2009 < 0.025)/length(wxPv2009)
wilTestCamResults <- c(wilTestCamResults,resCam)
#####implement wilcoxon test on control intersections
finalControlWxPv2009 <- wilcoxon_test(FinalControlCrime_nearby50)
resCon <- sum(finalControlWxPv2009 < 0.025) / length(finalControlWxPv2009)
wilTestConResults <- c(wilTestConResults,resCon)

###### END

##plotting
years <- 1:8
values <- c(wilTestCamResults*100, wilTestConResults*100)
results <- data.frame(years,values)
#results <- as.data.frame(results)
names(results) <- c("Accumulated_Years_Since_Installation","Percent")
barplot(wilTestCamResults$Percent)
Percentage_In_Group <-c(rep("Camera Batch 2008", 8), rep("Control Group", 8))
p<-ggplot(data=results, aes(x = Accumulated_Years_Since_Installation, y = Percent)) +
    geom_bar(stat="identity", aes(fill = Percentage_In_Group),position = "dodge")  + labs(title = "Percentage of Members Rejecting Wilcoxon Test") + theme(plot.title = element_text(hjust = 0.5))
p

Percentage_In_Group
results
