# Package ID: edi.1075.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Fish abundance in the San Francisco Estuary (1959-2024), an integration of 10 monitoring surveys..
# Data set creator:  Samuel Bashevkin - State Water Board 
# Data set creator:  Trinh Nguyen - California Department of Fish and Wildlife and the Interagency Ecological Program 
# Data set creator:  Enoch Tham - CDFW Water Branch 
# Data set creator:  Jereme Gaeta - California Department of Fish and Wildlife and the Interagency Ecological Program 
# Data set creator:  Lara Mitchell - U.S. Fish and Wildlife Service 
# Data set creator:  Shruti Khanna - California Department of Fish and Wildlife and the Interagency Ecological Program 
# Contact:  Samuel Bashevkin -  State Water Board  - sam.bashevkin@waterboards.ca.gov
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1075/2/79240c490fe74543da6b86a1c7c751b9" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Source",     
                 "Station",     
                 "Latitude",     
                 "Longitude",     
                 "Date",     
                 "Datetime",     
                 "Survey",     
                 "Depth",     
                 "SampleID",     
                 "Method",     
                 "Tide",     
                 "Sal_surf",     
                 "Sal_bot",     
                 "Temp_surf",     
                 "TurbidityNTU",     
                 "TurbidityFNU",     
                 "Secchi",     
                 "Secchi_estimated",     
                 "Tow_duration",     
                 "Tow_area",     
                 "Tow_volume",     
                 "Cable_length",     
                 "Tow_direction",     
                 "Notes_tow",     
                 "Notes_flowmeter"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Source)!="factor") dt1$Source<- as.factor(dt1$Source)
if (class(dt1$Station)!="factor") dt1$Station<- as.factor(dt1$Station)
if (class(dt1$Latitude)=="factor") dt1$Latitude <-as.numeric(levels(dt1$Latitude))[as.integer(dt1$Latitude) ]               
if (class(dt1$Latitude)=="character") dt1$Latitude <-as.numeric(dt1$Latitude)
if (class(dt1$Longitude)=="factor") dt1$Longitude <-as.numeric(levels(dt1$Longitude))[as.integer(dt1$Longitude) ]               
if (class(dt1$Longitude)=="character") dt1$Longitude <-as.numeric(dt1$Longitude)                                   
# attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$Date != "",]) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    

# attempting to convert dt1$Datetime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1Datetime<-as.POSIXct(dt1$Datetime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$Datetime != "",]) == length(tmp1Datetime[!is.na(tmp1Datetime)])){dt1$Datetime <- tmp1Datetime } else {print("Date conversion failed for dt1$Datetime. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$Survey)=="factor") dt1$Survey <-as.numeric(levels(dt1$Survey))[as.integer(dt1$Survey) ]               
if (class(dt1$Survey)=="character") dt1$Survey <-as.numeric(dt1$Survey)
if (class(dt1$Depth)=="factor") dt1$Depth <-as.numeric(levels(dt1$Depth))[as.integer(dt1$Depth) ]               
if (class(dt1$Depth)=="character") dt1$Depth <-as.numeric(dt1$Depth)
if (class(dt1$SampleID)!="factor") dt1$SampleID<- as.factor(dt1$SampleID)
if (class(dt1$Method)!="factor") dt1$Method<- as.factor(dt1$Method)
if (class(dt1$Tide)!="factor") dt1$Tide<- as.factor(dt1$Tide)
if (class(dt1$Sal_surf)=="factor") dt1$Sal_surf <-as.numeric(levels(dt1$Sal_surf))[as.integer(dt1$Sal_surf) ]               
if (class(dt1$Sal_surf)=="character") dt1$Sal_surf <-as.numeric(dt1$Sal_surf)
if (class(dt1$Sal_bot)=="factor") dt1$Sal_bot <-as.numeric(levels(dt1$Sal_bot))[as.integer(dt1$Sal_bot) ]               
if (class(dt1$Sal_bot)=="character") dt1$Sal_bot <-as.numeric(dt1$Sal_bot)
if (class(dt1$Temp_surf)=="factor") dt1$Temp_surf <-as.numeric(levels(dt1$Temp_surf))[as.integer(dt1$Temp_surf) ]               
if (class(dt1$Temp_surf)=="character") dt1$Temp_surf <-as.numeric(dt1$Temp_surf)
if (class(dt1$TurbidityNTU)=="factor") dt1$TurbidityNTU <-as.numeric(levels(dt1$TurbidityNTU))[as.integer(dt1$TurbidityNTU) ]               
if (class(dt1$TurbidityNTU)=="character") dt1$TurbidityNTU <-as.numeric(dt1$TurbidityNTU)
if (class(dt1$TurbidityFNU)=="factor") dt1$TurbidityFNU <-as.numeric(levels(dt1$TurbidityFNU))[as.integer(dt1$TurbidityFNU) ]               
if (class(dt1$TurbidityFNU)=="character") dt1$TurbidityFNU <-as.numeric(dt1$TurbidityFNU)
if (class(dt1$Secchi)=="factor") dt1$Secchi <-as.numeric(levels(dt1$Secchi))[as.integer(dt1$Secchi) ]               
if (class(dt1$Secchi)=="character") dt1$Secchi <-as.numeric(dt1$Secchi)
if (class(dt1$Secchi_estimated)!="factor") dt1$Secchi_estimated<- as.factor(dt1$Secchi_estimated)
if (class(dt1$Tow_duration)=="factor") dt1$Tow_duration <-as.numeric(levels(dt1$Tow_duration))[as.integer(dt1$Tow_duration) ]               
if (class(dt1$Tow_duration)=="character") dt1$Tow_duration <-as.numeric(dt1$Tow_duration)
if (class(dt1$Tow_area)=="factor") dt1$Tow_area <-as.numeric(levels(dt1$Tow_area))[as.integer(dt1$Tow_area) ]               
if (class(dt1$Tow_area)=="character") dt1$Tow_area <-as.numeric(dt1$Tow_area)
if (class(dt1$Tow_volume)=="factor") dt1$Tow_volume <-as.numeric(levels(dt1$Tow_volume))[as.integer(dt1$Tow_volume) ]               
if (class(dt1$Tow_volume)=="character") dt1$Tow_volume <-as.numeric(dt1$Tow_volume)
if (class(dt1$Cable_length)=="factor") dt1$Cable_length <-as.numeric(levels(dt1$Cable_length))[as.integer(dt1$Cable_length) ]               
if (class(dt1$Cable_length)=="character") dt1$Cable_length <-as.numeric(dt1$Cable_length)
if (class(dt1$Tow_direction)!="factor") dt1$Tow_direction<- as.factor(dt1$Tow_direction)
if (class(dt1$Notes_tow)!="factor") dt1$Notes_tow<- as.factor(dt1$Notes_tow)
if (class(dt1$Notes_flowmeter)!="factor") dt1$Notes_flowmeter<- as.factor(dt1$Notes_flowmeter)

# Convert Missing Values to NA for non-dates

dt1$Latitude <- ifelse((trimws(as.character(dt1$Latitude))==trimws("NA")),NA,dt1$Latitude)               
suppressWarnings(dt1$Latitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Latitude))==as.character(as.numeric("NA"))),NA,dt1$Latitude))
dt1$Longitude <- ifelse((trimws(as.character(dt1$Longitude))==trimws("NA")),NA,dt1$Longitude)               
suppressWarnings(dt1$Longitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Longitude))==as.character(as.numeric("NA"))),NA,dt1$Longitude))
dt1$Survey <- ifelse((trimws(as.character(dt1$Survey))==trimws("NA")),NA,dt1$Survey)               
suppressWarnings(dt1$Survey <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Survey))==as.character(as.numeric("NA"))),NA,dt1$Survey))
dt1$Depth <- ifelse((trimws(as.character(dt1$Depth))==trimws("NA")),NA,dt1$Depth)               
suppressWarnings(dt1$Depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Depth))==as.character(as.numeric("NA"))),NA,dt1$Depth))
dt1$Tide <- as.factor(ifelse((trimws(as.character(dt1$Tide))==trimws("NA")),NA,as.character(dt1$Tide)))
dt1$Sal_surf <- ifelse((trimws(as.character(dt1$Sal_surf))==trimws("NA")),NA,dt1$Sal_surf)               
suppressWarnings(dt1$Sal_surf <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Sal_surf))==as.character(as.numeric("NA"))),NA,dt1$Sal_surf))
dt1$Sal_bot <- ifelse((trimws(as.character(dt1$Sal_bot))==trimws("NA")),NA,dt1$Sal_bot)               
suppressWarnings(dt1$Sal_bot <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Sal_bot))==as.character(as.numeric("NA"))),NA,dt1$Sal_bot))
dt1$Temp_surf <- ifelse((trimws(as.character(dt1$Temp_surf))==trimws("NA")),NA,dt1$Temp_surf)               
suppressWarnings(dt1$Temp_surf <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Temp_surf))==as.character(as.numeric("NA"))),NA,dt1$Temp_surf))
dt1$TurbidityNTU <- ifelse((trimws(as.character(dt1$TurbidityNTU))==trimws("NA")),NA,dt1$TurbidityNTU)               
suppressWarnings(dt1$TurbidityNTU <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbidityNTU))==as.character(as.numeric("NA"))),NA,dt1$TurbidityNTU))
dt1$TurbidityFNU <- ifelse((trimws(as.character(dt1$TurbidityFNU))==trimws("NA")),NA,dt1$TurbidityFNU)               
suppressWarnings(dt1$TurbidityFNU <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbidityFNU))==as.character(as.numeric("NA"))),NA,dt1$TurbidityFNU))
dt1$Secchi <- ifelse((trimws(as.character(dt1$Secchi))==trimws("NA")),NA,dt1$Secchi)               
suppressWarnings(dt1$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Secchi))==as.character(as.numeric("NA"))),NA,dt1$Secchi))
dt1$Secchi_estimated <- as.factor(ifelse((trimws(as.character(dt1$Secchi_estimated))==trimws("NA")),NA,as.character(dt1$Secchi_estimated)))
dt1$Tow_duration <- ifelse((trimws(as.character(dt1$Tow_duration))==trimws("NA")),NA,dt1$Tow_duration)               
suppressWarnings(dt1$Tow_duration <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Tow_duration))==as.character(as.numeric("NA"))),NA,dt1$Tow_duration))
dt1$Tow_area <- ifelse((trimws(as.character(dt1$Tow_area))==trimws("NA")),NA,dt1$Tow_area)               
suppressWarnings(dt1$Tow_area <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Tow_area))==as.character(as.numeric("NA"))),NA,dt1$Tow_area))
dt1$Tow_volume <- ifelse((trimws(as.character(dt1$Tow_volume))==trimws("NA")),NA,dt1$Tow_volume)               
suppressWarnings(dt1$Tow_volume <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Tow_volume))==as.character(as.numeric("NA"))),NA,dt1$Tow_volume))
dt1$Cable_length <- ifelse((trimws(as.character(dt1$Cable_length))==trimws("NA")),NA,dt1$Cable_length)               
suppressWarnings(dt1$Cable_length <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Cable_length))==as.character(as.numeric("NA"))),NA,dt1$Cable_length))
dt1$Tow_direction <- as.factor(ifelse((trimws(as.character(dt1$Tow_direction))==trimws("NA")),NA,as.character(dt1$Tow_direction)))
dt1$Notes_tow <- as.factor(ifelse((trimws(as.character(dt1$Notes_tow))==trimws("NA")),NA,as.character(dt1$Notes_tow)))
dt1$Notes_flowmeter <- as.factor(ifelse((trimws(as.character(dt1$Notes_flowmeter))==trimws("NA")),NA,as.character(dt1$Notes_flowmeter)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Source)
summary(Station)
summary(Latitude)
summary(Longitude)
summary(Date)
summary(Datetime)
summary(Survey)
summary(Depth)
summary(SampleID)
summary(Method)
summary(Tide)
summary(Sal_surf)
summary(Sal_bot)
summary(Temp_surf)
summary(TurbidityNTU)
summary(TurbidityFNU)
summary(Secchi)
summary(Secchi_estimated)
summary(Tow_duration)
summary(Tow_area)
summary(Tow_volume)
summary(Cable_length)
summary(Tow_direction)
summary(Notes_tow)
summary(Notes_flowmeter) 
# Get more details on character variables

summary(as.factor(dt1$Source)) 
summary(as.factor(dt1$Station)) 
summary(as.factor(dt1$SampleID)) 
summary(as.factor(dt1$Method)) 
summary(as.factor(dt1$Tide)) 
summary(as.factor(dt1$Secchi_estimated)) 
summary(as.factor(dt1$Tow_direction)) 
summary(as.factor(dt1$Notes_tow)) 
summary(as.factor(dt1$Notes_flowmeter))
detach(dt1)               



inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/1075/2/5429d3e82b1671e7454c7b5d7a15c6ef" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "SampleID",     
                 "Taxa",     
                 "Length",     
                 "Count",     
                 "Notes_catch"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$SampleID)!="factor") dt2$SampleID<- as.factor(dt2$SampleID)
if (class(dt2$Taxa)!="factor") dt2$Taxa<- as.factor(dt2$Taxa)
if (class(dt2$Length)=="factor") dt2$Length <-as.numeric(levels(dt2$Length))[as.integer(dt2$Length) ]               
if (class(dt2$Length)=="character") dt2$Length <-as.numeric(dt2$Length)
if (class(dt2$Count)=="factor") dt2$Count <-as.numeric(levels(dt2$Count))[as.integer(dt2$Count) ]               
if (class(dt2$Count)=="character") dt2$Count <-as.numeric(dt2$Count)
if (class(dt2$Notes_catch)!="factor") dt2$Notes_catch<- as.factor(dt2$Notes_catch)

# Convert Missing Values to NA for non-dates

dt2$Length <- ifelse((trimws(as.character(dt2$Length))==trimws("NA")),NA,dt2$Length)               
suppressWarnings(dt2$Length <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Length))==as.character(as.numeric("NA"))),NA,dt2$Length))

if (!dir.exists("data")) dir.create("data")

dt1 |> 
  dplyr::mutate(Year = lubridate::year(Date),
                Month = lubridate::month(Date),
                SourceStation = paste(Source, Station),
                LatRound = round(Latitude, 1),
                LonRound = round(Longitude, 1)) |> 
  saveRDS(file.path("data", "dt1.rds"))

sources = levels(dt1$Source)

for (i in sources){
  tmp = dplyr::filter(dt1, Source == i)
  dt2 |> 
    dplyr::filter(SampleID %in% unique(tmp$SampleID)) |> 
    saveRDS(file.path("data", paste0("dt2-", gsub(" ", "", i), ".rds")))
}

if (Sys.getenv("EgnyteKey") != ""){
  remote_path = file.path("Shared", "Admin", "Practices", "Fish and Aquatic Science",
                          "Data Science", "EDI-SFE-Data")
  
  for (i in list.files("data")){
    Sys.sleep(0.4)
    # install egnyter with remotes::install_github("thinkelman-esa/egnyter")
    egnyter::upload_file(file.path("data", i),
                         file.path(remote_path, i), 
                         domain = "https://oneesa.egnyte.com",
                         token = Sys.getenv("EgnyteKey"))
  }
}

