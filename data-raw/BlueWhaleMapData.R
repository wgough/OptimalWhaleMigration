## code to prepare datasets related to blue whale map figure go here

#### Load Libraries and Functions ####
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
pkgTest("tidyverse")
pkgTest("ncdf4")
pkgTest("chron")
pkgTest("lattice")
pkgTest("RColorBrewer")
pkgTest("oce")
pkgTest("raster")
pkgTest("ggpubr")
pkgTest("lubridate")
pkgTest("rnaturalearth")
pkgTest("maptools")
pkgTest("hms")
# for the BW data
pkgTest("geosphere")
# pkgTest("tagtools")
pkgTest("fossil")
pkgTest("ggmap")
pkgTest("maps")
pkgTest("rnaturalearthdata")
pkgTest("sf")
pkgTest("ggspatial")
#pkgTest("marmap")
pkgTest("mapdata")
pkgTest("metR")
pkgTest("rgdal")
pkgTest("stats")
# pkgTest("plotly")
pkgTest("adehabitatHR")
pkgTest("move")
pkgTest("lwgeom")
pkgTest("viridis")
pkgTest("RcppRoll")
pkgTest("signal")
pkgTest("plotly")
# pkgtest("cowplot")

pkgTest("R.matlab")
pkgTest("rstudioapi")
# Helper functions specific to this analysis
`%notin%` <- Negate(`%in%`)
# Finds Sunrise and sunset times
source("R/find_Astronomical.R")
# Finds Point to Point metrics
source("R/pt2pt_fxns.R")
# Finds Vertical Velocity
source("R/tagtools_fxns.R")
# pkgTest("metR") # note: getNOAA.bathy was not working, but an alternate version on github is used here
source('R/getNOAAbathy.R')

#### Global Variables ####
# Note: this file makes assumptions of whaleNames
dep_metadata <- read.csv(paste0(getwd(),"./data-raw/BlueWhaleDeploymentMetadata.csv") , sep=",",header=TRUE)
withinTime <- 5 # Number of minutes to allow between location time and start of a dive
minDiveDepth <- 10 # depth in meters to be considered a dive
tzOffset <- "Etc/GMT+7"
#### Import and Process Whale Data ####
# filename <- file.choose()
filename <- "./data-raw/BlueWhaleData/Bm190916-TDR14 1HzprhSC.mat"
species <- substr(basename(filename),1,2) # extract the species
pathChoice <- dirname(filename)
# This imports all GPS.csv files in the directory chosen
filenames <- list.files(path = pathChoice, pattern = "* 1HzprhSC.mat", all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = TRUE)
# Create an empty dataframe to hold all the 1 hz data for all animals
dataset1hz = data.frame()
# Create an empty dataframe to hold all the dive data for all animals
diveDataset = data.frame()
# Create an empty dataframe to hold all the location data for all animals
locDataset = data.frame()
# Create an empty dataframe to hold all the location data for all animals
lungeDataset = data.frame()
# Create an empty dataframe to hold all the location data for all animals
callDataset = data.frame()
# Iteratively import files from each deployment and create columns for sorting and processing.
for(i in 1:length(filenames)){
  ## Import Dive Data ##
  # 1Hz prh exported from matlab
  prh1hzData <- readMat(filenames[i])
  prh1hzData_DF <- data.frame(p = prh1hzData$p, DN = prh1hzData$DN, speed = prh1hzData$speedJJ,
                              LL = prh1hzData$Light, Temp = prh1hzData$T, tagon = prh1hzData$tagon)
  # determine deployment ID from filename (used throughout)
  depid <- str_split(basename(filenames[i])," 1HzprhSC.mat")[[1]][1]
  prh1hzData_DF$depid <- depid
  cat(paste0("\nProcessing: ",  depid))
  # find tz_Offset, region and location (from external file)
  # tzOffset <- dep_metadata %>% dplyr::filter(ID == depid) %>% .$tzOffset %>% as.character()
  prh1hzData_DF$tzOffset <- tzOffset
  prh1hzData_DF$region <- dep_metadata %>% dplyr::filter(ID == depid) %>% .$Region %>% as.character()
  prh1hzData_DF$location <- dep_metadata %>% dplyr::filter(ID == depid) %>% .$Location %>% as.character()
  # Create proper datetime fields (dttz is the datetime in local time)
  ## print fractional seconds
  options(digits.secs=2)
  prh1hzData_DF$dttz <- round_date(as.POSIXct((prh1hzData_DF$DN - 719529)*86400, origin = "1970-01-01", tz = 'GMT'),unit="second")
  #JAF# Make sure this forcing is consistent between Acousondes and TDR10s
  prh1hzData_DF$dttz <- force_tz(prh1hzData_DF$dttz, tzOffset)
  # attr(prh1hzData_DF$dttz, "tzone")

  # make a UTC Datetime column
  prh1hzData_DF$dt <- prh1hzData_DF$dttz
  attr(prh1hzData_DF$dt, "tzone") <- "GMT" # Set TZ to GMT
  # Reorder the columns
  prh1hzData_DF <- prh1hzData_DF %>% dplyr::select(depid,dttz,dt, everything(),-DN)
  # Check to see if a corrected depth was included in the prhfile
  if(exists("Depth",prh1hzData)){
    prh1hzData_DF$p_Orig <- prh1hzData_DF$p
    pnew <- data.frame(p=prh1hzData$Depth)
    prh1hzData_DF$p <- round(pnew$p,1)
    rm(pnew)
  }
  # Determine Tagon and Tagoff times (GMT) basd on the sensor data
  tagonT <- min(prh1hzData_DF$dt[prh1hzData_DF$tagon==1])
  tagoffT <- max(prh1hzData_DF$dt[prh1hzData_DF$tagon==1])
  cat(paste0("\nTagon GMT: ",  tagonT))
  cat(paste0("\nTagoff GMT: ",  tagoffT))

  ## Dives and Surfacings (dives >10m) ##
  # find_dives in tagtools returns 4 values per dive
  # startI, endI, maxDepth, maxDepthI
  dives <- do.call(cbind.data.frame, prh1hzData$dives)
  colnames(dives) <- c('startI', 'endI', 'maxDepth', 'maxDepthI')
  #determine datetime of dives
  dives$dtStart <- prh1hzData_DF$dt[dives$startI]
  dives$dtEnd <- prh1hzData_DF$dt[dives$endI]
  dives$dttzStart <- prh1hzData_DF$dttz[dives$startI]
  dives$dttzEnd <- prh1hzData_DF$dttz[dives$endI]
  dives$DiveNum <- seq.int(nrow(dives))
  dives <- dives %>%
    # dive duration in min
    mutate(dive_Duration = (endI-startI)/60,
           depid = depid)
  dives$region <- dep_metadata %>% dplyr::filter(ID == depid) %>% .$Region %>% as.character()
  dives$location <- dep_metadata %>% dplyr::filter(ID == depid) %>% .$Location %>% as.character()

  # findsurfs2 in tagtools returns 2 variables
  # startI, endI (NOTE: these are post-dive surfacings)
  surfs <- as.data.frame(prh1hzData$surfs)
  colnames(surfs) <- c('surf_startI', 'surf_endI')
  surfs <- surfs %>%
    mutate(surf_Duration = (surf_endI-surf_startI)/60) # surface duration in min
  # determine datetime of surfacings
  surfs$surf_dtStart <- prh1hzData_DF$dt[surfs$surf_startI]
  surfs$surf_dtEnd <- prh1hzData_DF$dt[surfs$surf_endI]
  # depending on tag type, pre-deployment time may be identified as a surfacing
  if(length(surfs$surf_startI)>length(dives$DiveNum)){
    surfs <- surfs[-c(1),]
  }
  surfs$surf_DiveNum <- seq.int(nrow(surfs))
  # join Surface interval to dives
  dives <- left_join(dives,surfs, by = c("DiveNum"= "surf_DiveNum"))
  # Reorder the columns
  dives <- dives %>% dplyr::select(depid, DiveNum, region, location, everything())


  ## Import Movement Data ##
  # Note: The location nearest to the dive start time is selected,
  # but the search time for positions is 10 min, meaning some locations
  # could be associated with multiple dives. 10 min seems to be a reasonable span
  fnLocs <- paste0(str_split(filenames[i]," 1HzprhSC.mat")[[1]][1],"-GPS.csv" )#file.choose()
  whaleLocs <- read_csv(fnLocs,col_names=TRUE,cols(`...1` = col_double(),DateTimeUTC = col_datetime(format = ""),
                                                   Lat = col_double(),Long = col_double(),lc = col_double()))
  # check that dt imported properly to GMT time
  if(is.null(attr(whaleLocs$DateTimeUTC, "tzone"))){
    whaleLocs$DateTimeUTC <- as.POSIXct(strptime(whaleLocs$DateTimeUTC,format="%m/%d/%Y %H:%M:%S"), tz = 'GMT')
  }
  whaleLocs$locNum <- seq(1:length(whaleLocs$DateTimeUTC)) # create a column for joining later
  whaleLocs <- subset(whaleLocs, select = -c(1, lc)) # remove unused columns
  whaleLocs$depid <- depid
  # find tz_Offset
  #tzOffset <- dep_metadata %>% dplyr::filter(ID == depid) %>% .$tzOffset %>% as.character()
  whaleLocs$tzOffset <- tzOffset
  # make a Local Datetime column
  whaleLocs$dttz <- whaleLocs$DateTimeUTC
  attr(whaleLocs$dttz, "tzone") <- tzOffset # Set TZ to Local
  # filter to include positions that overlap with dive data (tagonT, tagoffT)
  whaleLocs <- whaleLocs %>%
    dplyr::filter(DateTimeUTC >= tagonT - minutes(15),
                  DateTimeUTC <= tagoffT + minutes(15))
  ## Calculate time differences between locations (lead and lag)
  whaleLocs <- whaleLocs %>%
    arrange(DateTimeUTC) %>%
    # time difference between consecutive points
    mutate(time_Diff_lag = pt2pt.back.duration(dttz,output.units='mins'),
           time_Diff_lead = pt2pt.duration(dttz,output.units='mins'))
  # Reorder the columns
  whaleLocs <- whaleLocs %>% dplyr::select(depid, locNum,dttz, everything())
  # Attach GPS Data to Dives
  dives$Lat <- NA
  dives$Long <- NA
  dives$locNum <- NA # To be used later as a unique identifier
  dives$locClosest <- NA # Nearest location number
  dives$locTimeDif <- NA # Time diff in minutes to the nearest location
  dives$knownPosGap <- NA # Gap between lead and lag positions

  # Find locations withinTime minutes of the start of the dive
  # Note: Can be before or after start of dive (since positions can occur on Initial surfacing
  # as well and terminal dives)
  for(k in 1:dim(dives)[1]){
    temp<- whaleLocs %>%
      # filter locations withinTime min, before the start of the dive
      dplyr::filter(dives$dtStart[k] >= min(whaleLocs$DateTimeUTC),
                    abs(difftime(dives$dtStart[k],
                                 whaleLocs$DateTimeUTC, units = "secs")) <= (60*withinTime)) %>%
      # calculate the time difference and arrange smallest first
      mutate(tDif = abs(as.numeric(difftime(dives$dtStart[k], .$DateTimeUTC, units = "secs")/60))) %>%
      arrange(tDif)
    if(dim(temp)[1] != 0){
      # select the location nearest to the dive start time
      dives$Lat[k] <- temp$Lat[1]
      dives$Long[k] <- temp$Long[1]
      dives$locNum[k] <- as.character(temp$locNum[1])
      dives$locClosest[k] <- as.character(temp$locNum[1])
      dives$locTimeDif[k] <- temp$tDif[1]
      if(dives$dtStart[k] > temp$DateTimeUTC){
        dives$knownPosGap[k] <- temp$time_Diff_lead[1]
      }else{
        dives$knownPosGap[k] <- temp$time_Diff_lag[1]
      }

    }
  }
  rm(k)
  dives <- dives %>%
    group_by(locNum) %>%
    arrange(locTimeDif) %>%
    mutate(locRep = if_else(n()==1 ,0,1),
           locPriority = seq(1,n(),by=1)) %>%
    ungroup()   %>%
    arrange(dttzStart)
  dives$locRep[is.na(dives$Lat)] <- as.numeric(NA)
  dives$locPriority[is.na(dives$Lat)] <- as.numeric(NA)

  # This cleans up Lat/Longs for cases where 2 dives used the same location (i.e. both withinTime)
  dives <- dives %>%
    mutate(Lat = if_else(!is.na(locRep) & locRep==1 & locPriority > 1,as.numeric(NA),Lat),
           Long = if_else(!is.na(locRep) & locRep==1 & locPriority >1,as.numeric(NA),Long)) %>%
    dplyr::select(-locRep)
  dives$locNum[is.na(dives$Lat)] <- as.character(NA)
  # red plot should look like the deployment map (grey)
  # some segments will be missing because dives are a subset (i.e. >10m)
  plot(whaleLocs$Long,whaleLocs$Lat, col="grey",type="l",
       ylim=c(min(whaleLocs$Lat),max(whaleLocs$Lat)),
       xlim=c(min(whaleLocs$Long),max(whaleLocs$Long)),
       xlab="Longitude",ylab="Latitude")
  points(dives$Long,dives$Lat,col="red",pch = 20)
  cat(paste0("\n# Dives: ",  length(dives$depid)))
  cat(paste0("\n# With Locations: ",  sum(!is.na(dives$Lat))))
  ## Calculate Distance and Speed Metrics (only for dives with locations)
  diveDistMetrics <- dives %>%
    dplyr::filter(!is.na(locNum)) %>%
    # differences between consecutive points (backward)
    mutate(diveTimeDif_locs = pt2pt.back.duration(dttzStart,output.units='mins'),
           diveDist_locs = pt2pt.back.distance(Lat,Long)/1000, # Dist in KM
           diveSPD_locs = pt2pt.speed(diveDist_locs,diveTimeDif_locs/60), # KM/hr
           diveDIR_locs = pt2pt.back.direction(Lat,Long)) # degrees
  # join metrics to dives
  dives <- left_join(dives,dplyr::select(diveDistMetrics,DiveNum,
                                         diveTimeDif_locs,diveDist_locs, diveSPD_locs, diveDIR_locs), by = "DiveNum")
  rm(diveDistMetrics)

  ## Load Lunges ##
  ## Select the lunges.mat file
  lungeFile <- paste0(str_split(filenames[i]," 1HzprhSC.mat")[[1]][1]," lunges.mat" )#file.choose()
  lunges = readMat(lungeFile)
  lungeDT <- round_date(as.POSIXct((lunges$LungeDN - 719529)*86400, origin = "1970-01-01", tz = 'GMT'),unit = "seconds")
  lunge_dttz  <- force_tz(lungeDT,tzone=tzOffset) # Force TZ
  # attr(lunge_dttz, "tzone") #Check tz
  lungeDF <- as.data.frame(lunge_dttz)
  lungeDF$lunge_dt <- lungeDF$lunge_dttz
  attr(lungeDF$lunge_dt, "tzone") <- 'GMT'
  lungeDF$lungeDepth <- lunges$LungeDepth
  lungeDF$lungeC <- lunges$LungeC
  rm(lunge_dttz, lungeDT)
  # Select only High-Confidence Lunges Detected
  lungeDF <- lungeDF %>%
    dplyr::filter(lungeC == 3) %>%
    dplyr::select(-lungeC) # remove unneeded columns
  lungeDF$lungeNum <- seq(1:length(lungeDF$lunge_dttz)) # create a column for joining later
  lungeDF$depid <- depid
  # Reorder the columns
  lungeDF <- lungeDF %>% dplyr::select(depid, lungeNum,lunge_dttz, everything())
  # Find Lunge Temps and LL
  lungeDF$LungeLL <- NA
  lungeDF$LungeTemp <- NA
  lungeDF$LungeLLmax10s <- as.numeric(NA)
  for(ii in 1:length(lungeDF$lunge_dttz)){
    ptemp <- prh1hzData_DF %>%
      dplyr::filter(dt == lungeDF$lunge_dt[ii]) %>%
      dplyr::select(LL, Temp)
    lungeDF$LungeLL[ii] <- ptemp$LL
    lungeDF$LungeTemp[ii] <- ptemp$Temp
    lungeDF$LungeLLmax10s[ii] <- prh1hzData_DF %>%
      dplyr::filter(dt >= lungeDF$lunge_dt[ii]-seconds(5),
                    dt < lungeDF$lunge_dt[ii]+seconds(5)) %>%
      summarize(max(LL)) %>% as.numeric
    rm(ptemp)
  }
  rm(ii)

  # Attach Foraging Data to Dives
  dives$LungeCount <- 0
  dives$meanLungeDepth <- NA
  dives$sdLungeDepth <- NA
  dives$meanLungeLL <- NA
  dives$meanLungeTemp <- NA
  dives$sdLungeTemp <- NA
  dives$meanILI <- NA # Inter Lunge Interval
  dives$sdILI <- NA

  # Find lunges between start and end of dive and
  for(k in 1:dim(dives)[1]){
    # Attach Lunges to Dives
    temp <- lungeDF %>%
      dplyr::filter(lunge_dt >=  dives$dtStart[k] & lunge_dt <=  dives$dtEnd[k])
    if(dim(temp)[1] != 0){
      dives$LungeCount[k] <- dim(temp)[1]
      dives$meanLungeDepth[k] <- mean(temp$lungeDepth)
      dives$sdLungeDepth[k] <- sd(temp$lungeDepth)
      dives$meanLungeLL[k] <- mean(temp$LungeLL)
      dives$meanLungeTemp[k] <- mean(temp$LungeTemp)
      dives$sdLungeTemp[k] <- sd(temp$LungeTemp)
      # Inter Lunge Interval (only on dives with more than 1 lunge)
      if(dim(temp)[1] > 0){
        dives$meanILI[k] <- mean(pt2pt.back.duration(temp$lunge_dt,output.units='secs'),na.rm=TRUE)
        dives$sdILI[k] <- sd(pt2pt.back.duration(temp$lunge_dt,output.units='secs'),na.rm=TRUE)
      }
    }
    rm(temp)
  }
  rm(k)
  # make sure 1 lunge dives have a SD of 0
  dives$sdLungeDepth[dives$lungeCount==1] <- 0

  # Attach Dive Metrics to Dives
  dives$bottomPhaseStartdttz <- as.POSIXct(NA)
  dives$bottomPhaseEnddttz <- as.POSIXct(NA)
  dives$bottomPhase <- NA # length in seconds of Bottom Phase of dive
  dives$descentRate <- NA # m/s, start of dive to start of bottom phase
  dives$descentRateSD <- NA
  dives$ascentRate <- NA # m/s, end of bottom phase to end of dive
  dives$ascentRateSD <- NA
  # Calculate Bottom Phase and Ascent/Descent Rate
  for(k in 1:dim(dives)[1]){
    # Determine Bottom Phase Start and End
    temp <- prh1hzData_DF %>%
      dplyr::filter(dt >=  dives$dtStart[k] & dt <=  dives$dtEnd[k])
    temp$pSmooth <- RcppRoll::roll_mean(temp$p,n=10,fill = NA)
    temp$pSmoothDif <- c(NA,diff(temp$pSmooth))
    botS <- temp %>%
      dplyr::filter(pSmooth>minDiveDepth,pSmoothDif<0) %>%
      summarize(botS = first(dttz)) %>% .$botS
    botE <- temp %>%
      dplyr::filter(pSmooth>minDiveDepth,pSmoothDif>0) %>%
      summarize(botE = last(dttz)) %>% .$botE
    # plot(temp$dttz,temp$pSmooth)
    # points(botS,10,col="red")
    # points(botE,10,col="blue")

    # Excludes shallow and v shaped dives
    if(!is.na(botS) & !is.na(botE) & botS<botE){
      depRate <- depth_rate(temp$p, 1)
      dives$bottomPhaseStartdttz[k] <- botS
      dives$bottomPhaseEnddttz[k] <- botE
      dives$bottomPhase[k] <- as.numeric(difftime(botE,botS, units = "secs")/60)
      dives$descentRate[k] <- mean(depRate[temp$dttz<botS],na.rm=TRUE)
      dives$descentRateSD[k] <- sd(depRate[temp$dttz<botS],na.rm=TRUE)
      dives$ascentRate[k] <-abs(mean(depRate[temp$dttz>botE],na.rm=TRUE))
      dives$ascentRateSD[k] <- sd(depRate[temp$dttz>botE],na.rm=TRUE)
      rm(depRate)
    }
    rm(temp,botS,botE)
  }
  rm(k)

  ## Load Calls ##
  ## Select the lunges.mat file
  if(depid == "Bm190916-TDR14"){
    callFile <- paste0(str_split(filenames[i]," 1HzprhSC.mat")[[1]][1],"_32HzAcc_calls.mat" )#file.choose()
    calls = readMat(callFile)
    callDT <- round_date(as.POSIXct((calls$callDN - 719529)*86400, origin = "1970-01-01", tz = 'GMT'),unit = "seconds")
    call_dttz  <- force_tz(callDT,tzone=tzOffset) # Force TZ
    # attr(lunge_dttz, "tzone") #Check tz
    callDF <- as.data.frame(call_dttz)
    callDF$call_dt <- callDF$call_dttz
    attr(callDF$call_dt, "tzone") <- 'GMT'
    rm(calls,callDT,call_dttz, callFile)
    callDF$callNum <- seq(1:length(callDF$call_dttz)) # create a column for joining later
    callDF$depid <- depid
    # Reorder the columns
    callDF <- callDF %>% dplyr::select(depid, callNum,call_dttz, everything())
    # Find Call Depths
    callDF$callDepth <- NA
    for(ii in 1:length(callDF$call_dttz)){
      ptemp <- prh1hzData_DF %>%
        dplyr::filter(dttz == callDF$call_dttz[ii]) %>%
        dplyr::select(p)
      callDF$callDepth[ii] <- ptemp$p
      rm(ptemp)
    }
    rm(ii)
  }else{
    callDF <- data.frame(call_dt=as.POSIXct(NA))
  }

  # Attach Foraging Data to Dives
  dives$CallCount <- 0
  dives$meanCallDepth <- NA
  dives$sdCallDepth <- NA
  dives$meanICI <- NA # Inter Call Interval
  dives$sdICI <- NA

  # Find calls between start and end of dive and
  for(k in 1:dim(dives)[1]){
    # Attach Calls to Dives
    temp <- callDF %>%
      dplyr::filter(call_dt >=  dives$dtStart[k] & call_dt <=  dives$dtEnd[k])
    if(dim(temp)[1] != 0){
      dives$CallCount[k] <- dim(temp)[1]
      dives$meanCallDepth[k] <- mean(temp$callDepth)
      dives$sdCallDepth[k] <- sd(temp$callDepth)
      # Inter Call Interval (only on dives with more than 1 Call)
      if(dim(temp)[1] > 0){
        dives$meanICI[k] <- mean(pt2pt.back.duration(temp$call_dt,output.units='secs'),na.rm=TRUE)
        dives$sdICI[k] <- sd(pt2pt.back.duration(temp$call_dt,output.units='secs'),na.rm=TRUE)
      }
    }
    rm(temp)
  }
  rm(k)
  # make sure 1 lunge dives have a SD of 0
  dives$sdCallDepth[dives$CallCount==1] <- 0

  dives %>%
    dplyr::filter(LungeCount>0) %>%
    summarize(Foraging = n(),
              Not_Foraging = length(dives$DiveNum)-n())
  # Save individual data to overall dataset
  if(i==1){
    dataset1hz <- prh1hzData_DF # save the 1Hz data
    diveDataset <- dives # save the dives
    locDataset <- whaleLocs
    lungeDataset <- lungeDF
    callDataset <- callDF
  } else{
    dataset1hz <- rbind(dataset1hz,prh1hzData_DF)
    diveDataset <- rbind(diveDataset,dives)
    locDataset <- rbind(locDataset,whaleLocs)
    lungeDataset <- rbind(lungeDataset,lungeDF)
  }
  rm(lungeDF, prh1hzData_DF, prh1hzData)
  rm(tagonT, tagoffT, lunges, depid)
}
# Cleanup
rm(lungeFile,pathChoice,i, surfs, dives, whaleLocs, filename,filenames, fnLocs)
# Clip 1Hz data to Tagon
dataset1hz <- dataset1hz %>%
  dplyr::filter(tagon ==1)

## Make sure the spatial data look right
#b = getNOAA.bathy(lon1 = min(diveDataset$Long-3,na.rm=TRUE), lon2 = max(diveDataset$Long+3,na.rm=TRUE), lat1 =max(diveDataset$Lat+3,na.rm=TRUE), lat2 = min(diveDataset$Lat-3,na.rm=TRUE),resolution = 1)
# convert bathymetry to data frame
#bf = fortify.bathy(b)
load(file="data-raw/bf.Rdata")
world <- ne_countries(scale = "large", returnclass = "sf")

# Determine Migration Start time         --------------------------------------------
# Add 1 day to the end of the last feeding bout

# Bm190916-TDR14 has a period without any feeding between 09-22 13:40 and 09-29 20:36
lungeDataset %>% filter(depid=="Bm190916-TDR14")
# Bm181021-TDR11 ceases feeding entirely on 10-30
lungeDataset %>% filter(depid=="Bm181021-TDR11") %>%
  summarize(last(lunge_dttz))
# Save the start/end times
migrationStarts <- data.frame(depid = c("Bm181021-TDR11","Bm190916-TDR14"),
                              migStart = c(as.POSIXct("2018-10-30 14:42:34",tz=tzOffset)+days(1), # Start for Bm181021-TDR11
                                           as.POSIXct("2019-09-22 13:40:36",tz=tzOffset)+days(1)),# Start for Bm190916-TDR14
                              migEnd = c(as.POSIXct("2018-11-22 10:44:44",tz=tzOffset), # End for Bm181021-TDR11
                                         as.POSIXct("2019-09-29 20:36:31",tz=tzOffset)))# End for Bm190916-TDR14
class(migrationStarts$migStart)
# Determine Overground Migration Speed   --------------------------------------------
# Create a location subset for each individual
locDataset_tag11 <- locDataset %>%
  dplyr::filter(depid == "Bm181021-TDR11",
                dttz>=migrationStarts$migStart[migrationStarts$depid=="Bm181021-TDR11"])
locDataset_tag14 <- locDataset %>%
  dplyr::filter(depid == "Bm190916-TDR14",
                dttz >= migrationStarts$migStart[migrationStarts$depid=="Bm190916-TDR14"],
                dttz < migrationStarts$migEnd[migrationStarts$depid=="Bm190916-TDR14"])
locDatasetMigration <- rbind(locDataset_tag11,locDataset_tag14)

locDatasetMigrationSummary <- locDatasetMigration %>%
  group_by(depid) %>%

  summarize(
    # Calculate total overground distance for each individual (Straight-line)
    DistTravelled = geodDist(longitude1 = first(Long),
                             latitude1 = first(Lat),
                             longitude2 = last(Long),
                             latitude2 = last(Lat))*1000,
    # Calculate total overground distance for each individual (With Sub-points)
    DistTravelledAlongPath = last(geodDist(longitude1 = Long,
                                           latitude1 = Lat,
                                           alongPath =TRUE)*1000),
    migrationPeriod = difftime(last(dttz),first(dttz), units="secs"),
    StraightLineSpeed = DistTravelled/as.numeric(migrationPeriod),
    AlongPathSpeed = DistTravelledAlongPath/as.numeric(migrationPeriod))



# Calculate total overground distance for each individual
d_Tag11 <- geodDist(
  longitude1=locDataset_tag11$Long[1],
  latitude1 =locDataset_tag11$Lat[1],
  longitude2 = locDataset_tag11$Long[length(locDataset_tag11$Long)],
  latitude2 = locDataset_tag11$Lat[length(locDataset_tag11$Lat)])*1000 # convert to meters
dAlongPath_Tag11 <- last(geodDist(
  longitude1=locDataset_tag11$Long,
  latitude1 =locDataset_tag11$Lat,
  alongPath =TRUE)*1000)

t_Tag11 <- difftime(locDataset_tag11$dttz[length(locDataset_tag11$dttz)],
                    locDataset_tag11$dttz[1], units="secs")
d_Tag11/as.numeric(t_Tag11)

d_Tag14 <- geodDist(
  longitude1=locDataset_tag14$Long[1],
  latitude1 =locDataset_tag14$Lat[1],
  longitude2 = locDataset_tag14$Long[length(locDataset_tag14$Long)],
  latitude2 = locDataset_tag14$Lat[length(locDataset_tag14$Lat)])*1000
t_Tag14 <- difftime(locDataset_tag14$dttz[length(locDataset_tag14$dttz)],
                    locDataset_tag14$dttz[1], units="secs")
d_Tag14/as.numeric(t_Tag14)

# Determine 1Hz Swimming Speed           --------------------------------------------
dataset1hz_tag11 <- dataset1hz %>%
  dplyr::filter(depid == "Bm181021-TDR11",
                dttz>=migrationStarts$migStart[migrationStarts$depid=="Bm181021-TDR11"])
dataset1hz_tag14 <- dataset1hz %>%
  dplyr::filter(depid == "Bm190916-TDR14",
                dttz >= migrationStarts$migStart[migrationStarts$depid=="Bm190916-TDR14"],
                dttz < migrationStarts$migEnd[migrationStarts$depid=="Bm190916-TDR14"])
dataset1hz_tag11 %>%
  dplyr::filter(p>2) %>%
  summarize(mean(speed,na.rm=TRUE),sd(speed,na.rm=TRUE))
dataset1hz_tag14 %>%
  dplyr::filter(p>2) %>%
  summarize(mean(speed,na.rm=TRUE),sd(speed,na.rm=TRUE))

usethis::use_data(world, overwrite = TRUE)
usethis::use_data(bf, overwrite = TRUE)
usethis::use_data(diveDataset, overwrite = TRUE)
usethis::use_data(locDatasetMigration, overwrite = TRUE)
usethis::use_data(dataset1hz_tag11, overwrite = TRUE)
usethis::use_data(dataset1hz_tag14, overwrite = TRUE)

# Map Plot w/ Only Migration Portions
pMap <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(min(diveDataset$Long,na.rm = TRUE)-.5, max(diveDataset$Long,na.rm = TRUE)+.5),
           ylim = c(min(diveDataset$Lat,na.rm = TRUE)-.5, max(diveDataset$Lat,na.rm = TRUE)+.5), expand = FALSE) +
  # add 500m contour
  geom_contour(data = bf,
               aes(x=x, y=y, z=z),
               breaks=c(-500),
               size=c(0.4),
               colour="darkgrey", show.legend = FALSE) +
  #geom_text_contour(data = bf, aes(x=x, y=y,z = z),breaks=c(-100,-200,-300,-400),
  #geom_text_contour(data = bf, aes(x=x, y=y,z = z),breaks=c(-500),
  #                  show.legend = FALSE, size = 2.2, alpha = .6, nudge_y = -.002) +
  geom_path(data = locDatasetMigration, aes(Long,Lat, color = depid),size=1) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic(base_size = 8) +
  theme(axis.text = element_text(size = 24),
        axis.title = element_text(size = 30),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.2),
        panel.background = element_rect(fill = "aliceblue"))
pMap

# Depth Plot w/ Only Migration Portions
p3 <- ggplot(data = dataset1hz_tag11) +
  geom_path(aes(x=dttz, y=-p))
p3


