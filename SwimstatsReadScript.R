


## This script connects to the latest Swimming db on this computer
        ch <- odbcConnect("Swimming")

## read1 <- read.csv(file="C:/Users/Admin/Dropbox/SwimData/DataSet1/Meet Results-Sa Short Course Swimming Champs (25m) PMBG-07Aug2014-002/MRSASCSC.csv")
        read1 <- read.delim(file="C:/Users/Admin/Dropbox/SwimData/DataSet1/Meet Results-Sa Short Course Swimming Champs (25m) PMBG-07Aug2014-002/MRSASCSC.csv", header = FALSE, sep = "\t", dec = ".")


#IDvector <- lapply(read1$v1, getlineID)
#Apply getLineID to the whole dataframe that is in the file:
        IDvector <- lapply(as.vector(read1)[,1], getlineID)
#The unlisted ID Vector
        uID<-unlist(IDvector)
# All Clubs
        allClubs <- (substr(uID, 1, 3)=="C18")
        nClubs = sum(allClubs)
## emptyClubs <- vector(mode="character", length=length(uID))
# ClubName Vector is an index vector where the index points to the index 
# in the read table which contains the name of the vector
# that has the club name for each single event attached        
        ClubNameVIndex <- most.recent(allClubs)
# ClubNameVector is now the vector that has clob names for each event:
        ClubNameVector <- substr(read1[,1],12,16)[ClubNameVIndex]

# All the swimmers:
        allSwimmers<-uID[nchar(uID)>7]

        bDays <- substr(allSwimmers, 3, 8)
# All Events
        allEvents <- uID[nchar(uID)==5]
        #All D Events
                allDEvents <- substr(uID, 1, 3)=="D08"
                DTable <- as.character((read1[allDEvents,]))
# The following creates a list of data frames
        #this is a test for getEventDetailDB
        getFinalsEventDetailDB(DTable[[1]])
                                                
                gEN <- lapply(DTable, getEventDetailX)
                HeatsDB <- lapply(DTable, getHeatsEventDetailDB)
                FinalsDB <- lapply(DTable, getFinalsEventDetailDB)
# The following game changer applies the data.frame function to
# EACH data frame listed in gEN:
                eventFrame <- ldply(gEN, data.frame)
                HeatsEF <- ldply(HeatsDB, data.frame)
                FinalsEF <- ldply(FinalsDB, data.frame)
                eventFrame$ClubCode <- ClubNameVector[allDEvents]
                HeatsEF$ClubCode <- ClubNameVector[allDEvents]
                FinalsEF$ClubCode <- ClubNameVector[allDEvents]
                FinalsEF<-FinalsEF[FinalsEF$Event>0,]
                galaFrame<-rbind(HeatsEF,FinalsEF)
        
#-Old Code
#         #All G Events
#                 allGEvents <- substr(uID, 1, 3)=="G08"
#         #All Swimming Events
#                 allSEvents <- substr(uID, 1, 3)=="G08"

        eventFrame$MultiClass <- is.element(eventFrame$EventNumber,MCEventList)
        FinalsVector <- lapply(eventFrame$finalsSeconds, FinalsFunc)
        eventFrame$Finals <-unlist(FinalsVector)
        
        EventStringdf<-data.frame(eventFrame$EventNumber, eventFrame$Gender, eventFrame$Distance, eventFrame$Stroke, eventFrame$MultiClass)
        EventStringdf<-setNames(EventStringdf,c('EventNo', 'Gender', 'Distance', 'Stroke', 'MultiClass'))                          
 
#The following section of code adds various string identifiers 
#        to the data base for use to identify subsection of events.
        LonggEN <- apply(EventStringdf, 1, EventStringFunc)
        gEN <- apply(EventStringdf, 1, ShortEventStringFunc)
        gENstroke <- apply(EventStringdf, 1, strokeStringFunc)
        gENdistance <- apply(EventStringdf, 1, distanceStringFunc)      
        eventFrame$LongEventString <- LonggEN 
        eventFrame$EventString <- gEN 
        eventFrame$StrokeString <- gENstroke      
        eventFrame$DistanceString <- gENdistance
#       Here we will build a completeFrame for all avents - heats and finals
        
                heatFrame <-data.frame(eventFrame$Province,eventFrame$EventNumber,eventFrame$Swimmer)
                drops<-c("finalsTime",
                          "finalsSeconds",
                          "finalsEvent",
                          "finalsLane",
                          "finalsPosition")
                DF[,!(names(DF) %in% drops)]
#This gives the number of unique multiclass events        
        unique(eventFrame$EventString[eventFrame$MultiClass])    
        
write.table(x=eventFrame, file="C:/Users/Admin/Dropbox/SwimData/DataSet1/Meet Results-Sa Short Course Swimming Champs (25m) PMBG-07Aug2014-002/eventFrame1.csv",sep=",")
write.table(x=galaFrame, file="C:/Users/Admin/Dropbox/SwimData/DataSet1/Meet Results-Sa Short Course Swimming Champs (25m) PMBG-07Aug2014-002/galaFrame.csv",sep=",")
sqlDrop(ch, "eventFrame") 
sqlSave(ch, eventFrame, append = FALSE,
        colnames = FALSE,rownames = FALSE)
sqlDrop(ch, "galaFrame") 
        sqlSave(ch, galaFrame, append = FALSE,
                colnames = FALSE,rownames = FALSE)
        
odbcClose(ch)     

# All Clubs
        allClubs <- (substr(uID, 1, 3)=="C18")
        nClubs = sum(allClubs)
        ## emptyClubs <- vector(mode="character", length=length(uID))
# ClubName Vector is an index vector where the index points to the index 
# in the read table which contains the name of the vector
# that has the club name for each single event attached        
        ClubNameVIndex <- most.recent(allClubs)
# ClubNameVector is now the vector that has clob names for each event:
        ClubNameVector <- substr(read1[,1],12,16)[ClubNameVIndex]

# All Provinces
        allProv <- (substr(allEvents, 4, 5))
        uProv = unique(allProv)
        nProv = length(uProv)
## hist(allProv[nchar(uID)>7])
        #setNames(EventStringdf, c('EventNo', 'Gender', 'Distance', 'Stroke', 'MultiClass'))
        
ages <- (as.Date(format(Sys.time(), "%y%m%d"),"%y%m%d")-as.Date(substr(allSwimmers, 3, 8),"%y%m%d"))/365.25
        
        
        #This piece of code works out how big the circles in the presentation needs to be the maximum being 27mm:
        #Collect data based on number of swimmers by Province
                dF1<-aggregate(Swimmer~Province, SwimmersData, length)
        #Now sort on number of Swimmers:
                dF1<-dF1[with(dF1, order(Swimmer)), ]
        #Now recalibrate on a given acceptable size:
                dF1$Swimmer <- dF1$Swimmer/91*27
        dF1$Radius <- sqrt((dF1$Swimmer/27) * (27^2*pi) * (1/pi))  # dF1$Swimmer^2*pi
