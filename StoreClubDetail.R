library(plyr)
library(RODBC)

## This script connects to the latest Swimming db on this computer
        ch <- odbcConnect("Swimming")
# This closes the db connection:
         
## read1 <- read.csv(file="C:/Users/Admin/Dropbox/SwimData/DataSet1/Meet Results-Sa Short Course Swimming Champs (25m) PMBG-07Aug2014-002/MRSASCSC.csv")
        read1 <- read.delim(file="C:/Users/Admin/Dropbox/SwimData/DataSet1/Meet Results-Sa Short Course Swimming Champs (25m) PMBG-07Aug2014-002/MRSASCSC.csv", header = FALSE, sep = "\t", dec = ".")



#IDvector <- lapply(read1$v1, getlineID)
#Apply getLineID to the whole dataframe that is in the file:
IDvector <- lapply(as.vector(read1)[,1], getlineID)
#The unlisted ID Vector
uID<-unlist(IDvector)
# All Clubs
        allClubs <- (substr(uID, 1, 3)=="C18")
# Number of Unique clubs
        nClubs = sum(allClubs)
## emptyClubs <- vector(mode="character", length=length(uID))
# ClubName Vector is an index vector where the index points to the index 
# in the read table which contains the name of the vector
# that has the club name for each single event attached        
        ClubNameVIndex <- most.recent(allClubs)
# ClubNameVector is now the vector that has club names for each event:
        ClubNameVector <- substr(read1[,1],12,16)[ClubNameVIndex]

        ClubTable <- as.character(read1[allClubs,])
        ClubList <- lapply(ClubTable, getClubDetail)
        ClubDetail <- ldply(ClubList, data.frame)

        sqlSave(ch, ClubDetail, append = FALSE,
                colnames = FALSE,rownames = FALSE)
 
odbcClose(ch) 


# All the swimmers:
allSwimmers<-uID[nchar(uID)>7]

bDays <- substr(allSwimmers, 3, 8)
# All Events
allEvents <- uID[nchar(uID)==5]
#All D Events
allDEvents <- substr(uID, 1, 3)=="D08"
DTable <- as.character((read1[allDEvents,]))
# The following creates a list of data frames

gEN <- lapply(DTable, getEventDetailX)
# The folloowing game changer applies the data.frame function to
# EACH data frame listed in gEN:
eventFrame <- ldply(gEN, data.frame)
eventFrame$ClubCode <- ClubNameVector[allDEvents]
#All G Events
allGEvents <- substr(uID, 1, 3)=="G08"
#All Swimming Events
allSEvents <- substr(uID, 1, 3)=="G08"
write.table(x=eventFrame, file="C:/Users/Admin/Dropbox/SwimData/DataSet1/Meet Results-Sa Short Course Swimming Champs (25m) PMBG-07Aug2014-002/eventFrame1.csv",sep=",")
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
read1[allClubs,]
# All Provinces
allProv <- (substr(allEvents, 4, 5))
uProv = unique(allProv)
nProv = length(uProv)
hist(allProv[nchar(uID)>7])
#EventString <- function(EventNo, Gender, Distance, Stroke, MultiClass)
ages <- (as.Date(format(Sys.time(), "%y%m%d"),"%y%m%d")-as.Date(substr(allSwimmers, 3, 8),"%y%m%d"))/365.25
