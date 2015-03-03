# Run this script FIRST!

library(plyr)
library(RODBC)
library(ggplot2)
library(ggthemes)
library(outliers)
library("devtools")


# The best PRINTING of graphs is achieved by EXPORTING the file to .png format
# from Adobe Illustrator.

#It creates one file per ARTBOARD.


#       1. makeFootnote
#       2. makeHeadnote
#       3. most.recent
#       4. getlineID
#       5. getEventDetail
#       6. getEventDetailX
#       7. toSeconds
#       8. getClubDetail
#       9. StrokeID
#       10. GenderID
#       11. getSwimmersDetailX


#The following variable identifies the multi class event by event number:
MCEventList <-cbind(103,104,105,106,111,112,113,114,118,
                    119,120,121,201,202,203,204,210,211,
                    212,213,219,220,221,222,303,304,305,
                    306,312,313,314,315,403,404,405,406,
                    410,411,412,413)
##################################################################
##                                                              ##
##      This Script contains the libraries and functions        ##
##      used in this analysis                                   ##
##                                                              ##
##################################################################

# basic information at the beginning of each script
#scriptName <- "filename.R"
#author <- "mh"
#footnote <- paste(scriptName, format(Sys.time(), "%d %b %Y"),
#                 author, sep=" / ")

FootNoteALL <-"Meet Results-SA Short Course Swimming Champions PMBG 7-10 August 2014"
HeadNoteALL <-"Meet Results-SA Short Course Swimming Champions PMBG 7-10 August 2014"
ShortCode <- "SASCSC - PMBG 7-10 August 2014"
# default footnote is today's date, cex=.7 (size) and color
# is a kind of grey
tImprov <- function(Seconds, entrySeconds){
        tI<-as.numeric(Seconds)-as.numeric(entrySeconds)
        return(tI)
}
standardTheme <- theme(
        axis.title.y = element_text(size = rel(1.5), colour = "white", face ='bold'),
        axis.title.x = element_text(size = rel(1.5), colour = "white", face ='bold'),
        plot.title = element_text(size = rel(2), colour = "navy", face ='bold'),
        axis.text = element_text(size = rel(1.5), colour = "white", face ='bold'),
        panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        #                plot.background = element_rect(fill = "white",colour = NA)
        plot.background = element_rect(fill = "transparent",colour = NA)
        #axis.text = element_text(colour="yellow")
)


makeFootnote <- function(footnoteText=
                                 format(Sys.time(), "%d %b %Y"),
                         size= .5, color= grey(.5))
{
        require(grid)
        pushViewport(viewport())
        grid.text(label= footnoteText ,
                  x = unit(1,"npc") - unit(2, "mm"),
                  y= unit(2, "mm"),
                  just=c("right", "bottom"),
                  gp=gpar(cex= size, col=color))
        popViewport()
}


makeHeadnote <- function(headnoteText=
                                 format(Sys.time(), "%d %b %Y"),
                         size= .5, color= grey(.5))
{
        require(grid)
        pushViewport(viewport())
        grid.text(label= headnoteText ,
                  x = unit(1,"npc") - unit(2, "mm"),
                  y= unit(1,"npc") - unit(2, "mm"),
                  just=c("right", "top"),
                  gp=gpar(cex= size, col=color))
        popViewport()
}

# This is a game changer function that replicates the INDEX value of the 
# most recent TRUE value....  NIFTY 
most.recent <- function(x) {
        # return a vector of indices of the most recent TRUE value
        if (!is.logical(x))
                stop("x must be logical")
        x.pos <- which(x)
        if (length(x.pos)==0 || x.pos[1] != 1)
                x.pos <- c(1, x.pos)
        rep(x.pos, c(diff(x.pos), length(x) - x.pos[length(x.pos)] + 1))
}


toSeconds <- function(x){
        if (!is.character(x)) stop("x must be a character string of the form H:M:S")
        if (length(x)<=0)return(x)
        
        unlist(
                lapply(x,
                       function(i){
                               i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
                               if (length(i) == 3) 
                                       i[1]*3600 + i[2]*60 + i[3]
                               else if (length(i) == 2) 
                                       i[1]*60 + i[2]
                               else if (length(i) == 1) 
                                       i[1]
                       }  
                )  
        )  
} 



getlineID <- function(x){ y <- unlist(strsplit(as.character(x)," ", fixed = TRUE))
                          d<-y[1]
                          return(d)}

getEventDetail <- function(x){ y <- unlist(strsplit(as.character(x)," ", fixed = TRUE))
                               y<-y[y!=""]
                               Province <- substr(y[1], nchar(y[8])-2, nchar(y[8]))
                               EventNumber <- substr(y[8], nchar(y[8])-2, nchar(y[8]))
                               return(d)}

# function getEventDetailDB and function getFinalsEventDetailDB works together to create two
# different DBs that can be joinded to give one DB for the entire gala:

getHeatsEventDetailDB <- function(x){Province <- substr(x, 4, 5)
                                
                               EventNumber <- as.numeric(substr(x, 73, 75))
                               #Test if this is a MultiClass event:
                                       #MultiClass<-is.element(EventNumber, MCEventList)
                                       if(is.element(EventNumber, MCEventList)=="TRUE") MC <- ", MultiClass" else MC <- ""
                                       if(is.element(EventNumber, MCEventList)=="TRUE") MClass <- TRUE else MClass <- FALSE

                               
                               Swimmer <- substr(x, 40, 52)
                               #First Split along COMMAS:  this separates the surname from the rest
                                       ds1 <- unlist(strsplit(substr(x, 12, 39),",", fixed = TRUE))
                                       #Now split the FIRSTnames along spaces and UNLIST:
                                       ds2 <- unlist(strsplit(unlist(ds1)[2]," ", fixed = TRUE))
                                       ds3 <- ds2[ds2!=""]
                               Name <- paste(ds3[1], ds1[1])
                               #Surname<-strsplit(substr(x, 12, 39),",", fixed = TRUE)
                               #Surname <-
                               BirthDate <- as.character(as.Date(substr(x, 40, 45),"%y%m%d"))
                               Age <- round(as.numeric(as.Date(format(Sys.time(), "%y%m%d"),"%y%m%d")-as.Date(substr(x, 40, 45),"%y%m%d"))/365.25,2)
                               AgeGroup <- as.numeric(substr(x, 64, 65))
                               Gender <- substr(x, 66, 66)
                               Stroke <- as.numeric(substr(x, 72, 72))
                               Distance <- as.numeric(substr(x, 69, 71))
                               
                               Date <- substr(x, 81, 88)
                               DateString <- as.character(as.Date(substr(x, 81, 88),"%y%m%d"))
                               entryTime <- substr(x, 90, 96)
                               entrySeconds <- toSeconds(entryTime)
                               
                               recTime <- substr(x, 99, 105)
                               Seconds <- toSeconds(recTime)
                               tImprov <- tImprov(Seconds, entrySeconds)
                               Event <- as.numeric(substr(x, 125, 126))
                               Lane <- as.numeric(substr(x, 128, 128))
                               Position <- as.numeric(substr(x, 134, 135))
                               #Remove all white spaces:  gsub("[[:space:]]", "", recTime)
                               if(gsub("[[:space:]]", "", recTime)=="DQ") DQ <- "TRUE" else DQ <- "FALSE"
                               if(gsub("[[:space:]]", "", recTime)=="NS") NS <- "TRUE" else NS <- "FALSE"
                               # this yields the same as the function EventStringFunc:
                               LongEventString <- paste(paste("Event", 
                                                            as.character(round(as.numeric(as.character(EventNumber)),0)), 
                                                            "-", GenderID(Gender),
                                                            paste(as.character(Distance),
                                                        "m", sep=""),StrokeID(as.numeric(Stroke))), MC, sep="")
                               
                               # this yields the same as the function EventStringFunc:
                               EventString <- paste("Event", 
                                                      as.character(round(as.numeric(EventNumber),0)), 
                                                      "-", 
                                                      paste(as.character(Distance),"m", sep=""),
                                                      StrokeID(as.numeric(Stroke)), "- Heats")
                               #The following yields a string for the stroke of the event
                                      StrokeString <- paste(StrokeID(as.numeric(Stroke)))
                               #The following yields a string for the distance of the event
                                      DistanceString <- paste(paste(as.character(Distance),"m", sep=""))
                               Final <- FALSE
                               EventType <- "Heats"
                               points <- 0
                               
                               d <- data.frame(Province, 
                                               EventNumber, 
                                               Swimmer,
                                               Name,
                                               BirthDate,
                                               Age, AgeGroup, Gender, Stroke, Distance,
                                               Date,entryTime,entrySeconds,
                                               recTime,Seconds,Event,Lane,Position, tImprov, Final,
                                               points,
                                               LongEventString,
                                               EventString,
                                               StrokeString,
                                               DistanceString,
                                               DQ,
                                               NS,
                                               MClass,
                                               EventType)
                               
                               return(d)}


getFinalsEventDetailDB <- function(x){Province <- substr(x, 4, 5)
                                      
                                      EventNumber <- as.numeric(substr(x, 73, 75))
                                      #Test if this is a MultiClass event:
                                      #MultiClass<-is.element(EventNumber, MCEventList)
                                      if(is.element(EventNumber, MCEventList)=="TRUE") MC <- ", MultiClass" else MC <- ""
                                      if(is.element(EventNumber, MCEventList)=="TRUE") MClass <- TRUE else MClass <- FALSE
                                      
                                      Swimmer <- substr(x, 40, 52)
                                      #First Split along COMMAS:  this separates the surname from the rest
                                      ds1 <- unlist(strsplit(substr(x, 12, 39),",", fixed = TRUE))
                                      #Now split the FIRSTnames along spaces and UNLIST:
                                      ds2 <- unlist(strsplit(unlist(ds1)[2]," ", fixed = TRUE))
                                      ds3 <- ds2[ds2!=""]
                                      Name <- paste(ds3[1], ds1[1])
                                      #Surname<-strsplit(substr(x, 12, 39),",", fixed = TRUE)
                                      #Surname <-
                                      BirthDate <- as.character(as.Date(substr(x, 40, 45),"%y%m%d"))
                                      Age <- round(as.numeric(as.Date(format(Sys.time(), "%y%m%d"),"%y%m%d")-as.Date(substr(x, 40, 45),"%y%m%d"))/365.25,2)
                                      AgeGroup <- as.numeric(substr(x, 64, 65))
                                      Gender <- substr(x, 66, 66)
                                      Stroke <- as.numeric(substr(x, 72, 72))
                                      Distance <- as.numeric(substr(x, 69, 71))
                                      
                                      Date <- substr(x, 81, 88)
                                      DateString <- as.character(as.Date(substr(x, 81, 88),"%y%m%d"))                                      
                                      entryTime <- substr(x, 90, 96)
                                      entrySeconds <- toSeconds(entryTime)
                                      
                                #The following data is for finals only:
                                       recTime <- substr(x, 116, 123)
                                       Seconds <- toSeconds(recTime)
                                        tImprov <- tImprov(Seconds, entrySeconds)
                                       Event <- as.numeric(substr(x, 130, 130))
                                       Lane <- as.numeric(substr(x, 132, 132))
                                       Position <- as.numeric(substr(x, 137, 138))
                                if(gsub("[[:space:]]", "", recTime)=="DQ") DQ <- "TRUE" else DQ <- "FALSE"
                                if(gsub("[[:space:]]", "", recTime)=="NS") NS <- "TRUE" else NS <- "FALSE"
                                
                                #EventNo <- 
                                #MultiClass <- gsub(" ", "", getElement(FrameX, "MultiClass"), fixed = TRUE)
                                #Gender <- getElement(FrameX, "Gender")
                                #Distance <- getElement(FrameX, "Distance")
                                #Stroke <- getElement(FrameX, "Stroke")
                                
                                
                                # this yields the same as the function EventStringFunc:
                                LongEventString <- paste(paste("Event", 
                                                               as.character(round(as.numeric(as.character(EventNumber)),0)), 
                                                               "-", GenderID(Gender),
                                                               paste(as.character(Distance),
                                                                     "m", sep=""),StrokeID(as.numeric(Stroke))), MC, sep="")
                                
                                # this yields the same as the function EventStringFunc:
                                EventString <- paste("Event", 
                                                     as.character(round(as.numeric(EventNumber),0)), 
                                                     "-", 
                                                     paste(as.character(Distance),"m", sep=""),
                                                     StrokeID(as.numeric(Stroke)), "- Finals")
                                #The following yields a string for the stroke of the event
                                StrokeString <- paste(StrokeID(as.numeric(Stroke)))
                                #The following yields a string for the distance of the event
                                DistanceString <- paste(paste(as.character(Distance),"m", sep=""))
                                EventType <- "Finals"                                
                                Final = TRUE  
                                points <- as.numeric(substr(x, 140, 141))
                               
#                                LonggEN <- apply(EventStringdf, 1, EventStringFunc)
#                                gEN <- apply(EventStringdf, 1, ShortEventStringFunc)
#                                gENstroke <- apply(EventStringdf, 1, strokeStringFunc)
#                                gENdistance <- apply(EventStringdf, 1, distanceStringFunc)      
                               
                                d <- data.frame(Province, 
                                                EventNumber, 
                                                Swimmer,
                                                Name,
                                                BirthDate,
                                                Age, AgeGroup, Gender, Stroke, Distance,
                                                Date,entryTime,entrySeconds,
                                                recTime,Seconds,Event,Lane,Position,tImprov, Final,
                                                points,
                                                LongEventString,
                                                EventString,
                                                StrokeString,
                                                DistanceString,
                                                DQ,
                                                NS,
                                                MClass,
                                                EventType)
                                return(d)}


getEventDetailX <- function(x){Province <- substr(x, 4, 5)
                               EventNumber <- as.numeric(substr(x, 73, 75))
                               Swimmer <- substr(x, 40, 52)
                               #First Split along COMMAS:  this separates the surname from the rest
                               ds1 <- unlist(strsplit(substr(x, 12, 39),",", fixed = TRUE))
                               #Now split the FIRSTnames along spaces and UNLIST:
                               ds2 <- unlist(strsplit(unlist(ds1)[2]," ", fixed = TRUE))
                               ds3 <- ds2[ds2!=""]
                               Name <- paste(ds3[1], ds1[1])
                               #Surname<-strsplit(substr(x, 12, 39),",", fixed = TRUE)
                               #Surname <-
                               BirthDate <- as.character(as.Date(substr(x, 40, 45),"%y%m%d"))
                               Age <- round(as.numeric(as.Date(format(Sys.time(), "%y%m%d"),"%y%m%d")-as.Date(substr(x, 40, 45),"%y%m%d"))/365.25,2)
                               AgeGroup <- as.numeric(substr(x, 64, 65))
                               Gender <- substr(x, 66, 66)
                               Stroke <- as.numeric(substr(x, 72, 72))
                               Distance <- as.numeric(substr(x, 69, 71))
                               
                               Date <- substr(x, 81, 88)
                               entryTime <- substr(x, 90, 96)
                               entrySeconds <- toSeconds(entryTime)
                               
                               heatTime <- substr(x, 99, 105)
                               heatSeconds <- toSeconds(heatTime)
                               heatEvent <- as.numeric(substr(x, 125, 126))
                               heatLane <- as.numeric(substr(x, 128, 128))
                               heatPosition <- as.numeric(substr(x, 134, 135))
                               
                               finalsTime <- substr(x, 116, 123)
                               finalsSeconds <- toSeconds(finalsTime)
                               finalsEvent <- as.numeric(substr(x, 130, 130))
                               finalsLane <- as.numeric(substr(x, 132, 132))
                               finalsPosition <- as.numeric(substr(x, 137, 138))
                               
                               points <- as.numeric(substr(x, 140, 141))
                               
                               d <- data.frame(Province, 
                                               EventNumber, 
                                               Swimmer,
                                               Name,
                                               BirthDate,
                                               Age, AgeGroup, Gender, Stroke, Distance,
                                               Date,entryTime,entrySeconds,
                                               heatTime,heatSeconds,heatEvent,heatLane,heatPosition,
                                               finalsTime,finalsSeconds,finalsEvent,finalsLane,finalsPosition,
                                               points)
                               
                               return(d)}




getClubDetail <- function(x){ClubCode <- substr(x, 12, 17)
                             ClubName <- substr(x, 18, 47)
                             ClubShortName <- substr(x, 48, 63)
                             ClubAddress <- substr(x, 64, 139)
                             ClubCountry <- substr(x, 140, 142)
                             
                             d <- data.frame(ClubCode, 
                                             ClubName, 
                                             ClubShortName,
                                             ClubAddress,
                                             ClubCountry)
                             
                             return(d)}


StrokeID <- function(type) {
        switch(type,
               "FreeStyle",
               "BackStroke",
               "BreastStroke",
               "Fly",
               "IM")
}

GenderID <- function(type) {
        if (type == "F") "Women"
        else if (type == "M") "Men"
}


getSwimmersDetailX <- function(SwimmerCode, eF){
        #eF refers to the Gala eventFrame as per the SQL db.
        x <- eF[is.element(eF$Swimmer, SwimmerCode),][1,]
        Swimmer <- x$Swimmer
        Name <- x$Name
        Province <- x$Province
        BirthDate <- x$BirthDate
        Age <- x$Age
        AgeGroup <- x$AgeGroup
        Gender <- x$Gender
        Club <- x$ClubCode
        
        d <- data.frame(Swimmer, 
                        Name, 
                        Province,
                        BirthDate,
                        Age, AgeGroup, Gender, Club)
        
        return(d)}

EventStringFunc <- function(FrameX){
        EventNo <- getElement(FrameX, "EventNo")
        MultiClass <- gsub(" ", "", getElement(FrameX, "MultiClass"), fixed = TRUE)
        Gender <- getElement(FrameX, "Gender")
        Distance <- getElement(FrameX, "Distance")
        Stroke <- getElement(FrameX, "Stroke")
        
        if(MultiClass=="TRUE") MC <- ", MultiClass" else MC <- ""
        IdentifierSTR <- paste(paste("Event", as.character(round(as.numeric(EventNo),0)), " - ", GenderID(Gender),paste(as.character(Distance),"M", sep=""),StrokeID(as.numeric(Stroke))), MC, sep="")
        return(IdentifierSTR)
        
}
        


ShortEventStringFunc <- function(FrameX){
        EventNo <- getElement(FrameX, "EventNo")
#        MultiClass <- gsub(" ", "", getElement(FrameX, "MultiClass"), fixed = TRUE)
#        Gender <- getElement(FrameX, "Gender")
        Distance <- getElement(FrameX, "Distance")
        Stroke <- getElement(FrameX, "Stroke")
        
#        if(MultiClass=="TRUE") MC <- ", MultiClass" else MC <- ""
        IdentifierSTR <- paste("Event", as.character(round(as.numeric(EventNo),0)), " - ", paste(as.character(Distance),"m", sep=""),StrokeID(as.numeric(Stroke)))
        return(IdentifierSTR)
        
}


strokeStringFunc <- function(FrameX){
        Stroke <- getElement(FrameX, "Stroke")
        
        #        if(MultiClass=="TRUE") MC <- ", MultiClass" else MC <- ""
        IdentifierSTR <- paste(StrokeID(as.numeric(Stroke)))
        return(IdentifierSTR)
        
}


distanceStringFunc <- function(FrameX){
        Distance <- getElement(FrameX, "Distance")
        IdentifierSTR <- paste(paste(as.character(Distance),"m", sep=""))
        return(IdentifierSTR)
        
}

FinalsFunc <- function(finalsSeconds){
        if(as.numeric(finalsSeconds)>0&!(is.na(as.numeric(finalsSeconds)))) Final <- TRUE else Final<-FALSE 
        return(Final)
        
}

fileNameString <- function(galaFrame, 
                        Stroke1, 
                        Stroke2, 
                        Distance1, 
                        Distance2, 
                        Final, 
                        MClass, 
                        InclMen, 
                        InclWomen){
        
}
#The following function takes the dataframe/eventframe  
subStrokeDB <- function(galaFrame, 
                        Stroke1, 
                        Stroke2, 
                        Distance1, 
                        Distance2, 
                        Final, 
                        MClass, 
                        InclMen, 
                        InclWomen){                
        #                    Get the names of swimmers who swam Stroke1, Distance1
        T1 <- galaFrame[(galaFrame$Stroke==Stroke1&
                                 galaFrame$Distance==Distance1),]$Name
        #            Get the names of swimmers who swam Stroke2, Distance2
        T2 <- galaFrame[(galaFrame$Stroke==Stroke2&galaFrame$Distance==Distance2),]$Name
        
        nameSelection<-T1[is.element(T1,T2)]
        if(InclMen==TRUE & InclWomen==TRUE) galaFrame$genderVector <- TRUE else
                if(InclMen==FALSE & InclWomen==TRUE){
                        galaFrame$genderVector <- (galaFrame$Gender == "F")} else 
                                if(InclMen==TRUE & InclWomen==FALSE){
                                galaFrame$genderVector <- (galaFrame$Gender == "M")}
        
        
        df1<-galaFrame[is.element(galaFrame$Name,nameSelection)&
                               (galaFrame$Stroke==Stroke1&
                                        galaFrame$Distance==Distance1&
                                        galaFrame$Final==Final&
                                        galaFrame$MClass==MClass&
                                        galaFrame$genderVector),]
        
        # Name1<-galaFrame[is.element(galaFrame$Name,nameSelection),]$Name
        df2<-galaFrame[is.element(galaFrame$Name,nameSelection)&
                               (galaFrame$Stroke==Stroke2&
                                        galaFrame$Distance==Distance2&
                                        galaFrame$Final==Final&
                                        galaFrame$MClass==MClass&
                                        galaFrame$genderVector),]
        #df2$tImprov2 <- df2$tImprov
        #Here is the game changer
        
        m1<-merge(df1,df2,by="Swimmer")
        
        return(m1)
}

numberField<-function(gF){
        nF <- gF
        nF$Age <- as.numeric(as.character(gF$Age))
        nF$AgeGroup <- as.numeric(as.character(gF$AgeGroup))
        nF$entrySeconds <- as.numeric(as.character(gF$entrySeconds))
        nF$Distance <- as.numeric(as.character(gF$Distance))
        nF$Seconds <- as.numeric(as.character(gF$Seconds))
        nF$Event <- as.numeric(as.character(gF$Event))
        nF$Lane <- as.numeric(as.character(gF$Lane))
        nF$Position <- as.numeric(as.character(gF$Position))
        nF$tImprov <- as.numeric(as.character(gF$tImprov))
        nF$Position <- as.numeric(as.character(gF$Position))
        nF$points <- as.numeric(as.character(gF$points))
        return(nF)
}

numberFieldmerged<-function(gF){
        nF <- gF
        nF$Age.x <- as.numeric(as.character(gF$Age.x))
        nF$AgeGroup.x <- as.numeric(as.character(gF$AgeGroup.x))
        nF$entrySeconds.x <- as.numeric(as.character(gF$entrySeconds.x))
        nF$Distance.x <- as.numeric(as.character(gF$Distance.x))
        nF$Seconds.x <- as.numeric(as.character(gF$Seconds.x))
        nF$Event.x <- as.numeric(as.character(gF$Event.x))
        nF$Lane.x <- as.numeric(as.character(gF$Lane.x))
        nF$Position.x <- as.numeric(as.character(gF$Position.x))
        nF$tImprov.x <- as.numeric(as.character(gF$tImprov.x))
        nF$Position.x <- as.numeric(as.character(gF$Position.x))
        nF$points.x <- as.numeric(as.character(gF$points.x))
        
        nF$Age.y <- as.numeric(as.character(gF$Age.y))
        nF$AgeGroup.y <- as.numeric(as.character(gF$AgeGroup.y))
        nF$entrySeconds.y <- as.numeric(as.character(gF$entrySeconds.y))
        nF$Distance.y <- as.numeric(as.character(gF$Distance.y))
        nF$Seconds.y <- as.numeric(as.character(gF$Seconds.y))
        nF$Event.y <- as.numeric(as.character(gF$Event.y))
        nF$Lane.y <- as.numeric(as.character(gF$Lane.y))
        nF$Position.y <- as.numeric(as.character(gF$Position.y))
        nF$tImprov.y <- as.numeric(as.character(gF$tImprov.y))
        nF$Position.y <- as.numeric(as.character(gF$Position.y))
        nF$points.y <- as.numeric(as.character(gF$points.y))
        return(nF)
}