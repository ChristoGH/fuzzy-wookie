initial.dir<-getwd()
setwd("C:/Users/Admin/Creative Cloud Files/Swimming/SA Short Course Champs Aug PMBG 2014/StrokeAnalysis")


#Define a function that will correctly label the stroke:
#1 = FreeStyle; 2 = BackStroke; 3 BreastStroke; 4 = Fly; 5 = IM

## This script connects to the latest Swimming db on this computer
ch <- odbcConnect("Swimming")
eventFrame<-sqlFetch(ch, "eventFrame", as.is= TRUE)
odbcClose(ch)   
#This file starts with eventFrame and anlyzes the file by distance and stroke
#Female 50 freestyle table)
eventFrame$points<-as.numeric(eventFrame$points)
Gender <- "F" #Women
Distance <- 200 #200m
Stroke <- 5 #IM

        Idx <- (eventFrame$Gender==Gender)&
                (as.numeric(eventFrame$Stroke)==Stroke)&
                (as.numeric(eventFrame$Distance)==Distance)&
                (as.logical(eventFrame$MultiClass) == FALSE)
 
        IdentifierSTR <- paste(GenderID(Gender),paste(as.character(Distance),"M", sep=""),StrokeID(Stroke))

        FFrame <- eventFrame[Idx,]
        #splitFrame[order(splitFrame$heatSplits),]
        #boxplot(AgeGroup~heatSeconds,data=splitFrame, main="Car Milage Data", xlab="Number of Cylinders", ylab="Miles Per Gallon")
        #boxplot(heatSeconds~AgeGroup,data=splitFrame, main="Car Milage Data", notch=TRUE, col=(c("gold","darkgreen")), xlab="Number of Cylinders", ylab="Miles Per Gallon")

#        plot(splitFrame$AgeGroup, splitFrame$heatSeconds)

        ##pdf(file="c:/doc/testPlot1.pdf")
        FFrame$timeDiff <- (as.numeric(FFrame$entrySeconds)-as.numeric(FFrame$heatSeconds))
        splitFrame <- FFrame[!outlier(as.numeric(FFrame$entrySeconds)-as.numeric(FFrame$heatSeconds),logical=TRUE),]
        splitFrame$entrySeconds<-as.numeric(splitFrame$entrySeconds)
        splitFrame$heatSeconds<-as.numeric(splitFrame$heatSeconds)
##x=reorder(factor(Province),table(factor(Province))[factor(Province)])
          
        p <- ggplot(splitFrame, aes(x=reorder(factor(Province),table(factor(Province))[factor(Province)]), y=as.numeric(heatSeconds)))
        p + geom_boxplot(aes(fill=Province), outlier.colour="red") + coord_flip() 

        splitFrame$tImprov <- tImprov(Seconds = splitFrame$heatSeconds, 
                                      entrySeconds= splitFrame$entrySeconds)
        
        p <- ggplot(splitFrame, aes(y=tImprov, x=Swimmer))
        p + geom_bar(binwidth=1, stat="identity") 


#-The following code  produces a graph showing the time improvement
        pdf(file=paste("Time Improvement by Swimmer and Agegroup", IdentifierSTR,".pdf"))
                p <- ggplot(splitFrame, aes(x=reorder(Swimmer, tImprov), y=tImprov, fill=AgeGroup))
                p + geom_bar(stat="identity") + coord_flip() +
                labs(title = paste("Time Improvement by Swimmer and Agegroup\n",IdentifierSTR), x="Swimmer", y="Improvement in Seconds") + theme(
                        axis.title.y = element_text(size = rel(1), colour = "white", face ='bold'),
                        axis.title.x = element_text(size = rel(1), colour = "white", face ='bold'),
                        plot.title = element_text(size = rel(2), colour = "navy", face ='bold'),
                        axis.text.x = element_text(size = rel(1), colour = "white", face ='bold'),
                        axis.text.y = element_text(size = rel(1), colour = "white", face ='bold'),
                        panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                        panel.grid.minor = element_blank(), 
                        panel.grid.major = element_blank(),
                        #                plot.background = element_rect(fill = "white",colour = NA)
                        plot.background = element_rect(fill = "transparent",colour = NA)
                        #axis.text = element_text(colour="yellow")
                )
                makeFootnote(paste("Event",as.character(round(as.numeric(splitFrame$EventNumber[1]),0)), as.Date(splitFrame$Date[1],format="%m%d%Y")), color = "black")
                makeHeadnote(HeadNoteALL, color = "black")
        dev.off()




# This graph still needs some work:
        p <- ggplot(splitFrame, aes(x=reorder(Swimmer, heatSeconds), y=heatSeconds, fill=Province))
        p + geom_bar(stat="identity") + coord_flip()

        p <- ggplot(splitFrame, aes(x=AgeGroup, y=as.numeric(heatSeconds)))
        p + geom_boxplot(aes(fill=Province), outlier.colour="red") + coord_flip() 


        ggplot(splitFrame, aes(x=as.numeric(heatSeconds),fill = factor(AgeGroup))) +
        geom_bar(binwidth=1)


        ggplot(splitFrame, aes(x=as.numeric(heatSeconds),fill = factor(Province))) +
        geom_bar(binwidth=1)

        p <- ggplot(splitFrame, aes(x=factor(ClubCode), y=as.numeric(heatSeconds)))
        p + geom_boxplot(aes(fill=ClubCode), outlier.colour="red") + coord_flip() 

        pdf(file=paste("TimeImprovementbyClub", IdentifierSTR,".pdf"))
                p <- ggplot(splitFrame, aes(x=factor(ClubCode), y=as.numeric(timeDiff)))
                p + geom_boxplot(aes(fill=ClubCode), outlier.colour="red") + coord_flip() +
                labs(title = paste("Time Improvement by Club\n",IdentifierSTR), x="AgeGroup", y="Seconds")
                makeFootnote(paste("Event",as.character(round(as.numeric(splitFrame$EventNumber[1]),0)), as.Date(splitFrame$Date[1],format="%m%d%Y")), color = "black")
                makeHeadnote(HeadNoteALL, color = "black")
        dev.off()

##      Save the following:      
##        plot.new()

# Time improvement by Age group
        pdf(file=paste("TimeImprovementbyAge", IdentifierSTR,".pdf"))
                p <- ggplot(splitFrame, aes(x=factor(AgeGroup), y=as.numeric(timeDiff)))
                p + geom_boxplot(aes(fill=AgeGroup), outlier.colour="red") + coord_flip() +
                labs(title = paste("Time Improvement by Agegroup\n",IdentifierSTR), x="AgeGroup", y="Seconds")
                makeFootnote(paste("Event",as.character(round(as.numeric(splitFrame$EventNumber[1]),0)), as.Date(splitFrame$Date[1],format="%m%d%Y")), color = "black")
                makeHeadnote(HeadNoteALL, color = "black")
        dev.off()

# Time improvement by Lane
        pdf(file=paste("TimeImprovementbyLane", IdentifierSTR,".pdf"))
                p <- ggplot(splitFrame, aes(x=factor(heatLane), y=as.numeric(timeDiff)))
                p + geom_boxplot(aes(fill=heatLane), outlier.colour="red") + coord_flip() +
                labs(title = paste("Time Improvement by Lane\n",IdentifierSTR), x="Lane", y="Seconds")
                makeFootnote(paste("Event",as.character(round(as.numeric(splitFrame$EventNumber[1]),0)), as.Date(splitFrame$Date[1],format="%m%d%Y")), color = "black")
                makeHeadnote(HeadNoteALL, color = "black")
        dev.off()

        pdf(file=paste("TimebyLane", IdentifierSTR,".pdf"))
                p <- ggplot(splitFrame, aes(x=factor(heatLane), y=as.numeric(heatSeconds)))
                p + geom_boxplot(aes(fill=heatLane), outlier.colour="red") + coord_flip() +
                labs(title = paste("Time by Lane\n",IdentifierSTR), x="Lane", y="Seconds")
                makeFootnote(paste("Event",as.character(round(as.numeric(splitFrame$EventNumber[1]),0)), as.Date(splitFrame$Date[1],format="%m%d%Y")), color = "black")
                makeHeadnote(HeadNoteALL, color = "black")
        dev.off()

        pdf(file=paste("Age distribution by Province", IdentifierSTR,".pdf"))
                ggplot(SwimmersData, aes(x=reorder(factor(Province),table(factor(Province))[factor(Province)]), y=AgeGroup, fill = factor(Province))) +
                geom_boxplot() +  coord_flip() + labs(title = "Age distribution by Province\n Test this", x="Province", y="Age Group") +  theme(legend.text = element_text(colour="blue", size = 16, face = "bold"))+
                scale_fill_discrete(name="Province") 
                makeFootnote(paste("Event",as.character(round(as.numeric(splitFrame$EventNumber[1]),0)), as.Date(splitFrame$Date[1],format="%m%d%Y")), color = "black")
                makeHeadnote(HeadNoteALL, color = "black")
        dev.off()
#BubbleData analysis


bubbleData<-splitFrame
bubbleData$AgeGroup<-as.numeric(splitFrame$AgeGroup)

ggplot(bubbleData, aes(x=tImprov, y=heatSeconds, size=AgeGroup, label=AgeGroup), guide=FALSE)+
        geom_point(colour="white", fill="red", shape=21) + scale_size_area(max_size = 10)+
        geom_text(size=4)+
        theme_bw()
 
# To test the function below:
# bubblePlotFunc(eventFrame$LongEventString[1], eventFrame)
# LongEventString<-eventFrame$LongEventString[1]
# FrameX <- eventFrame
lapply(unique(FrameX$LongEventString), bubblePlotFunc, FrameX)

bubblePlotFunc <- function(LongEventString, FrameX){
        FFrame <- FrameX[FrameX$LongEventString==LongEventString,]
        splitFrame <- FFrame[!outlier(as.numeric(FFrame$entrySeconds)-as.numeric(FFrame$heatSeconds),logical=TRUE),]
        splitFrame$entrySeconds<-as.numeric(splitFrame$entrySeconds)
        splitFrame$heatSeconds<-as.numeric(splitFrame$heatSeconds)
        splitFrame$tImprov <- tImprov(Seconds = splitFrame$heatSeconds, 
                                      entrySeconds= splitFrame$entrySeconds)
        splitFrame$AgeGroup <-as.numeric(splitFrame$AgeGroup)
        pdf(file=paste(LongEventString,".pdf"))
        BubblePlot <- ggplot(splitFrame, aes(x=tImprov, y=heatSeconds,size=AgeGroup,label=Name))+
                geom_point(aes(colour=factor(Province),name = "Province"), alpha=0.5) +  labs(title = LongEventString, x="Time improvement in Seconds", y="Time in Seconds") +#scale_size_area(min_size = 1,max_size = 10)+
                geom_text(size=2, aes(label=Name),hjust=-0.15, vjust=0) +
                scale_size(range = c(1, 15), name = "Relative Age")+  
                guides(col = guide_legend(override.aes = list(shape = 15, size = 10))) +
                theme(
                        axis.title.y = element_text(size = rel(1.25), colour = "white", face ='bold'),
                        axis.title.x = element_text(size = rel(1.25), colour = "white", face ='bold'),
                        plot.title = element_text(size = rel(1.5), colour = "navy", face ='bold', vjust=0.35),
                        axis.text = element_text(size = rel(1), colour = "white", face ='bold'),
                        panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                        panel.grid.minor = element_blank(), 
                        panel.grid.major = element_blank(),
                        #legend.title = element_text("This is it!"),
                        #                plot.background = element_rect(fill = "white",colour = NA)
                        plot.background = element_rect(fill = "transparent",colour = NA)
                        #axis.text = element_text(colour="yellow")
                )+ scale_fill_discrete(name="Experimental\nCondition")+
                guides(size=guide_legend(override.aes = list(fill="black", alpha=1)))+
                scale_colour_brewer(palette="Set1")
        print(BubblePlot)
        makeHeadnote(HeadNoteALL, color = "black")
        
        dev.off()
        
        return(splitFrame)
}

heatsBubblesPlots <- function(LongEventString, FrameX){
        splitFrame <- FrameX$LongEventString==
        Stroke <- getElement(FrameX, "Stroke")
        
        #        if(MultiClass=="TRUE") MC <- ", MultiClass" else MC <- ""
        IdentifierSTR <- paste(StrokeID(as.numeric(Stroke)))
        return(IdentifierSTR)
        
}

 

        Freestyle50FFrame <-data.frame(as.numeric(as.vector(FFrame$finalsLane)),as.numeric(as.vector(FFrame$finalsSeconds)), as.vector(FFrame$Name), as.vector(FFrame$ClubCode), round(as.numeric(as.vector(FFrame$AgeGroup)),2))
        Freestyle50FFrame<-setNames(Freestyle50FFrame, c("finalsLane", "finalsSeconds", "Name", "ClubCode", "Age"))
 
        Freestyle50FFrame <- Freestyle50FFrame[!is.na(Freestyle50FFrame$finalsSeconds),]
 
 #setNames(aggCobblingSTTableTF, c("ServiceItem", "Value"))
 #DF<-as.data.frame(D)
 Freestyle50FFrame<-Freestyle50FFrame[with(Freestyle50FFrame, order(finalsLane)), ]
 Freestyle50FFrame$TimeDiff <- min(Freestyle50FFrame$finalsSeconds)-Freestyle50FFrame$finalsSeconds
 Freestyle50FFrame$DistanceDiff <- round(Freestyle50FFrame$finalsSeconds/50*Freestyle50FFrame$TimeDiff*10,0)*10
Freestyle50FFrame

plot(as.numeric(as.vector(Frame$Age)),as.numeric(as.vector(Frame$entryTime))-as.numeric(as.vector(Frame$heatTime)))

MFrame <- eventFrame[(eventFrame$Gender=="M")&(eventFrame$Stroke==1)&(eventFrame$Distance==50)&(eventFrame$MultiClass == FALSE),]
Freestyle50MFrame <-data.frame(as.numeric(as.vector(MFrame$finalsLane)),as.numeric(as.vector(MFrame$finalsSeconds)), as.vector(MFrame$Name), as.vector(MFrame$ClubCode), round(as.numeric(as.vector(MFrame$AgeGroup)),2))
Freestyle50MFrame<-setNames(Freestyle50MFrame, c("finalsLane", "finalsSeconds", "Name", "ClubCode", "Age"))

Freestyle50MFrame <- Freestyle50MFrame[!is.na(Freestyle50MFrame$finalsSeconds),]
Freestyle50MFrame
#setNames(aggCobblingSTTableTF, c("ServiceItem", "Value"))
#DF<-as.data.frame(D)
Freestyle50MFrame<-Freestyle50MFrame[with(Freestyle50MFrame, order(finalsLane)), ]
Freestyle50MFrame$TimeDiff <- min(Freestyle50MFrame$finalsSeconds)-Freestyle50MFrame$finalsSeconds
Freestyle50MFrame$DistanceDiff <- round(Freestyle50MFrame$finalsSeconds/50*Freestyle50MFrame$TimeDiff*10,0)*10
Freestyle50MFrame
plot(as.numeric(as.vector(MFrame$Age)),as.numeric(as.vector(MFrame$entryTime))-as.numeric(as.vector(MFrame$heatTime)))






#This line returns the directory to its original state:
#setwd(initial.dir)


