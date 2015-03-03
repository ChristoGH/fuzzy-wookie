initial.dir<-getwd()
setwd("C:/rProgramming/SwimStats/SA Short Course Champs Aug PMBG 2014/StrokeAnalysis")
#C:/rProgramming/SwimStats/StrokeAnalysis

#Define a function that will correctly label the stroke:
#1 = FreeStyle; 2 = BackStroke; 3 BreastStroke; 4 = Fly; 5 = IM

## This script connects to the latest Swimming db on this computer
ch <- odbcConnect("Swimming")
eventFrame<-sqlFetch(ch, "eventFrame", as.is= TRUE)
odbcClose(ch)   

# We need a new eventFrame that will have separate entries for 
# each swimmer for heats and finals alike: 

# To test the function below:
# ScreenbubblePlotFunc(eventFrame$LongEventString[1], eventFrame)
# LongEventString<-eventFrame$LongEventString[1]
# FrameX <- eventFrame
lapply(unique(eventFrame$LongEventString), ScreenbubblePlotFunc, eventFrame)
lapply(unique(eventFrame$LongEventString), PDFbubblePlotFunc, eventFrame)

#         ggplot(splitFrame, aes(x=tImprov, y=heatSeconds,size=AgeGroup,label=Name))+
#                 geom_point(aes(colour= Province,title = "Province"), alpha=0.5) +  labs(title = LongEventString, x="Time improvement in Seconds", y="Time in Seconds")+
#                 #This line puts each swimmers Name next to its point
#                 geom_text(size=2, aes(label=Name),hjust=-0.15, vjust=0) +
#                 scale_size(range = c(1, 15), name = "Relative Age")       
        

FFrame <- FrameX[FrameX$LongEventString==LongEventString,]
splitFrame <- FFrame[!outlier(as.numeric(FFrame$entrySeconds)-as.numeric(FFrame$heatSeconds),logical=TRUE),]
splitFrame$entrySeconds<-as.numeric(splitFrame$entrySeconds)
splitFrame$heatSeconds<-as.numeric(splitFrame$heatSeconds)
splitFrame$tImprov <- tImprov(splitFrame$heatSeconds, splitFrame$entrySeconds)
splitFrame$AgeGroup <-as.numeric(splitFrame$AgeGroup)

ScreenbubblePlotFunc <- function(LongEventString, FrameX){
                        print(LongEventString)
                        newFFrame <- FrameX[FrameX$LongEventString==LongEventString,]
                        
                        heatFRame <-data.frame(newFFrame$LongEventString,
                                               newFFrame$Name,
                                               newFFrame$Finals,
                                            as.numeric(newFFrame$entrySeconds),
                                            as.numeric(newFFrame$heatSeconds),
                                            as.numeric(newFFrame$AgeGroup),
                                            newFFrame$Province,
                                            newFFrame$ClubCode)
                        
                        heatFRame<-setNames(heatFRame,c("LongEventString",
                                          "Name",
                                          "Finals",
                                          "entrySeconds",
                                          "heatSeconds",
                                          "AgeGroup",
                                          "Province",
                                          "ClubCode"))
                        
                        finalFRame <-data.frame(newFFrame$LongEventString,
                                                newFFrame$Name,
                                                newFFrame$Finals,
                                               as.numeric(newFFrame$entrySeconds),
                                               as.numeric(newFFrame$finalsSeconds),
                                               as.numeric(newFFrame$AgeGroup),
                                               newFFrame$Province,
                                               newFFrame$ClubCode)
                        finalFRame<-setNames(finalFRame,c("LongEventString",
                                                        "Name",
                                                        "Finals",
                                                        "entrySeconds",
                                                        "finalsSeconds",
                                                        "AgeGroup",
                                                        "Province",
                                                        "ClubCode"))     
                        
                        finalFRame<-finalFRame[as.logical(finalFRame$Finals),]
                        heatFRame<-na.omit(heatFRame)
                        splitFrame <- finalFRame
                        finalFRame$finalsSeconds<-as.numeric(splitFrame$finalsSeconds)
                        #splitFrame$tImprov <- 
                        finalFRame$tImprov <- tImprov(splitFrame$finalsSeconds, splitFrame$entrySeconds)
                        finalFRame$AgeGroup <-as.numeric(splitFrame$AgeGroup)
                        #<-heatFRame[!as.logical(heatFRame$Finals),]
                #         newHeatsFrame<-na.omit(FFrame[!as.logical(FFrame$Finals),])
        
        if(length(finalFRame[,1])>0) {
                FinalsStr <- "- Finals"
                splitFrame <- finalFRame#[!outlier(as.numeric(finalFRame$entrySeconds),logical=TRUE),]
#--error condition---------------------------------------------------------------------------                
                if(length(splitFrame[,1])>2){
#                         if(unique(FrameX$LongEventString)[11]==LongEventString){browser()}
#                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$finalsSeconds),logical=TRUE),]
#                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$entrySeconds)-as.numeric(splitFrame$finalsSeconds),logical=TRUE),]
#         #                 splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$entrySeconds),logical=TRUE),]
                        
                        splitFrame$finalsSeconds<-as.numeric(splitFrame$finalsSeconds)
                        
                        splitFrame$tImprov <- tImprov(splitFrame$finalsSeconds, splitFrame$entrySeconds)
                        splitFrame$AgeGroup <-as.numeric(splitFrame$AgeGroup)
                        BubblePlot <- ggplot(splitFrame, aes(x=tImprov, y=finalsSeconds,size=AgeGroup,label=Name))+
                                geom_point(aes(colour=Province,title = "Province"), alpha=0.5) +  labs(title = paste(LongEventString, FinalsStr), x="Time improvement in Seconds", y="Time in Seconds") +#scale_size_area(min_size = 1,max_size = 10)+
                                geom_text(size=2, aes(label=Name),hjust=-0.15, vjust=0) +
                                scale_size(range = c(1, 15), name = "Relative Age")+  
                                guides(col = guide_legend(override.aes = list(shape = 15, size = 10))) +
                                theme(
                                        axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                                        axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                                        plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                                        axis.text = element_text(size = rel(1), colour = "Black"),
                                        #panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                                        #panel.grid.minor = element_blank(), 
                                        #panel.grid.major = element_blank(),
                                        #legend.title = element_text("This is it!"),
                                        #                plot.background = element_rect(fill = "white",colour = NA)
                                        plot.background = element_rect(fill = "transparent",colour = NA)
                                        #axis.text = element_text(colour="yellow")
                                ) +
                                guides(size=guide_legend(override.aes = list(fill="black", alpha=1)))#+
                                print(BubblePlot)
                                makeHeadnote(HeadNoteALL, color = "black")
                }
        } 
        if(length(heatFRame[,1])>0) {
                FinalsStr <- "Heats"
#-------------------------------------------------------------------------------------------
                splitFrame <- heatFRame#[!outlier(as.numeric(heatFRame$entrySeconds),logical=TRUE),]
#--error condition---------------------------------------------------------------------------                
                if(length(splitFrame[,1])>2){
#                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$heatSeconds),logical=TRUE),]
#                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$entrySeconds)-as.numeric(splitFrame$heatSeconds),logical=TRUE),]
#                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$entrySeconds),logical=TRUE),]
        
        #-------------------------------------------------------------------------------------------
                        splitFrame$heatSeconds<-as.numeric(splitFrame$heatSeconds)
        
                        splitFrame$tImprov <- tImprov(splitFrame$heatSeconds, splitFrame$entrySeconds)
                        splitFrame$AgeGroup <-as.numeric(splitFrame$AgeGroup)
                        BubblePlot <- ggplot(splitFrame, aes(x=tImprov, y=heatSeconds,size=AgeGroup,label=Name))+
                                geom_point(aes(colour=Province,title = "Province"), alpha=0.5) +  labs(title = paste(LongEventString, FinalsStr), x="Time improvement in Seconds", y="Time in Seconds") +#scale_size_area(min_size = 1,max_size = 10)+
                                geom_text(size=2, aes(label=Name),hjust=-0.15, vjust=0) +
                                scale_size(range = c(1, 15), name = "Relative Age")+  
                                guides(col = guide_legend(override.aes = list(shape = 15, size = 10))) +
                                theme(
                                        axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                                        axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                                        plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                                        axis.text = element_text(size = rel(1), colour = "Black"),
                                        #panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                                        #panel.grid.minor = element_blank(), 
                                        #panel.grid.major = element_blank(),
                                        #legend.title = element_text("This is it!"),
                                        #                plot.background = element_rect(fill = "white",colour = NA)
                                        plot.background = element_rect(fill = "transparent",colour = NA)
                                        #axis.text = element_text(colour="yellow")
                                ) +
                                guides(size=guide_legend(override.aes = list(fill="black", alpha=1)))#+
                                print(BubblePlot)
                                makeHeadnote(HeadNoteALL, color = "black")
                }
        }
        
               #scale_colour_brewer(palette="Set1")
        
        return(splitFrame)
}

#-This function prints a pdf for each graph ------------------------------------------------
PDFbubblePlotFunc <- function(LongEventString, FrameX){
        print(LongEventString)
        newFFrame <- FrameX[FrameX$LongEventString==LongEventString,]
        
        heatFRame <-data.frame(newFFrame$LongEventString,
                               newFFrame$Name,
                               newFFrame$Finals,
                               as.numeric(newFFrame$entrySeconds),
                               as.numeric(newFFrame$heatSeconds),
                               as.numeric(newFFrame$AgeGroup),
                               newFFrame$Province,
                               newFFrame$ClubCode)
        
        heatFRame<-setNames(heatFRame,c("LongEventString",
                                        "Name",
                                        "Finals",
                                        "entrySeconds",
                                        "heatSeconds",
                                        "AgeGroup",
                                        "Province",
                                        "ClubCode"))
        
        finalFRame <-data.frame(newFFrame$LongEventString,
                                newFFrame$Name,
                                newFFrame$Finals,
                                as.numeric(newFFrame$entrySeconds),
                                as.numeric(newFFrame$finalsSeconds),
                                as.numeric(newFFrame$AgeGroup),
                                newFFrame$Province,
                                newFFrame$ClubCode)
        finalFRame<-setNames(finalFRame,c("LongEventString",
                                          "Name",
                                          "Finals",
                                          "entrySeconds",
                                          "finalsSeconds",
                                          "AgeGroup",
                                          "Province",
                                          "ClubCode"))     
        
        finalFRame<-finalFRame[as.logical(finalFRame$Finals),]
        heatFRame<-na.omit(heatFRame)
        #<-heatFRame[!as.logical(heatFRame$Finals),]
        #         newHeatsFrame<-na.omit(FFrame[!as.logical(FFrame$Finals),])
        
        if(length(finalFRame[,1])>0) {
                FinalsStr <- "- Finals"
                splitFrame <- finalFRame#[!outlier(as.numeric(finalFRame$entrySeconds),logical=TRUE),]
                #--error condition---------------------------------------------------------------------------                
                if(length(splitFrame[,1])>2){
                        #                         if(unique(FrameX$LongEventString)[11]==LongEventString){browser()}
                        #                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$finalsSeconds),logical=TRUE),]
                        #                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$entrySeconds)-as.numeric(splitFrame$finalsSeconds),logical=TRUE),]
                        #         #                 splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$entrySeconds),logical=TRUE),]
                        
                        splitFrame$finalsSeconds<-as.numeric(splitFrame$finalsSeconds)
                        
                        splitFrame$tImprov <- tImprov(splitFrame$finalsSeconds, splitFrame$entrySeconds)
                        splitFrame$AgeGroup <-as.numeric(splitFrame$AgeGroup)
                        
                        #-Print to pdf -----------------------------------------------------
                        pdf(file=paste(LongEventString,FinalsStr,".pdf"))                   
                        #-------------------------------------------------------------------
                        
                        BubblePlot <- ggplot(splitFrame, aes(x=tImprov, y=finalsSeconds,size=AgeGroup,label=Name))+
                                geom_point(aes(colour=Province,title = "Province"), alpha=0.5) +  labs(title = paste(LongEventString, FinalsStr), x="Time improvement in Seconds", y="Time in Seconds") +#scale_size_area(min_size = 1,max_size = 10)+
                                geom_text(size=2, aes(label=Name),hjust=-0.15, vjust=0) +
                                scale_size(range = c(1, 15), name = "Relative Age")+  
                                guides(col = guide_legend(override.aes = list(shape = 15, size = 10))) +
                                theme(
                                        axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                                        axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                                        plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                                        axis.text = element_text(size = rel(1), colour = "Black"),
                                        #panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                                        #panel.grid.minor = element_blank(), 
                                        #panel.grid.major = element_blank(),
                                        #legend.title = element_text("This is it!"),
                                        #                plot.background = element_rect(fill = "white",colour = NA)
                                        plot.background = element_rect(fill = "transparent",colour = NA)
                                        #axis.text = element_text(colour="yellow")
                                ) +
                                guides(size=guide_legend(override.aes = list(fill="black", alpha=1)))#+
                        print(BubblePlot)
                        makeHeadnote(HeadNoteALL, color = "black")
                        dev.off()
                        
                }
        } 
        if(length(heatFRame[,1])>0) {
                FinalsStr <- "- Heats"
                #-------------------------------------------------------------------------------------------
                splitFrame <- heatFRame#[!outlier(as.numeric(heatFRame$entrySeconds),logical=TRUE),]
                #--error condition---------------------------------------------------------------------------                
                if(length(splitFrame[,1])>2){
                        #                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$heatSeconds),logical=TRUE),]
                        #                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$entrySeconds)-as.numeric(splitFrame$heatSeconds),logical=TRUE),]
                        #                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$entrySeconds),logical=TRUE),]
                        
                        #-------------------------------------------------------------------------------------------
                        splitFrame$heatSeconds<-as.numeric(splitFrame$heatSeconds)
                        
                        splitFrame$tImprov <- tImprov(splitFrame$heatSeconds, splitFrame$entrySeconds)
                        splitFrame$AgeGroup <-as.numeric(splitFrame$AgeGroup)
                        #-Print to PDF------------------------------------------------------
                        pdf(file=paste(LongEventString,FinalsStr,".pdf"))                        
                        
                        BubblePlot <- ggplot(splitFrame, aes(x=tImprov, y=heatSeconds,size=AgeGroup,label=Name))+
                                geom_point(aes(colour=Province,title = "Province"), alpha=0.5) +  labs(title = paste(LongEventString, FinalsStr), x="Time improvement in Seconds", y="Time in Seconds") +#scale_size_area(min_size = 1,max_size = 10)+
                                geom_text(size=2, aes(label=Name),hjust=-0.15, vjust=0) +
                                scale_size(range = c(1, 15), name = "Relative Age")+  
                                guides(col = guide_legend(override.aes = list(shape = 15, size = 10))) +
                                theme(
                                        axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                                        axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                                        plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                                        axis.text = element_text(size = rel(1), colour = "Black"),
                                        #panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                                        #panel.grid.minor = element_blank(), 
                                        #panel.grid.major = element_blank(),
                                        #legend.title = element_text("This is it!"),
                                        #                plot.background = element_rect(fill = "white",colour = NA)
                                        plot.background = element_rect(fill = "transparent",colour = NA)
                                        #axis.text = element_text(colour="yellow")
                                ) +
                                guides(size=guide_legend(override.aes = list(fill="black", alpha=1)))#+
                        print(BubblePlot)
                        makeHeadnote(HeadNoteALL, color = "black")
                        dev.off()
                        
                }
        }
        
        #scale_colour_brewer(palette="Set1")
        
        return(splitFrame)
}

ScreenbubblePlotFuncTrial <- function(LongEventString, FrameX){
        print(LongEventString)
        splitFrame <- FrameX
        newFFrame <- FrameX[FrameX$LongEventString==LongEventString,]
        splitFrame<-newFFrame
        
        heatFRame <-data.frame(newFFrame$LongEventString,
                               newFFrame$Name,
                               newFFrame$Finals,
                               as.numeric(newFFrame$entrySeconds),
                               as.numeric(newFFrame$heatSeconds),
                               as.numeric(newFFrame$AgeGroup),
                               newFFrame$Province,
                               newFFrame$ClubCode)
        
        heatFRame<-setNames(heatFRame,c("LongEventString",
                                        "Name",
                                        "Finals",
                                        "entrySeconds",
                                        "heatSeconds",
                                        "AgeGroup",
                                        "Province",
                                        "ClubCode"))
        
        finalFRame <-data.frame(newFFrame$LongEventString,
                                newFFrame$Name,
                                newFFrame$Finals,
                                as.numeric(newFFrame$entrySeconds),
                                as.numeric(newFFrame$finalsSeconds),
                                as.numeric(newFFrame$AgeGroup),
                                newFFrame$Province,
                                newFFrame$ClubCode)
        finalFRame<-setNames(finalFRame,c("LongEventString",
                                          "Name",
                                          "Finals",
                                          "entrySeconds",
                                          "finalsSeconds",
                                          "AgeGroup",
                                          "Province",
                                          "ClubCode"))     
        
        finalFRame<-finalFRame[as.logical(finalFRame$Finals),]
        heatFRame<-na.omit(heatFRame)
        #<-heatFRame[!as.logical(heatFRame$Finals),]
        #         newHeatsFrame<-na.omit(FFrame[!as.logical(FFrame$Finals),])
        

        if(length(heatFRame[,1])>0) {
                FinalsStr <- "Heats"
                #-------------------------------------------------------------------------------------------
                splitFrame <- heatFRame#[!outlier(as.numeric(heatFRame$entrySeconds),logical=TRUE),]
                #--error condition---------------------------------------------------------------------------                
                if(length(splitFrame[,1])>2){
                        #                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$heatSeconds),logical=TRUE),]
                        #                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$entrySeconds)-as.numeric(splitFrame$heatSeconds),logical=TRUE),]
                        #                         splitFrame <- splitFrame[!outlier(as.numeric(splitFrame$entrySeconds),logical=TRUE),]
                        
                        #-------------------------------------------------------------------------------------------
                        splitFrame$heatSeconds<-as.numeric(splitFrame$heatSeconds)
                        
                        splitFrame$tImprov <- tImprov(splitFrame$heatSeconds, splitFrame$entrySeconds)
                        splitFrame$AgeGroup <-as.numeric(splitFrame$AgeGroup)
                        BubblePlot <- ggplot(splitFrame[splitFrame$Final==TRUE,], aes(x=tImprov, y=Seconds,size=AgeGroup,label=Name))+
                                geom_point(aes(colour=Province,title = "Province"), alpha=0.5) +
                                labs(title = paste(LongEventString, FinalsStr), 
                                x="Time improvement in Seconds", y="Time in Seconds") +#scale_size_area(min_size = 1,max_size = 10)+
                                geom_text(size=2, aes(label=Name),hjust=-0.15, vjust=0) +
                                scale_size(range = c(1, 15), name = "Relative Age")+  
                                guides(col = guide_legend(override.aes = list(shape = 15, size = 10))) +
                                theme(
                                        axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                                        axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                                        plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                                        axis.text = element_text(size = rel(1), colour = "Black"),
                                        #panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                                        #panel.grid.minor = element_blank(), 
                                        #panel.grid.major = element_blank(),
                                        #legend.title = element_text("This is it!"),
                                        #                plot.background = element_rect(fill = "white",colour = NA)
                                        plot.background = element_rect(fill = "transparent",colour = NA)
                                        #axis.text = element_text(colour="yellow")
                                ) +
                                guides(size=guide_legend(override.aes = list(fill="black", alpha=1)))#+
                        print(BubblePlot)
                        makeHeadnote(HeadNoteALL, color = "black")
                }
        }
        
        #scale_colour_brewer(palette="Set1")
        
        return(splitFrame)
}




splitFrame<-galaFrame
#splitFrame<-splitFrame[splitFrame$Distance<400,]
#galaFrame
StrokeString<-unique(galaFrame$StrokeString)[1]
galaFrame$Gender

newFFrame <- splitFrame[splitFrame$StrokeString==StrokeString,]
splitFrame<-newFFrame[newFFrame$Gender=="M"&newFFrame$MClass==FALSE,]

ggplot(splitFrame, aes(x=tImprov, y=Seconds,size=AgeGroup,
                       label=Name))+
        geom_point(data= splitFrame,aes(title = "Province",colour=Distance), alpha=0.5)+
        #geom_point(aes(colour = Distance)) +
        labs(title = LongEventString, 
             x="Time improvement in Seconds", y="Time in Seconds") +#scale_size_area(min_size = 1,max_size = 10)+
  #      geom_text(size=2, aes(label=Name),hjust=-0.15, vjust=0) +
#        scale_size(range = c(1, 15), name = "Relative Age")+  
#        guides(col = guide_legend(override.aes = list(shape = 15, size = 5))) +
        theme(
                axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                axis.text = element_text(size = rel(1), colour = "Black"),
                #panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                #panel.grid.minor = element_blank(), 
                #panel.grid.major = element_blank(),
                #legend.title = element_text("This is it!"),
                #                plot.background = element_rect(fill = "white",colour = NA)
                plot.background = element_rect(fill = "transparent",colour = NA)
                #axis.text = element_text(colour="yellow")
        ) +
        guides(size=guide_legend(override.aes = list(fill="black", alpha=1)))#+

#-This calculates for equal distances-------------------------------------------------------
splitFrame<-galaFrame
splitFrame<-splitFrame[splitFrame$Distance==100,]
#galaFrame
#StrokeString<-unique(galaFrame$Distance)[1]
#galaFrame$Gender

splitFrame<-galaFrame
splitFrame<-splitFrame[splitFrame$Gender=="M"&splitFrame$MClass==FALSE,]
#The following graph shows time andd time improvement across all strokes per distance
ggplot(splitFrame, aes(x=tImprov, y=Seconds,size=AgeGroup,
                       label=Name))+
        geom_point(data= splitFrame,aes(title = "Province",colour=StrokeString), alpha=0.55)+
        #geom_point(aes(colour = Distance)) +
        labs(title = "", 
             x="Time improvement in Seconds", y="Time in Seconds") +#scale_size_area(min_size = 1,max_size = 10)+
        #      geom_text(size=2, aes(label=Name),hjust=-0.15, vjust=0) +
        #        scale_size(range = c(1, 15), name = "Relative Age")+  
        #        guides(col = guide_legend(override.aes = list(shape = 15, size = 5))) +
        theme(
                axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                axis.text = element_text(size = rel(1), colour = "Black"),
                #panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                #panel.grid.minor = element_blank(), 
                #panel.grid.major = element_blank(),
                #legend.title = element_text("This is it!"),
                #                plot.background = element_rect(fill = "white",colour = NA)
                plot.background = element_rect(fill = "transparent",colour = NA)
                #axis.text = element_text(colour="yellow")
        ) +
        guides(size=guide_legend(override.aes = list(fill="red", alpha=1)))#+

splitFrame<-galaFrame
splitFrame<-splitFrame[splitFrame$Gender=="M"&splitFrame$MClass==FALSE,]

ggplot(splitFrame, aes(x=tImprov, y=Seconds,size=Distance,
                       label=Name))+
        geom_point(data= splitFrame,aes(title = "Province",colour=StrokeString), alpha=0.35)+
        #geom_point(aes(colour = Distance)) +
        labs(title = "", 
             x="Time improvement in Seconds", y="Time in Seconds") +#scale_size_area(min_size = 1,max_size = 10)+
        #      geom_text(size=2, aes(label=Name),hjust=-0.15, vjust=0) +
        #        scale_size(range = c(1, 15), name = "Relative Age")+  
        #        guides(col = guide_legend(override.aes = list(shape = 15, size = 5))) +
        theme(
                axis.title.y = element_text(size = rel(1.25), colour = "Black"),
                axis.title.x = element_text(size = rel(1.25), colour = "Black"),
                plot.title = element_text(size = rel(1.5), colour = "Black", vjust=0.35),
                axis.text = element_text(size = rel(1), colour = "Black"),
                #panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                #panel.grid.minor = element_blank(), 
                #panel.grid.major = element_blank(),
                #legend.title = element_text("This is it!"),
                #                plot.background = element_rect(fill = "white",colour = NA)
                plot.background = element_rect(fill = "transparent",colour = NA)
                #axis.text = element_text(colour="yellow")
        ) +
        guides(size=guide_legend(override.aes = list(fill="red", alpha=1)))#+

#+        geom_point(data= splitFrame[splitFrame$Final==TRUE,],mapping=aes(fill=FALSE,title = "Province"), alpha=0.5, size=4)


FFrame <- FrameX[FrameX$LongEventString==LongEventString,]
splitFrame <- FFrame[!outlier(as.numeric(FFrame$entrySeconds)-as.numeric(FFrame$heatSeconds),logical=TRUE),]
splitFrame$entrySeconds<-as.numeric(splitFrame$entrySeconds)
splitFrame$heatSeconds<-as.numeric(splitFrame$heatSeconds)

splitFrame$tImprov <- tImprov(splitFrame$heatSeconds, splitFrame$entrySeconds)
splitFrame$AgeGroup <-as.numeric(splitFrame$AgeGroup)


finalFRame$heatSeconds<-as.numeric(finalFRame$heatSeconds)
tImprov(splitFrame$finalsSeconds, splitFrame$entrySeconds)
finalFRame$tImprov <- tImprov(splitFrame$finalsSeconds, splitFrame$entrySeconds)
finalFRame$AgeGroup <-as.numeric(finalFRame$AgeGroup)
splitFrame$AgeGroup <- as.numeric(splitFrame$AgeGroup)


#-This is the start of a new function ------------------------------------------------------
bubblePlotNew<-function(LongEventString, FrameX){
        print(LongEventString)
        newFFrame <- FrameX[FrameX$LongEventString==LongEventString,]
        
        
        heatFRame <-data.frame(newFFrame$LongEventString,
                               newFFrame$Name,
                               newFFrame$Finals,
                               as.numeric(newFFrame$entrySeconds),
                               as.numeric(newFFrame$heatSeconds),
                               as.numeric(newFFrame$AgeGroup),
                               newFFrame$Province,
                               newFFrame$ClubCode)
        
        heatFRame<-setNames(heatFRame,c("LongEventString",
                                        "Name",
                                        "Finals",
                                        "entrySeconds",
                                        "heatSeconds",
                                        "AgeGroup",
                                        "Province",
                                        "ClubCode"))
        
        finalFRame <-data.frame(newFFrame$LongEventString,
                                newFFrame$Name,
                                newFFrame$Finals,
                                as.numeric(newFFrame$entrySeconds),
                                as.numeric(newFFrame$finalsSeconds),
                                as.numeric(newFFrame$AgeGroup),
                                newFFrame$Province,
                                newFFrame$ClubCode)
        finalFRame<-setNames(finalFRame,c("LongEventString",
                                          "Name",
                                          "Finals",
                                          "entrySeconds",
                                          "finalsSeconds",
                                          "AgeGroup",
                                          "Province",
                                          "ClubCode"))     
        
        finalFRame<-finalFRame[as.logical(finalFRame$Finals),]
        heatFRame<-na.omit(heatFRame)
 
        
        
        ggplot(splitFrame, aes(x=tImprov, y=heatSeconds,size=AgeGroup,label=Name))+
       geom_point(aes(colour=Province,title = "Province"), alpha=0.5) +
        geom_point(data=finalFRame, aes(x=tImprov, y=finalsSeconds,size=AgeGroup,label=Name), alpha=0.5) 
       BubblePlot+geom_point(data=finalFRame, aes(x=tImprov, y=finalsSeconds,colour=factor(),size=AgeGroup,label=Name), alpha=0.5)        
}