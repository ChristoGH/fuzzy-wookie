# Get the name of the current current Dsirectory
        Initial.dir<-getwd()
# Set the name of the working directory:        
        setwd("C:/rProgramming/SwimStats/SA Short Course Champs Aug PMBG 2014/StrokeAnalysis")

        
        ## This script connects to the latest "Swimming" db on this computer
                ch <- odbcConnect("Swimming")
                galaFrame <- sqlFetch(ch, "galaFrame")
                odbcClose(ch) 
#                 correct the following fields:        
#                        Age,AgeGroup,Distance,entrySeconds,Seconds,Event,Lane,Position,tImprov,Points
        galaFrame<-numberField(galaFrame)      
                
# Define the following graph function:
#         This plots Time in Seconds y-axis vs Time Improvement in seconds on x-axis 
#         as functions of age (size) and province colour and heats and finals.
#         It puts the event desription on the Title bar.
#         A headnote gives the gala name.
        
newPDFbubblePlotFunc <- function(LongEventString, FrameX){
        
        # This file is used like this for the first entry:
        #       1.      newPDFbubblePlotFuncBlue(unique(galaFrame$LongEventString)[1], galaFrame)
        # or for the entire collection of events found in the Event Frame:
        #       2.      lapply(unique(galaFrame$LongEventString), newPDFbubblePlotFunc, galaFrame)
        
        # This function prints the Event string:
                print(LongEventString)
        # This line distills the Frame to those entries of LongEventString:     
                newFFrame <- FrameX[FrameX$LongEventString==LongEventString,]
        # This is a list of the fields in from galaFrame used
        #         tImprov
        #         Seconds
        #         AgeGroup
        #         EventType
        #         Name
        #         Province
        
        splitFrame <- newFFrame
        
        #-Set the file name and print to pdf -----------------------------------------------------
                pdf(file=paste(ShortCode, LongEventString,".pdf"))                   
        #-------------------------------------------------------------------
        
        BubblePlot <- ggplot(splitFrame, aes(x=tImprov, y=Seconds,size=AgeGroup,
                               shape=EventType,label=Name))+
                geom_point(data= splitFrame,aes(shape=EventType,title = "Province",colour=Province), alpha=0.5)+
                #geom_point(aes(colour = Province)) +
                labs(title = LongEventString, 
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
        dev.off()
}        

        
        
        newPDFbubblePlotFunc2 <- function(LongEventString, FrameX){
                
                # This file is used like this for the first entry:
                #       1.      newPDFbubblePlotFunc2(unique(galaFrame$LongEventString)[1], galaFrame)
                # or for the entire collection of events found in the Event Frame:
                #       2.      lapply(unique(galaFrame$LongEventString), newPDFbubblePlotFunc, galaFrame)
                
                # This function prints the Event string:
                print(LongEventString)
                # This line distills the Frame to those entries of LongEventString:     
                newFFrame <- FrameX[FrameX$LongEventString==LongEventString,]
                # This is a list of the fields in from galaFrame used
                #         tImprov
                #         Seconds
                #         AgeGroup
                #         EventType
                #         Name
                #         Province
                
                splitFrame <- newFFrame
                
                #-Set the file name and print to pdf -----------------------------------------------------
                pdf(file=paste(ShortCode, LongEventString,".pdf"))                   
                #-------------------------------------------------------------------
                
                BubblePlot <- ggplot(splitFrame, aes(x=tImprov, y=Seconds,size=AgeGroup,
                                                     shape=EventType,label=Name))+
                        geom_point(data= splitFrame,aes(shape=EventType,title = "Province",colour=Province), alpha=0.5)+
                        #geom_point(aes(colour = Province)) +
                        labs(title = LongEventString, 
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
                dev.off()
        }        
        
lapply(unique(FrameX$LongEventString), bubblePlotFunc, FrameX)
newPDFbubblePlotFuncBlue(unique(galaFrame$LongEventString)[1], galaFrame)

newPDFbubblePlotFuncBlue <- function(LongEventString, FrameX){
        print(LongEventString)
        newFFrame <- FrameX[FrameX$LongEventString==LongEventString,]
        #         tImprov
        #         Seconds
        #         AgeGroup
        #         EventType
        #         Name
        #         Province
        
        splitFrame <- newFFrame
        
        #-Print to pdf -----------------------------------------------------
        pdf(file=paste(ShortCode, LongEventString,".pdf"))                   
        #-------------------------------------------------------------------
        
        BubblePlot <- ggplot(splitFrame, mapping = aes(x=tImprov, y = Seconds,colour = AgeGroup,
                                             shape = EventType,label = Name))+
                geom_point(data= splitFrame,aes(shape=EventType,title = "Event"), alpha=0.5)+
                #geom_point(aes(colour = Province)) +
                labs(title = LongEventString, 
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
        dev.off()
}        
        testIn<-galaFrame[[galaFrame$Stroke==1&galaFrame$Distance==50,]$Name[is.element(galaFrame[galaFrame$Stroke==1&galaFrame$Distance==50,]$Name,
                   galaFrame[galaFrame$Stroke==2&galaFrame$Distance==50,]$Name)]
                   
subStrokeDB <- function(galaFrame, Stroke1, Stroke2, Distance1, Distance2, Final, MClass){                
        #                    Get the names of swimmers who swam Stroke1, Distance1
                                   T1 <- galaFrame[(galaFrame$Stroke==Stroke1&
                                                            galaFrame$Distance==Distance1),]$Name
                                   #            Get the names of swimmers who swam Stroke2, Distance2
                                   T2 <- galaFrame[(galaFrame$Stroke==Stroke2&galaFrame$Distance==Distance2),]$Name
                                   
                           nameSelection<-T1[is.element(T1,T2)]
        
        df1<-galaFrame[is.element(galaFrame$Name,nameSelection)&
                               (galaFrame$Stroke==Stroke1&
                                        galaFrame$Distance==Distance1&
                                        galaFrame$Final==Final&
                                        galaFrame$MClass==MClass),]
        
        # Name1<-galaFrame[is.element(galaFrame$Name,nameSelection),]$Name
        df2<-galaFrame[is.element(galaFrame$Name,nameSelection)&
                                    (galaFrame$Stroke==Stroke2&
                                             galaFrame$Distance==Distance2&
                                             galaFrame$Final==Final&
                                             galaFrame$MClass==MClass),]
        #df2$tImprov2 <- df2$tImprov
        #Here is the game changer
        m1<-merge(df1,df2,by="Swimmer")
return(m1)
}
m1 <- subStrokeDB(galaFrame, 
                  Stroke1=2, 
                  Stroke2=2, 
                  Distance1=50, 
                  Distance2=100, 
                  Final=FALSE,
                  MClass=FALSE,
                  InclMen=TRUE,
                  InclWomen=FALSE)

m1<-numberFieldmerged(m1)

#This section gives a graph comparing change in recorced time vs
#entry times:
#         fit <- lm(tImprov.y ~ tImprov.x, data = m1)
#         Cooksdist <- as.numeric(tail(row.names(outs[order(outs$CookD), ]), n))

       png(file=paste("Time Change Comparison -", 
                       m1$DistanceString.x[1], 
                       m1$StrokeString.x[1], "vs", 
                       m1$DistanceString.y[1], 
                       m1$StrokeString.y[1],
                       m1$EventType.y[1],
                       ".png"))        

        xlab <- paste("Change in Time (s) \n",
                m1$DistanceString.x[1],m1$StrokeString.x[1], "-", m1$EventType.x[1])
#         xlab <- expression(atop(paste(Delta, " Time (s)"), 
#                                 paste(m1$DistanceString.x[1], 
#                                       m1$StrokeString.x[1], 
#                                       "-", 
#                                       m1$EventType.x[1])))
        ylab <- paste("Change in Time (s) \n",
              m1$DistanceString.y[1],m1$StrokeString.y[1], "-", m1$EventType.y[1])
        titleLab <- paste("Time Change Comparison")
        compplot<-ggplot(m1, mapping = aes(x=tImprov.x, y=tImprov.y,size=AgeGroup.x,
                                 label=Name.x, colour=Gender.x))+
                geom_point(data= m1,aes(title = "Event"), alpha=0.5)+
                #geom_smooth(method=lm) 
                #geom_point(data= m1,aes(shape=EventType.x,name = "Event"), alpha=0.5)+
                #geom_point(aes(colour = Province)) +
                labs(title = titleLab,
                     x=xlab,
                     #x=paste("Improvement in Seconds \n",m1$DistanceString.x[1],m1$StrokeString.x[1], "-", m1$EventType.x[1]), 
                     y=ylab,
                     colour = "Gender") +#scale_size_area(min_size = 1,max_size = 10)+
                geom_text(size=2.5, aes(label=Name.x),hjust=-0.15, vjust=0, colour="black") +
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
                guides(size=guide_legend(override.aes = list(fill="black", alpha=1)))
        makeHeadnote(HeadNoteALL, color = "black")
        print(compplot)
        dev.off()
compplot+geom_smooth(method=lm, mapping = aes(x=Seconds.x, y=Seconds.y,size=AgeGroup.x,
                                                  label=Name.x, colour=Gender.x)) 
#-------------------------------------------------------------------------------------------

png(file=paste("Time Comparison -", 
               m1$DistanceString.x[1], 
               m1$StrokeString.x[1], "vs", 
               m1$DistanceString.y[1], 
               m1$StrokeString.y[1],
               m1$EventType.y[1],
               ".png"))        


                compplot<-ggplot(m1, mapping = aes(x=Seconds.x, y=Seconds.y,size=AgeGroup.x,
                                         label=Name.x, colour=Gender.x))+
                        geom_point(data= m1,aes(title = "Event"), alpha=0.5)+
                        #geom_point(data= m1,aes(shape=EventType.x,name = "Event"), alpha=0.5)+
                        #geom_point(aes(colour = Province)) +
                        labs(title = "Time Comparison", 
                             x=paste("Time (s) \n",m1$EventString.x[1]), 
                             y=paste("Time (s) \n",m1$EventString.y[1]),
                             colour = "Gender") +#scale_size_area(min_size = 1,max_size = 10)+
                        geom_text(size=2.5, aes(label=Name.x),hjust=-0.15, vjust=0, colour="black") +
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
                makeHeadnote(HeadNoteALL, color = "black")
                print(compplot)
                dev.off()

m1$Speed.x  <- as.numeric(as.character(m1$Distance.x))/as.numeric(as.character(m1$Seconds.x))
m1$Speed.y  <- as.numeric(as.character(m1$Distance.y))/as.numeric(as.character(m1$Seconds.y))

ggplot(m1, mapping = aes(x=Speed.x, y=Speed.y,size=AgeGroup.x,
                         label=Name.x, colour=Gender.x))+
        geom_point(data= m1,aes(title = "Event"), alpha=0.5)+
        #geom_point(aes(colour = Province)) +
        labs(title = "Distance Conversion - Same Stroke", 
             x=paste("Speed in meter/s \n",m1$EventString.x[1]), 
             y=paste("Speed in meter/s \n",m1$EventString.y[1]),
             colour = "Gender") +#scale_size_area(min_size = 1,max_size = 10)+
        geom_text(size=2.15, aes(label=Name.x),hjust=-0.15, vjust=0,colour="black") +
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
                #We do White here for publication:
                plot.background = element_rect(fill = "white",colour = NA)
                #axis.text = element_text(colour="yellow")
        ) +
        guides(size=guide_legend(override.aes = list(fill="black", alpha=1)))#+


compplot<-ggplot(m1, mapping = aes(x=Age.y, y=Seconds.y,size=AgeGroup.x,
                                   label=Name.x, colour=Gender.x))+
        geom_point(data= m1,aes(title = "Event"), alpha=0.5)+
        #geom_point(data= m1,aes(shape=EventType.x,name = "Event"), alpha=0.5)+
        #geom_point(aes(colour = Province)) +
        labs(title = "Time Comparison", 
             x=paste("Age (years)"), 
             y=paste("Time (s) \n",m1$EventString.y[1]),
             colour = "Gender") +#scale_size_area(min_size = 1,max_size = 10)+
        geom_text(size=2.5, aes(label=Name.x),hjust=-0.15, vjust=0, colour="black") +
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
makeHeadnote(HeadNoteALL, color = "black")
print(compplot)
dev.off()


Stroke1<-2 
Stroke2<-2 
Distance1<-50
Distance2<-200 
Final<-FALSE
MClass<-FALSE
InclMen<-TRUE
InclWomen<-TRUE
