#This script uses the galaFrame setup
        initial.dir<-getwd()
        setwd("C:/rProgramming/SwimStats/SA Short Course Champs Aug PMBG 2014/Demographics")
# Files saved to C:\Users\Admin\Creative Cloud Files\Swimming\SA Short Course Champs Aug PMBG 2014\SwimmersData

#The following Graphs are created:
#               1.AthleteAgeDistribution.pdf
#               2.AthleteAgeDistribution TransparentBG2.pdf
#               3.AthleteAgeDistribution by Gender.pdf
#               4.Entry number by Event by AgeGroup - Men.pdf
#               5.Entry number by Event by Province - Men.pdf
#               6.Entry number by Event by AgeGroup - Women.pdf
#               7.Entry number by Event by Province - Women.pdf
#               8.Swimmer age by Club.pdf
#               9.Swimmer age by Province.pdf
#               10.Entries by Club by Province.pdf
#               11.All Athlete AgebyProvince.pdf


## This script connects to the latest "Swimming" db on this computer
        ch <- odbcConnect("Swimming")
        galaFrame <- sqlFetch(ch, "galaFrame")
        odbcClose(ch) 

uniqueSwimmerList <- unique(galaFrame$Swimmer)



SwimmersDetail <- lapply(X = uniqueSwimmerList, FUN = getSwimmersDetailX, eF=galaFrame)

SwimmersData <- ldply(SwimmersDetail, data.frame)

sqlSave(ch, SwimmersData, append = FALSE,
        colnames = FALSE,rownames = FALSE)
odbcClose(ch)

# dfF<-SwimmersData$Gender=="F"
# Provincedf<-count(SwimmersData$Province)
# Provincedf<-setNames(Provincedf,c("Affiliate","Swimmers"))
# Provincedf[order(-Provincedf$Swimmers),]
# Provincedf

# 1.AthleteAgeDistribution.pdf
        pdf(file=paste("AthleteAgeDistribution.pdf"))
        m <- ggplot(SwimmersData, aes(x=AgeGroup, fill=..count..))
        m + geom_histogram(binwidth=1) +
                labs(title = "Athlete Age Distribution",x = "Age",y = "Number")
        makeHeadnote(HeadNoteALL, color = "black")
        dev.off()
        print(m)

#Latest Technology:

pdf(file=paste("AthleteAgeDistribution TransparentBG2.pdf"))
p <- ggplot(SwimmersData, aes(x=AgeGroup, fill=..count..))
p + geom_histogram(binwidth=1) +
        labs(title = "Athlete Age Distribution",x = "Age",y = "Number",colour="yellow")+
        standardTheme
makeHeadnote(HeadNoteALL, color = "black")
#returns white background
# png('C:/Users/Admin/Creative Cloud Files/Swimming/tr_tst2.png',width=300,height=300,units="px",bg = "transparent")
# print(p)
dev.off()

##########################################################################################

pdf(file=paste("AthleteAgeDistribution by Gender.pdf"))
hist_cut <- ggplot(SwimmersData, aes(x=AgeGroup, fill=Gender))
hist_cut + geom_bar(binwidth=1) + labs(title = "Athlete Age Distribution",x = "Age",y = "Number") +
        standardTheme
makeHeadnote(HeadNoteALL, color = "black")
dev.off()

ggplot(SwimmersData, aes(AgeGroup, fill = Gender)) +
        geom_density(alpha = 0.5) + labs(title = "Age group and Gender")

ggplot(SwimmersData, aes(factor(Province), fill = factor(AgeGroup))) +
        geom_bar(binwidth=1)

# Work on 8Jan2015:
pdf(file="Entry number by Event by AgeGroup - Men.pdf")
ggplot(galaFrame[(!galaFrame$MClass)&(!galaFrame$Final)&(galaFrame$Gender=="M"),], aes(x=reorder(factor(EventString),table(factor(EventString))[factor(EventString)]), fill = factor(AgeGroup))) +
        geom_bar(binwidth=1)+ coord_flip() + labs(title = "Entries by Event - Men", x="Event", y="Entries") +
        scale_fill_discrete(name="Age Grouping") +
        theme(
                axis.title.y = element_text(size = rel(1), colour = "white", face ='bold'),
                axis.title.x = element_text(size = rel(1), colour = "white", face ='bold'),
                plot.title = element_text(size = rel(2), colour = "navy", face ='bold'),
                axis.text = element_text(size = rel(1), colour = "white", face ='bold'),
                panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                #                plot.background = element_rect(fill = "white",colour = NA)
                plot.background = element_rect(fill = "transparent",colour = NA)
                #axis.text = element_text(colour="yellow")
        )

makeHeadnote(HeadNoteALL, color = "black")
dev.off()

#-New Function-------------------------------------------------------------------------------
pdf(file="Entry number by Event by Province - Men.pdf")
ggplot(galaFrame[(!galaFrame$MClass)&(!galaFrame$Final)&(galaFrame$Gender=="M"),], aes(x=reorder(factor(EventString),table(factor(EventString))[factor(EventString)]), fill = factor(Province))) +
        geom_bar(binwidth=1)+ coord_flip()+ labs(title = "Entries by Event by Province - Men", x="Event", y="Entries") +
        scale_fill_discrete(name="Province") +
        theme(
                axis.title.y = element_text(size = rel(1), colour = "white", face ='bold'),
                axis.title.x = element_text(size = rel(1), colour = "white", face ='bold'),
                plot.title = element_text(size = rel(2), colour = "navy", face ='bold'),
                axis.text = element_text(size = rel(1), colour = "white", face ='bold'),
                panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                #                plot.background = element_rect(fill = "white",colour = NA)
                plot.background = element_rect(fill = "transparent",colour = NA)
                #axis.text = element_text(colour="yellow")
        )

makeHeadnote(HeadNoteALL, color = "black")
dev.off()

#-New Function ------------------------------------------------------------------------------
radius <- sqrt( galaFrame[(!galaFrame$MClass)&(galaFrame$Final)&(galaFrame$Gender=="F"),]$points/ pi )

pdf(file="Entry number by Event by AgeGroup - Women.pdf")
ggplot(galaFrame[(!galaFrame$MClass)&(!galaFrame$Final)&(galaFrame$Gender=="F"),], aes(x=reorder(factor(EventString),table(factor(EventString))[factor(EventString)]), fill = factor(AgeGroup))) +
        geom_bar(binwidth=1)+ coord_flip() + labs(title = "Entries by Event - Women", x="Event", y="Entries") +
        scale_fill_discrete(name="Age Grouping") +
        theme(
                axis.title.y = element_text(size = rel(1), colour = "white", face ='bold'),
                axis.title.x = element_text(size = rel(1), colour = "white", face ='bold'),
                plot.title = element_text(size = rel(2), colour = "navy", face ='bold'),
                axis.text = element_text(size = rel(1), colour = "white", face ='bold'),
                panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                #                plot.background = element_rect(fill = "white",colour = NA)
                plot.background = element_rect(fill = "transparent",colour = NA)
                #axis.text = element_text(colour="yellow")
        )
makeHeadnote(HeadNoteALL, color = "black")

dev.off()

pdf(file="Entry number by Event by Province - Women.pdf")
ggplot(galaFrame[(!galaFrame$MClass)&(!galaFrame$Finals)&(galaFrame$Gender=="F"),], aes(x=reorder(factor(EventString),table(factor(EventString))[factor(EventString)]), fill = factor(Province))) +
        geom_bar(binwidth=1)+ coord_flip() + labs(title = "Entries by Event - Women", x="Event", y="Entries") +
        scale_fill_discrete(name="Province")+
        theme(
                axis.title.y = element_text(size = rel(1), colour = "white", face ='bold'),
                axis.title.x = element_text(size = rel(1), colour = "white", face ='bold'),
                plot.title = element_text(size = rel(2), colour = "navy", face ='bold'),
                axis.text = element_text(size = rel(1), colour = "white", face ='bold'),
                panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                #                plot.background = element_rect(fill = "white",colour = NA)
                plot.background = element_rect(fill = "transparent",colour = NA)
                #axis.text = element_text(colour="yellow")
        )
makeHeadnote(HeadNoteALL, color = "black")
dev.off()


pdf(file="Entry number by Distance by Province - Women.pdf")
ggplot(galaFrame[(!galaFrame$MClass)&(!galaFrame$Finals),], aes(x=reorder(factor(DistanceString),table(factor(DistanceString))[factor(DistanceString)]), fill = factor(Gender))) +
        geom_bar(binwidth=1)+ coord_flip() + labs(title = "Entries by Distance", x="Event", y="Entries") +
        scale_fill_discrete(name="Gender")+
        theme(
                axis.title.y = element_text(size = rel(1), colour = "white", face ='bold'),
                axis.title.x = element_text(size = rel(1), colour = "white", face ='bold'),
                plot.title = element_text(size = rel(2), colour = "navy", face ='bold'),
                axis.text = element_text(size = rel(1), colour = "white", face ='bold'),
                panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                #                plot.background = element_rect(fill = "white",colour = NA)
                plot.background = element_rect(fill = "transparent",colour = NA)
                #axis.text = element_text(colour="yellow")
        )
makeHeadnote(HeadNoteALL, color = "black")
dev.off()


bubbleData<-aggregate(points~Province+Distance+Stroke+Gender, data=galaFrame[(as.logical(galaFrame$MClass))&(as.logical(galaFrame$Finals))&(galaFrame$Gender=="M"),],sum)
ggplot(bubbleData, aes(x=Province, y=Stroke, size=points, label=Province),guide=FALSE)+
        geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 15)+
        scale_x_discrete(name="Murders per 1,000 population")+
        scale_y_discrete(name="Burglaries per 1,000 population")+
        geom_text(size=4)+
        theme_bw()

bubbleData<-aggregate(points~AgeGroup+Distance+Stroke+Gender+Province, data=galaFrame[(!galaFrame$MClass)&(!galaFrame$Finals)&(galaFrame$Gender=="M"),],sum)
ggplot(bubbleData, aes(x=AgeGroup, y=Stroke, size=points, label=AgeGroup),guide=FALSE)+
        geom_point(aes(colour = factor(Province), fill=Province), shape=21)+ scale_size_area(max_size = 30)+
        scale_x_discrete(name="Murders per 1,000 population")+
        scale_y_discrete(name="Burglaries per 1,000 population")+
        geom_text(size=4)+
        theme_bw()

ggplot(bubbleData, aes(x=AgeGroup, y=Distance, size=points, label=AgeGroup),guide=FALSE)+
        geom_point(aes(colour = factor(Province), fill=Province), shape=21)+ scale_size_area(max_size = 30)+
        scale_x_discrete(name="Murders per 1,000 population")+
        scale_y_discrete(name="Burglaries per 1,000 population")+
        geom_text(size=4)+
        theme_bw()

ggplot(galaFrame[(!galaFrame$MClass)&(galaFrame$Finals)&(galaFrame$Gender=="F"),], aes(x=reorder(factor(EventString),table(factor(EventString))[factor(EventString)]), y=points, fill = factor(Province))) +
        geom_bar(binwidth=1, stat="identity")+ coord_flip() + labs(title = "Entries by Event - Women", x="Event", y="Entries") +
        scale_fill_discrete(name="Province")

ggplot(SwimmersData, aes(x=factor(Club), fill = factor(AgeGroup))) +
        geom_bar(binwidth=1)


ggplot(galaFrame, aes(x=factor(Distance), fill = factor(AgeGroup))) +
        geom_bar(binwidth=1)

#C:\Users\Admin\Creative Cloud Files\Swimming
pdf(file="All Athlete AgebyProvince.pdf")
ggplot(SwimmersData, aes(x=reorder(factor(Province),table(factor(Province))[factor(Province)]), fill = factor(AgeGroup))) +
        geom_bar(binwidth=1) + coord_flip() + labs(title = "Athlete age by Province", x="Province", y="Count") +
        scale_fill_discrete(name="Age Grouping")+
        theme(
                axis.title.y = element_text(size = rel(1), colour = "white", face ='bold'),
                axis.title.x = element_text(size = rel(1), colour = "white", face ='bold'),
                plot.title = element_text(size = rel(2), colour = "navy", face ='bold'),
                axis.text = element_text(size = rel(1), colour = "white", face ='bold'),
                panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                #                plot.background = element_rect(fill = "white",colour = NA)
                plot.background = element_rect(fill = "transparent",colour = NA)
                #axis.text = element_text(colour="yellow")
        )
makeHeadnote(HeadNoteALL, color = "black")
dev.off()



#C:\Users\Admin\Creative Cloud Files\Swimming
pdf(file="Women Athlete Age by Province.pdf")
ggplot(SwimmersData[dfF,], aes(x=reorder(factor(Province),table(factor(Province))[factor(Province)]), fill = factor(AgeGroup))) +
        geom_bar(binwidth=1) + coord_flip() + labs(title = "Women Athlete age by Province", x="Province", y="Count") +
        scale_fill_discrete(name="Age Grouping")
dev.off()


#C:\Users\Admin\Creative Cloud Files\Swimming
pdf(file="Male Athlete Age by Province.pdf")
ggplot(SwimmersData[!dfF,], aes(x=reorder(factor(Province),table(factor(Province))[factor(Province)]), fill = factor(AgeGroup))) +
        geom_bar(binwidth=1) + coord_flip() + labs(title = "Male Athlete age by Province", x="Province", y="Count") +
        scale_fill_discrete(name="Age Grouping")
dev.off()


#This unique graph plots the number of swimmers with age distribution by CLUB
pdf(file="Swimmer age by Club.pdf")
ggplot(SwimmersData, aes(x=reorder(factor(Club),table(factor(Club))[factor(Club)]), fill = factor(AgeGroup))) +
        geom_bar(binwidth=1) +  coord_flip() + labs(title = "Swimmer age by Club", x="Club", y="Count")+
        theme(
                axis.title.y = element_text(size = rel(1), colour = "white", face ='bold'),
                axis.title.x = element_text(size = rel(1), colour = "white", face ='bold'),
                plot.title = element_text(size = rel(2), colour = "navy", face ='bold'),
                axis.text.x = element_text(size = rel(1), colour = "white", face ='bold'),
                axis.text.y = element_text(size = rel(0.7), colour = "white", face ='bold'),
                panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                #                plot.background = element_rect(fill = "white",colour = NA)
                plot.background = element_rect(fill = "transparent",colour = NA)
                #axis.text = element_text(colour="yellow")
        )
makeHeadnote(HeadNoteALL, color = "black")
dev.off()


#This unique graph plots the number of swimmers with age distribution by CLUB
pdf(file="Swimmers by Club by Province.pdf")
g1<-ggplot(SwimmersData, aes(x=reorder(factor(Club),table(factor(Club))[factor(Club)]), fill = factor(Province))) +
        geom_bar(binwidth=1) +  coord_flip() + labs(title = "Swimmers by Club by Province", x="Club", y="Count") +  theme(legend.text = element_text(colour="blue", size = 16, face = "bold"))
g1 + scale_fill_discrete(name="Province")+
        theme(
                axis.title.y = element_text(size = rel(1), colour = "white", face ='bold'),
                axis.title.x = element_text(size = rel(1), colour = "white", face ='bold'),
                plot.title = element_text(size = rel(2), colour = "navy", face ='bold'),
                axis.text.x = element_text(size = rel(1), colour = "white", face ='bold'),
                axis.text.y = element_text(size = rel(0.7), colour = "white", face ='bold'),
                panel.background = element_rect(fill = "white",colour = NA), # or theme_blank()
                panel.grid.minor = element_blank(), 
                panel.grid.major = element_blank(),
                #                plot.background = element_rect(fill = "white",colour = NA)
                plot.background = element_rect(fill = "transparent",colour = NA)
                #axis.text = element_text(colour="yellow")
        )
makeHeadnote(HeadNoteALL, color = "black")
dev.off()

ggplot(SwimmersData, aes(x=reorder(factor(Club),table(factor(Club))[factor(Club)]), y=AgeGroup)) +
        geom_boxplot() +  coord_flip() 
ggplot(SwimmersData, aes(x=reorder(factor(Province),table(factor(Province))[factor(Province)]), y=AgeGroup)) +
        geom_boxplot() +  coord_flip() 

pdf(file="Athlete Age by Province.pdf")
ggplot(SwimmersData, aes(x=reorder(factor(Province),table(factor(Province))[factor(Province)]), y=AgeGroup, fill = factor(Province))) +
        geom_boxplot() +  coord_flip() + labs(title = "Age distribution by Province", x="Province", y="Age Group") +  theme(legend.text = element_text(colour="blue", size = 16, face = "bold"))+
        scale_fill_discrete(name="Province") + theme(
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
makeHeadnote(HeadNoteALL, color = "black")
dev.off()


pdf(file="Men Age by Province.pdf")
ggplot(SwimmersData[!dfF,], aes(x=reorder(factor(Province),table(factor(Province))[factor(Province)]), y=AgeGroup, fill = factor(Province))) +
        geom_boxplot() +  coord_flip() + labs(title = "Men Age distribution by Province", x="Province", y="Age Group") +  theme(legend.text = element_text(colour="blue", size = 16, face = "bold"))+
        scale_fill_discrete(name="Province") + theme(
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
makeHeadnote(HeadNoteALL, color = "black")
dev.off()



pdf(file="Women Age by Province.pdf")
ggplot(SwimmersData[dfF,], aes(x=reorder(factor(Province),table(factor(Province))[factor(Province)]), y=AgeGroup, fill = factor(Province))) +
        geom_boxplot() +  coord_flip() + labs(title = "Women Age distribution by Province", x="Province", y="Age Group") +  theme(legend.text = element_text(colour="blue", size = 16, face = "bold"))+
        scale_fill_discrete(name="Province") + theme(
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
makeHeadnote(HeadNoteALL, color = "black")
dev.off()

#This line returns the directory to its original state:
#setwd(initial.dir)
