###############################################################
##                                                           ##
##      R: Good practice - adding footnotes to graphics      ##
##                                                           ##
###############################################################

# basic information at the beginning of each script
scriptName <- "filename.R"
author <- "mh"
footnote <- paste(scriptName, format(Sys.time(), "%d %b %Y"),
                  author, sep=" / ")

FootNoteALL <-"Meet Results-SA Short Course Swimming Champions PMBG 7-10 August 2014"
HeadNoteALL <-"Meet Results-SA Short Course Swimming Champions PMBG 7-10 August 2014"

# default footnote is today's date, cex=.7 (size) and color
# is a kind of grey

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

