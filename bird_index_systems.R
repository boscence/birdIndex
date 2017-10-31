##########
#set working directory
#########
  getwd()
  setwd("I:/MEL/CatchmentsTeam/BIS/Information Systems/Requests/2014_15/CGIS_0420 - Bird Index/Data")

  
##########
#load required libraries
#########
  library(plyr)
  library(data.table)
  library(ggplot2)
  library(reshape2)
  library(RODBC)
  library(reshape2)
  library(googleVis)
  library(maptools)
  library(sp)
  gpclibPermit()
  
##########
#Connect to Database and load in the expected species list. 
#The expected species list is used as the expected element of the observed over expected
#########
  setwd("I:/MEL/1. SHARED FOLDERS (Waterways Group) Inflo Migration/Cross Team Information/Investigations Programs/Birds/1 Data Management/11 Data/Birdlife Australia")
  #setwd("/home/ab/Projects/bird_index")

  db = file.path("MW_BirdDatabase_Feb15.accdb")
  channel <- odbcConnectAccess2007(db)  
  br =sqlQuery(channel, paste("select * from Tbl6_Expected_species"))
  close(channel)
  setwd("I:/MEL/CatchmentsTeam/BIS/Information Systems/Requests/2014_15/CGIS_0420 - Bird Index/Data")
  br = subset(br, br[,7] == 1)
  br = br[,c(1,9)]


##########
#add a logical field to the br dataset. This indicates if the species is used in the observed.
#alter names and clean whitespace to br in order to make the data cleaner.
#########  
  br$birduse = TRUE
  names(br) = c("commonname","serial","br")
  br$commonname = sub("[[:space:]]+$","",br$commonname)

##########
#Connect to Database and load in the expected observation/survey quesry
#This data contains the list of all species observed by observation. 
#########  
  #db = file.path("MW_BirdDatabase_Feb15.accdb")
  #channel <- odbcConnectAccess2007(db)
  #birds = sqlQuery(channel, paste("select * from BirdIndex"))
  #close(channel)

  birds = read.csv(file = "birdscsv.csv")

##########
#alter names and clean whitespace to br in order to make the data cleaner.
#fix two LGAs that are spelt a little differently
#########  
  names(birds) = tolower(names(birds))
  birds$commonname = sub("[[:space:]]+$","",birds$commonname)
  birds$lga[birds$lga == "Kingston(C)"] <- "Kingston (C)"
  birds$lga[birds$lga == "Frankston(C)"] <- "Frankston (C)"
  birds$lga = factor(birds$lga)


##########
#fix start dates for the birds dataset, add year and create year group.
#We do this because the dates do not come in cleanly and we need to extract years.
#The years are then grouped into the desired temporal grouping. In this case, in 5 year chunks going back from 2014  
#########  
  birds$datestart = as.Date(birds$datestart, format = "%d/%m/%Y")
  birds$startYear = as.integer(format(birds$datestart, "%Y"))
  birds$startYearGp = (cut(birds$startYear,seq(from = 2014,to = min(birds$startYear), by = -5)))
  birds$startYearGp = as.factor(gsub("\\(|\\]", "", birds$startYearGp))
  birds$startYearGp = gsub(pattern = ",", replacement = " to " , birds$startYearGp, ignore.case = FALSE, perl = FALSE,
                             fixed = FALSE, useBytes = FALSE)
  birds$startYearGp = gsub(pattern = "1984 to 1989", replacement = "1985 to 1989" , birds$startYearGp, ignore.case = FALSE, perl = FALSE,
                         fixed = FALSE, useBytes = FALSE)
  birds$startYearGp = gsub(pattern = "1989 to 1994", replacement = "1990 to 1994" , birds$startYearGp, ignore.case = FALSE, perl = FALSE,
                         fixed = FALSE, useBytes = FALSE)
  birds$startYearGp = gsub(pattern = "1994 to 1999", replacement = "1995 to 1999" , birds$startYearGp, ignore.case = FALSE, perl = FALSE,
                         fixed = FALSE, useBytes = FALSE)
  birds$startYearGp = gsub(pattern = "1999 to 2004", replacement = "2000 to 2004" , birds$startYearGp, ignore.case = FALSE, perl = FALSE,
                         fixed = FALSE, useBytes = FALSE)
  birds$startYearGp = gsub(pattern = "2004 to 2009", replacement = "2005 to 2009" , birds$startYearGp, ignore.case = FALSE, perl = FALSE,
                         fixed = FALSE, useBytes = FALSE)
  birds$startYearGp = gsub(pattern = "2009 to 2014", replacement = "2010 to 2014" , birds$startYearGp, ignore.case = FALSE, perl = FALSE,
                         fixed = FALSE, useBytes = FALSE)

##########
#Merge the birds dataset with the br dataset.
#This bring the 'birduse' logical field over.
#We now how a list of all species observed at each site and a field indicating if that species is expected
#########  
  birds = merge(birds, br, by.x = "commonname", by.y = "commonname")

##########
#Split the birds dataset bu lga.
#We do this to loop the bird index function on
#########  
  bsplit = split(birds, birds$system)

##########
#Create an empty dataframe
#We do this to create a home for the output of the bird index function
#########  
  bi = data.frame(YearGroup = factor(), system = factor(), indexscore = integer()) 
  
##########
#THIS IS THE BIRD INDEX FUNCTION.
#For every birds data set, split by lga, it
#Counts how many surveys were done at each lga and by each year group(surveysperLGA)
#Counts how many expected species were seen at each lga and year group (birdindex)
#Merges these two datasets (surveysperLGA and birdindex) based on same year group.
#Calculates the observed over expected
#Sums the observed over expected by lga and year group
#binds this to the epmty dataframe we made above (bi)
#########  
#########  
for (i in seq(bsplit)){
  mwSYSTEM = data.frame(unique(names(bsplit)))
  #Count how many surveys were done at each lga and by each year group(surveysperLGA)
  surveysperSYSTEM =  ddply(bsplit[[i]],.(system,startYearGp),here(summarize),
                         surveysAtSYSTEM = length(unique(surveyid)))
  SYSTEMname = names(bsplit[i])
  #Count how many expected species were seen at each lga and year group (birdindex)
  birdindex = ddply(bsplit[[i]],.(commonname[br == T],startYearGp),here(summarize),
                    surveys_seen_at=(length(unique(surveyid[br == T]))),
                    SYSTEM = SYSTEMname)
  #Merge surveysperLGA and birdindex based on same year group.
  birdindex = merge(surveysperSYSTEM, birdindex, by.x = "startYearGp", by.y = "startYearGp")
  
  #Calculate the observed over expected
  birdindex$OE = birdindex$surveys_seen_at/birdindex$surveysAtSYSTEM
  
  #Sums the observed over expected by lga and year group
  bindex = ddply(birdindex,.(system,startYearGp),here(summarize),
                 SYSTEM = unique(SYSTEMname),
                 indexscore = sum(OE),
                 seenat = sum(surveys_seen_at),
                 surveysatSYSTEM = sum(surveysAtSYSTEM))
  
  bindex = bindex[,c(2,3,4)]
  bindex = bindex[complete.cases(bindex),]
  names(bindex) = c("YearGroup", "SYSTEM", "indexscore")
  bindex = bindex[ which(bindex$YearGroup %in% c("1995 to 1999",
                                                 "2000 to 2004",
                                                 "2005 to 2009",
                                                 "2010 to 2014")), ]
  birdIndexPlot = qplot(YearGroup,indexscore,data = bindex,color = YearGroup,geom_point(colour=YearGroup),facets = ~SYSTEM,
                        xlab = "",
                        ylab = "Index Score") + geom_point(size = 5) + theme(legend.position="none") + scale_y_continuous(limits = c(0, 50)) + scale_x_discrete(limits = c("1995 to 1999",
                                                                                                                                                                           "2000 to 2004",
                                                                                                                                                                           "2005 to 2009",
                                                                                                                                                                           "2010 to 2014"))

  ggsave(birdIndexPlot,filename=paste("Avian Index - ",LGAname,".png",sep=""))
}

##########
#Now we re-run the algorithm to create the data for all LGAs  
#########  

for (i in seq(bsplit)){
  mwSYSTEM = data.frame(unique(names(bsplit)))
  #Count how many surveys were done at each lga and by each year group(surveysperLGA)
  surveysperSYSTEM =  ddply(bsplit[[i]],.(system,startYearGp),here(summarize),
                         surveysAtSYSTEM = length(unique(surveyid)))
  SYSTEMname = names(bsplit[i])
  #Count how many expected species were seen at each lga and year group (birdindex)
  birdindex = ddply(bsplit[[i]],.(commonname[br == T],startYearGp),here(summarize),
                    surveys_seen_at=(length(unique(surveyid[br == T]))),
                    SYSTEM = SYSTEMname)
  #Merge surveysperLGA and birdindex based on same year group.
  birdindex = merge(surveysperSYSTEM, birdindex, by.x = "startYearGp", by.y = "startYearGp")
  
  #Calculate the observed over expected
  birdindex$OE = birdindex$surveys_seen_at/birdindex$surveysAtSYSTEM
  
  #Sums the observed over expected by lga and year group
  bindex = ddply(birdindex,.(system,startYearGp),here(summarize),
                 SYSTEM = unique(SYSTEMname),
                 indexscore = sum(OE),
                 seenat = sum(surveys_seen_at),
                 surveysatSYSTEM = sum(surveysAtSYSTEM))
  
  bi = rbind(bi, bindex)
}
bi
#############
#Clean bi
#Remove unecessary fields
#Remove NAs
#############
  MWBirdIndex = bi[,c(2,3,4)]
  MWBirdIndex = MWBirdIndex[complete.cases(MWBirdIndex),]
  names(MWBirdIndex) = c("YearGroup", "SYSTEM", "indexscore")

#############
#Plot All
#############

birdIndexPlotAll = qplot(YearGroup,indexscore,data = MWBirdIndex,color = YearGroup,geom_point(colour=YearGroup),facets = ~SYSTEM,
                      main = "Avian Sub-Index for Melbourne Water's HWS Systems",
                      xlab = "Year Group",
                      ylab = "Index Score") + 
  geom_point(size = 2) + theme(legend.position="none") + 
  scale_y_continuous(limits = c(0, 30)) + 
  theme(axis.text.x=element_text(angle = -90,hjust = 0)) + 
  scale_x_discrete(limits = c("1995 to 1999","2000 to 2004","2005 to 2009","2010 to 2014"))

birdIndexPlotAll 



###Write the CSV
write.csv(MWBirdIndex, file = "AviansubIndexScoresSYSTEMS.csv")

###Write the chart
ggsave(birdIndexPlotAll,filename="AvianIndexPlot All SYSTEMS.png",width = 400, height = 400, units = "mm")

ggsave()
############################


MWBirdIndex = bi[,c(2,3,4)]
MWBirdIndex = MWBirdIndex[complete.cases(MWBirdIndex),]
names(MWBirdIndex) = c("YearGroup", "LGA", "indexscore")


biMotion = bi
biMotion = biMotion[complete.cases(biMotion),]
names(biMotion) = c("LGA", "YearGroup", "LGA2", "indexscore","SurveyswithExpected","SurveysatLGA")

unique(biMotion$year)
unique(biMotion$YearGroup)

biMotion$year = factor(biMotion$YearGroup, labels=c(1989,1994,1999,2004,2009,2014))
biMotion$year = as.Date(biMotion$year, format = "%Y")
biMotion$year = as.numeric(format(biMotion$year, "%Y"))
str(biMotion)


Motion=gvisMotionChart(biMotion, idvar="LGA", timevar="year",)
plot(Motion)



