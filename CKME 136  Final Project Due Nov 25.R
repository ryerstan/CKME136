
  

##################
# Load libraries #
##################

#install.packages('plyr')
#For count
if (!require(plyr)) install.packages('plyr')
library(plyr)

#For quantcut
if (!require(gtools)) install.packages('gtools')
library(gtools)

#For mean imputation
if (!require(zoo)) install.packages('zoo')
library(zoo)

#For plotting
if (!require(GGally)) install.packages('GGally')
library(GGally)

#For apriori
if (!require(arules)) install.packages('arules')
library(arules)

if (!require(arulesViz)) install.packages('arulesViz')
library("arulesViz")

#For Data Table
if (!require(data.table)) install.packages('data.table')
library(data.table)




#################
# Load datasets #
#################

setwd("C:/_Project/CKME136/Data")
Copy_1 <- read.csv("1_Copy.csv", header = TRUE,sep=",",strip.white=TRUE)
Copy_2 <- read.csv("2_Copy.csv", header = TRUE,sep=",",strip.white=TRUE)
Copy_3 <- read.csv("3_Copy.csv", header = TRUE,sep=",",strip.white=TRUE)
Copy_4 <- read.csv("4_Copy.csv", header = TRUE,sep=",",strip.white=TRUE)

################################################
# Remove unnecessary columns from each dataset #
################################################

#Rename columns in Copy_1
colnames(Copy_1)[1] <- "BoardID"
colnames(Copy_1)[4] <- "SchoolID"
#Remove unneeded columns from Copy_1
Copy_1 <- subset(Copy_1, select = -c(
  Board.Name, 
  School.Name, 
  Building.Suite,
  P.O..Box,
  Street,
  Municipality,
  City,
  Province,
  Postal.Code,
  Phone.Number,
  Fax.Number,
  School.Website,
  Board.Website,
  Extract.Date,
  Grade.Range,
  Latitude,
  Longitude,
  Percentage.of.Grade.9.Students.Achieving.the.Provincial.Standard.in.Academic.Mathematics,
  Change.in.Grade.9.Academic.Mathematics.Achievement.Over.Three.Years,
  Percentage.of.Grade.9.Students.Achieving.the.Provincial.Standard.in.Applied.Mathematics,
  Change.in.Grade.9.Applied.Mathematics.Achievement.Over.Three.Years,
  Percentage.of.Students.That.Passed.the.Grade.10.OSSLT.on.Their.First.Attempt,
  Change.in.Grade.10.OSSLT.Literacy.Achievement.Over.Three.Years,
  Change.in.Grade.3.Reading.Achievement.Over.Three.Years,
  Change.in.Grade.3.Writing.Achievement.Over.Three.Years,
  Change.in.Grade.3.Mathematics.Achievement.Over.Three.Years,
  Change.in.Grade.6.Reading.Achievement.Over.Three.Years,
  Change.in.Grade.6.Writing.Achievement.Over.Three.Years,
  Change.in.Grade.6.Mathematics.Achievement.Over.Three.Years,
  Extract.Date
))

#Rename columns in Copy_2
colnames(Copy_2)[1] <- "BoardID"
#Remove unnecessary columns from Copy_2
Copy_2 <- subset(Copy_2, select = -c(
  Board.Name,
  Total.Enrolment
))

#Rename columns in Copy_3
colnames(Copy_3)[3] <- "BoardID"
#Remove unnecessary columns from Copy_2
Copy_3 <- subset(Copy_3, select = c(
  BoardID,
  Board.Language,
  Elementary.Male.Educators,
  Elementary.Female.Educators
))


#Rename columns in Copy_4
colnames(Copy_4)[1] <- "BoardID"
colnames(Copy_4)[4] <- "SchoolID"
#Remove unnecessary columns from Copy_4
Copy_4 <- subset(Copy_4, select = c(
  BoardID,
  SchoolID,
  FSL.Core,
  FSL.Extended,
  FSL.Immersion
))

#########################################################################
# Merge datasets and extract Elementary School subset and write to file #
#########################################################################

#Common by School
Common_by_School <- merge(Copy_1,Copy_4, by="SchoolID")
colnames(Common_by_School)[2] <-"BoardID"
Common_by_School$BoardID.y <- NULL

#Common by Board
Final <- merge(Common_by_School, Copy_2, by="BoardID")
Final <- merge(Final, Copy_3, by="BoardID")

#Remove School Level not Elementary
Final <- subset(Final, School.Level=='Elementary')

#Remove column School.Level
Final$School.Level <- NULL

#Write Final to CSV
write.csv(Final,'Final.csv')
write.csv(Final,'Merged.csv')

##########################################################################################
# Preliminary removal of rows or columns #
##########################################################################################

#Remove Board Type, not needed as we have school type
Final$Board.Type <- NULL

#School.Type
count(Final,'School.Type')
Final <- subset(Final, School.Type != 'Protestant Separate')

#School.Special.Condition.Code
count(Final,'School.Special.Condition.Code')
# #Junior High School counts as Grade 6, this column can be removed
Final$School.Special.Condition.Code <- NULL

#School.Language
count(Final,'School.Language')
# #Only contains English, this column can be removed
Final$School.Language <- NULL

#Board.Language
count(Final,'Board.Language')
# #Only contains English, this column can be removed
Final$Board.Language <- NULL

#########################################################################
# Remove NAs by imputing averages by board or for independent variables #
#########################################################################

#Convert to NA all not numeric % in Percentage.of.Students.Whose.First.Language.Is.Not.English
#SKEW - ACCEPT
  Final$Percentage.of.Students.Whose.First.Language.Is.Not.English <- as.numeric(as.character(Final$Percentage.of.Students.Whose.First.Language.Is.Not.English))
a <- sort (Final$Percentage.of.Students.Whose.First.Language.Is.Not.English,decreasing=TRUE)   

#Count NA
  count(is.na(Final$Percentage.of.Students.Whose.First.Language.Is.Not.English))
  sum(is.na(Final$Percentage.of.Students.Whose.First.Language.Is.Not.English))/NROW(Final$Percentage.of.Students.Whose.First.Language.Is.Not.English)
  # 916 NA, 2147 numeric, 30% NA
  #Impute average for NA
  Final$Percentage.of.Students.Whose.First.Language.Is.Not.English <- zoo::na.aggregate(Final$Percentage.of.Students.Whose.First.Language.Is.Not.English, Final$BoardID, FUN=mean)
  #Calculate average value
  avg_temp <- mean(Final$Percentage.of.Students.Whose.First.Language.Is.Not.English, na.rm = TRUE) 
  #Replace NAN with average value
  Final$Percentage.of.Students.Whose.First.Language.Is.Not.English[is.nan(Final$Percentage.of.Students.Whose.First.Language.Is.Not.English)] <- avg_temp
  Final$Percentage.of.Students.Whose.First.Language.Is.Not.English <- round(Final$Percentage.of.Students.Whose.First.Language.Is.Not.English)
  
  
  x <- a
  h<-hist(x, breaks=10, col="red", xlab="Percentage.of.Students.Whose.First.Language.Is.Not.English",
          main="Histogram with Normal Curve")
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2)

  
#Convert to NA all not numeric % in Percentage.of.Students.Whose.First.Language.Is.Not.French
#SKEW - REJECT 
   Final$Percentage.of.Students.Whose.First.Language.Is.Not.French <- as.numeric(as.character(Final$Percentage.of.Students.Whose.First.Language.Is.Not.French))
  a <- sort (Final$Percentage.of.Students.Whose.First.Language.Is.Not.French,decreasing=TRUE)   
  
  #Count NA
  count(is.na(Final$Percentage.of.Students.Whose.First.Language.Is.Not.French))
  sum(is.na(Final$Percentage.of.Students.Whose.First.Language.Is.Not.French))/NROW(Final$Percentage.of.Students.Whose.First.Language.Is.Not.French)
  # 5 NA, 3058 numeric, 0.16% NA
  #Impute average for NA
  Final$Percentage.of.Students.Whose.First.Language.Is.Not.French <- zoo::na.aggregate(Final$Percentage.of.Students.Whose.First.Language.Is.Not.French, Final$BoardID, FUN=mean)
  #Calculate average value
  avg_temp <- mean(Final$Percentage.of.Students.Whose.First.Language.Is.Not.French, na.rm = TRUE) 
  #Replace NAN with average value
  Final$Percentage.of.Students.Whose.First.Language.Is.Not.French[is.nan(Final$Percentage.of.Students.Whose.First.Language.Is.Not.French)] <- avg_temp
  Final$Percentage.of.Students.Whose.First.Language.Is.Not.French <- round(Final$Percentage.of.Students.Whose.First.Language.Is.Not.French)
  
  x <- a
  h<-hist(x, breaks=10, col="red", xlab="Percentage.of.Students.Whose.First.Language.Is.Not.English",
          main="Histogram with Normal Curve")
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2)
  
  
##Convert to NA all not numeric % in Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country
## SKEW - ACCEPT
    Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country <- as.numeric(as.character(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country))
  a <- sort (Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country,decreasing=TRUE)   
  
   #Count NA
  count(is.na(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country))
  sum(is.na(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country))/NROW(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country)
  # 1607 NA, 1456 numeric, 52% NA
  #Impute average for NA
  Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country <- zoo::na.aggregate(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country, Final$BoardID, FUN=mean)
  #Calculate average value
  avg_temp <- mean(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country, na.rm = TRUE) 
  #Replace NAN with average value
  Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country[is.nan(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country)] <- avg_temp
  Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country <- round(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country)
  
  x <- a
  h<-hist(x, breaks=10, col="red", xlab="Percentage.of.Students.Whose.First.Language.Is.Not.English",
          main="Histogram with Normal Curve")
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2)
  
##Convert to NA all not numeric % in Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country
## SKEW - ACCEPT
    Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country <- as.numeric(as.character(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country))
  a <- sort (Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country,decreasing=TRUE)   
  
   #Count NA
  count(is.na(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country))
  sum(is.na(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country))/NROW(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country)
  # 1603 NA, 1460 numeric, 52% NA
  #Impute average for NA
  Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country <- zoo::na.aggregate(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country, Final$BoardID, FUN=mean)
  #Calculate average value
  avg_temp <- mean(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country, na.rm = TRUE) 
  #Replace NAN with average value
  Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country[is.nan(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country)] <- avg_temp
  Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country <- round(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country)
  
  
  x <- a
  h<-hist(x, breaks=10, col="red", xlab="Percentage.of.Students.Whose.First.Language.Is.Not.English",
          main="Histogram with Normal Curve")
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2)  
  
##Convert to NA all not numeric % in Percentage.of.Students.Receiving.Special.Education.Services
##SKEW ACCEPT
    Final$Percentage.of.Students.Receiving.Special.Education.Services <- as.numeric(as.character(Final$Percentage.of.Students.Receiving.Special.Education.Services))
  a <- sort (Final$Percentage.of.Students.Receiving.Special.Education.Services,decreasing=TRUE)   
  
    #Count NA
  count(is.na(Final$Percentage.of.Students.Receiving.Special.Education.Services))
  sum(is.na(Final$Percentage.of.Students.Receiving.Special.Education.Services))/NROW(Final$Percentage.of.Students.Receiving.Special.Education.Services)
  # 57 NA, 3006 numeric, 1.9% NA
  #Impute average for NA
  Final$Percentage.of.Students.Receiving.Special.Education.Services <- zoo::na.aggregate(Final$Percentage.of.Students.Receiving.Special.Education.Services, Final$BoardID, FUN=mean)
  #Calculate average value
  avg_temp <- mean(Final$Percentage.of.Students.Receiving.Special.Education.Services, na.rm = TRUE) 
  #Replace NAN with average value
  Final$Percentage.of.Students.Receiving.Special.Education.Services[is.nan(Final$Percentage.of.Students.Receiving.Special.Education.Services)] <- avg_temp
  Final$Percentage.of.Students.Receiving.Special.Education.Services <- round(Final$Percentage.of.Students.Receiving.Special.Education.Services)
  
  x <- a
  h<-hist(x, breaks=10, col="red", xlab="Percentage.of.Students.Whose.First.Language.Is.Not.English",
          main="Histogram with Normal Curve")
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2)    
  
##Convert to NA all not numeric % in Percentage.of.Students.Identified.as.Gifted, convert to binary
  Final$Percentage.of.Students.Identified.as.Gifted <- as.numeric(as.character(Final$Percentage.of.Students.Identified.as.Gifted))
  a <- sort (Final$Percentage.of.Students.Identified.as.Gifted,decreasing=TRUE)   
  
  count(is.na(Final$Percentage.of.Students.Identified.as.Gifted))
  sum(is.na(Final$Percentage.of.Students.Identified.as.Gifted))/NROW(Final$Percentage.of.Students.Identified.as.Gifted)
  # 2859 NA, 204 numeric, 93% NA
  Final$Percentage.of.Students.Identified.as.Gifted <- !is.na(Final$Percentage.of.Students.Identified.as.Gifted)
  Final$Percentage.of.Students.Identified.as.Gifted <- as.factor(Final$Percentage.of.Students.Identified.as.Gifted)
  names(Final)[names(Final) == "Percentage.of.Students.Identified.as.Gifted"] <- "Gifted"
  
  x <- a
  h<-hist(x, breaks=10, col="red", xlab="Percentage.of.Students.Whose.First.Language.Is.Not.English",
          main="Histogram with Normal Curve")
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2)   
  
    
##Convert to NA all not numeric % in Percentage.of.Children.Who.Live.in.Low.Income.Households
  Final$Percentage.of.Children.Who.Live.in.Low.Income.Households <- as.numeric(as.character(Final$Percentage.of.Children.Who.Live.in.Low.Income.Households))
  #Count NA
  count(is.na(Final$Percentage.of.Children.Who.Live.in.Low.Income.Households))
  sum(is.na(Final$Percentage.of.Children.Who.Live.in.Low.Income.Households))/NROW(Final$Percentage.of.Children.Who.Live.in.Low.Income.Households)
  # 77 NA, 2986 numeric, 2.5% NA
  #Impute average for NA
  Final$Percentage.of.Children.Who.Live.in.Low.Income.Households <- zoo::na.aggregate(Final$Percentage.of.Children.Who.Live.in.Low.Income.Households, Final$BoardID, FUN=mean)
  #Calculate average value
  avg_temp <- mean(Final$Percentage.of.Children.Who.Live.in.Low.Income.Households, na.rm = TRUE) 
  #Replace NAN with average value
  Final$Percentage.of.Children.Who.Live.in.Low.Income.Households[is.nan(Final$Percentage.of.Children.Who.Live.in.Low.Income.Households)] <- avg_temp
  Final$Percentage.of.Children.Who.Live.in.Low.Income.Households <- round(Final$Percentage.of.Children.Who.Live.in.Low.Income.Households)
  
  
##Convert to NA all not numeric % in Percentage.of.Students.Whose.Parents.Have.Some.University.Education
  Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education <- as.numeric(as.character(Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education))
  #Count NA
  count(is.na(Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education))
  sum(is.na(Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education))/NROW(Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education)
  # 51 NA, 3012 numeric, 1.7% NA
  #Impute average for NA
  Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education <- zoo::na.aggregate(Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education, Final$BoardID, FUN=mean)
  #Calculate average value
  avg_temp <- mean(Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education, na.rm = TRUE) 
  #Replace NAN with average value
  Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education[is.nan(Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education)] <- avg_temp
  Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education <- round(Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education)
  
  
##Convert to NA all not numeric % in Elementary.Male.Educators and replace NA with average
##Convert to NA all not numeric % in Elementary.Female.Educators and replace NA with average
##Replace both values at once because they will be treated as a ratio
  
  Final$Elementary.Male.Educators <- as.character(Final$Elementary.Male.Educators)
  Final$Elementary.Male.Educators = gsub(",","", Final$Elementary.Male.Educators)
  Final$Elementary.Male.Educators <- as.numeric(Final$Elementary.Male.Educators)
  #Count NA
  count(is.na(Final$Elementary.Male.Educators))
  sum(is.na(Final$Elementary.Male.Educators))/NROW(Final$Elementary.Male.Educators)
  # 2 NA, 3062 numeric, ~0% NA
  #Impute average for NA
  Final$Elementary.Male.Educators <- zoo::na.aggregate(Final$Elementary.Male.Educators, Final$BoardID, FUN=mean)
  
  Final$Elementary.Female.Educators <- as.character(Final$Elementary.Female.Educators)
  Final$Elementary.Female.Educators = gsub(",","", Final$Elementary.Female.Educators)
  Final$Elementary.Female.Educators <- as.numeric(Final$Elementary.Female.Educators)
  #Count NA
  count(is.na(Final$Elementary.Female.Educators))
  sum(is.na(Final$Elementary.Female.Educators))/NROW(Final$Elementary.Female.Educators)
  # 0 NA, 3064 numeric, 0% NA
  # No imputation necassary
  
    #Calculate average value
  avg_male <- mean(Final$Elementary.Male.Educators, na.rm = TRUE) 
  avg_female <- mean(Final$Elementary.Female.Educators, na.rm = TRUE) 
  #Replace NAN with average value for both Male and Female
  Final$Elementary.Female.Educators[is.nan(Final$Elementary.Male.Educators)] <- avg_female
  Final$Elementary.Male.Educators[is.nan(Final$Elementary.Male.Educators)] <- avg_male
  
  
  
  Final$Elementary.Male.Educators <- round(Final$Elementary.Male.Educators)
  Final$Elementary.Female.Educators <- round(Final$Elementary.Female.Educators)


  
##Convert to NA all not numeric % in Enrolment
  Final$Enrolment <- as.character(Final$Enrolment)
  Final$Enrolment = gsub(",","", Final$Enrolment)
  Final$Enrolment <- as.numeric(Final$Enrolment)
  #Count NA
  count(is.na(Final$Enrolment))
  sum(is.na(Final$Enrolment))/NROW(Final$Enrolment)
  # 7 NA, 3057 numeric, ~0% NA
  #Impute average for NA
  Final$Enrolment <- zoo::na.aggregate(Final$Enrolment, Final$BoardID, FUN=mean)
  #Calculate average value
  avg_temp <- mean(Final$Enrolment, na.rm = TRUE) 
  #Replace NAN with average value
  Final$Enrolment[is.nan(Final$Enrolment)] <- avg_temp

###########################################################################
# Find ratio of male to female teachers and add as a new column           #
###########################################################################    

for(i in 1: NROW(Final)){ 
  Final$Gender.Ratio.M.to.F[i] =
  Final$Elementary.Male.Educators[i] /
  Final$Elementary.Female.Educators[i]}

Final$Gender.Ratio.M.to.F <- round(Final$Gender.Ratio.M.to.F,digits=2)

Final$Elementary.Female.Educators <- NULL
Final$Elementary.Male.Educators <- NULL

#
  
###########################################################################
# Remove commas from enrolment columns and create 3 enrolment sum columns #
# Then remove unneeded original enrolment columns                         #
###########################################################################  
  
  Final$Junior.Kindergarten.Enrolment <- as.character(Final$Junior.Kindergarten.Enrolment)
  Final$Junior.Kindergarten.Enrolment = gsub(",","", Final$Junior.Kindergarten.Enrolment)
  Final$Junior.Kindergarten.Enrolment <- as.numeric(Final$Junior.Kindergarten.Enrolment)
  #Count NA
  count(is.na(Final$Junior.Kindergarten.Enrolment))
  sum(is.na(Final$Junior.Kindergarten.Enrolment))/NROW(Final$Junior.Kindergarten.Enrolment)
  # 0% NA
  
  Final$Kindergarten.Enrolment <- as.character(Final$Kindergarten.Enrolment)
  Final$Kindergarten.Enrolment = gsub(",","", Final$Kindergarten.Enrolment)
  Final$Kindergarten.Enrolment <- as.numeric(Final$Kindergarten.Enrolment)
  #Count NA
  count(is.na(Final$Kindergarten.Enrolment))
  sum(is.na(Final$Kindergarten.Enrolment))/NROW(Final$Kindergarten.Enrolment)
  # 0% NA
  
  Final$Grade.1.Enrolment <- as.character(Final$Grade.1.Enrolment)
  Final$Grade.1.Enrolment = gsub(",","", Final$Grade.1.Enrolment)
  Final$Grade.1.Enrolment <- as.numeric(Final$Grade.1.Enrolment)
  #Count NA
  count(is.na(Final$Grade.1.Enrolment))
  sum(is.na(Final$Grade.1.Enrolment))/NROW(Final$Grade.1.Enrolment)
  # 0% NA
  
  Final$Grade.2.Enrolment <- as.character(Final$Grade.2.Enrolment)
  Final$Grade.2.Enrolment = gsub(",","", Final$Grade.2.Enrolment)
  Final$Grade.2.Enrolment <- as.numeric(Final$Grade.2.Enrolment)
  #Count NA
  count(is.na(Final$Grade.2.Enrolment))
  sum(is.na(Final$Grade.2.Enrolment))/NROW(Final$Grade.2.Enrolment)
  # 0% NA
  
  Final$Grade.3.Enrolment <- as.character(Final$Grade.3.Enrolment)
  Final$Grade.3.Enrolment = gsub(",","", Final$Grade.3.Enrolment)
  Final$Grade.3.Enrolment <- as.numeric(Final$Grade.3.Enrolment)
  #Count NA
  count(is.na(Final$Grade.3.Enrolment))
  sum(is.na(Final$Grade.3.Enrolment))/NROW(Final$Grade.3.Enrolment)
  # 0% NA
  
  Final$Grade.4.Enrolment <- as.character(Final$Grade.4.Enrolment)
  Final$Grade.4.Enrolment = gsub(",","", Final$Grade.4.Enrolment)
  Final$Grade.4.Enrolment <- as.numeric(Final$Grade.4.Enrolment)
  #Count NA
  count(is.na(Final$Grade.4.Enrolment))
  sum(is.na(Final$Grade.4.Enrolment))/NROW(Final$Grade.4.Enrolment)
  # 0% NA
  
  Final$Grade.5.Enrolment <- as.character(Final$Grade.5.Enrolment)
  Final$Grade.5.Enrolment = gsub(",","", Final$Grade.5.Enrolment)
  Final$Grade.5.Enrolment <- as.numeric(Final$Grade.5.Enrolment)
  #Count NA
  count(is.na(Final$Grade.5.Enrolment))
  sum(is.na(Final$Grade.5.Enrolment))/NROW(Final$Grade.5.Enrolment)
  # 0% NA
  
  Final$Grade.6.Enrolment <- as.character(Final$Grade.6.Enrolment)
  Final$Grade.6.Enrolment = gsub(",","", Final$Grade.6.Enrolment)
  Final$Grade.6.Enrolment <- as.numeric(Final$Grade.6.Enrolment)
  #Count NA
  count(is.na(Final$Grade.6.Enrolment))
  sum(is.na(Final$Grade.6.Enrolment))/NROW(Final$Grade.6.Enrolment)
  # 0% NA
  
  Final$Grade.7.Enrolment <- as.character(Final$Grade.7.Enrolment)
  Final$Grade.7.Enrolment = gsub(",","", Final$Grade.7.Enrolment)
  Final$Grade.7.Enrolment <- as.numeric(Final$Grade.7.Enrolment)
  #Count NA
  count(is.na(Final$Grade.7.Enrolment))
  sum(is.na(Final$Grade.7.Enrolment))/NROW(Final$Grade.7.Enrolment)
  # 0% NA
  
  Final$Grade.8.Enrolment <- as.character(Final$Grade.8.Enrolment)
  Final$Grade.8.Enrolment = gsub(",","", Final$Grade.8.Enrolment)
  Final$Grade.8.Enrolment <- as.numeric(Final$Grade.8.Enrolment)
  #Count NA
  count(is.na(Final$Grade.8.Enrolment))
  sum(is.na(Final$Grade.8.Enrolment))/NROW(Final$Grade.8.Enrolment)
  # 0% NA
  
  Final$Enrol_sum_all <- 0
  Final$Enrol_sum_JKto3 <- 0
  Final$Enrol_sum_4to6 <-0
  
 for(i in 1: NROW(Final)){ 
  Final$Enrol_sum_all[i] =
  Final$Junior.Kindergarten.Enrolment[i]+
  Final$Kindergarten.Enrolment[i]+
  Final$Grade.1.Enrolment[i]+
  Final$Grade.2.Enrolment[i]+
  Final$Grade.3.Enrolment[i]+
  Final$Grade.4.Enrolment[i]+
  Final$Grade.5.Enrolment[i]+
  Final$Grade.6.Enrolment[i]+
  Final$Grade.7.Enrolment[i]+
  Final$Grade.8.Enrolment}
  
for(i in 1: NROW(Final)){ 
  Final$Enrol_sum_JKto3[i] =
    Final$Junior.Kindergarten.Enrolment[i]+
    Final$Kindergarten.Enrolment[i]+
    Final$Grade.1.Enrolment[i]+
    Final$Grade.2.Enrolment[i]+
    Final$Grade.3.Enrolment[i]}
  
for(i in 1: NROW(Final)){ 
  Final$Enrol_sum_4to6[i] =
    Final$Grade.4.Enrolment[i]+
    Final$Grade.5.Enrolment[i]+
    Final$Grade.6.Enrolment[i]}

Final$Junior.Kindergarten.Enrolment <- NULL
Final$Kindergarten.Enrolment <- NULL
Final$Grade.1.Enrolment <- NULL
Final$Grade.2.Enrolment <- NULL
Final$Grade.3.Enrolment <- NULL
Final$Grade.4.Enrolment <- NULL
Final$Grade.5.Enrolment <- NULL
Final$Grade.6.Enrolment <- NULL
Final$Grade.7.Enrolment <- NULL
Final$Grade.8.Enrolment <- NULL
  
###########################################################################
# Remove rows with NAs in dependent variable columns                      #
# NOTE: this occurs after imputing values of independent variable columns #
###########################################################################
  
  
## Remove all NA records from EQAO results (dependent variable)
#Convert to NA all EQAO results that are not numeric % (Grade 3 and Grade 6)
Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Reading <- as.numeric(sub("%","",Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Reading))
Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Writing <- as.numeric(sub("%","",Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Writing))
Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Mathematics <- as.numeric(sub("%","",Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Mathematics))
Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Reading <- as.numeric(sub("%","",Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Reading))
Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Writing <- as.numeric(sub("%","",Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Writing))
Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Mathematics <- as.numeric(sub("%","",Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Mathematics))

NROW(Final)
#3063 Rows

Final <- Final[!is.na(Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Reading),]
Final <- Final[!is.na(Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Writing),]
Final <- Final[!is.na(Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Mathematics),]
Final <- Final[!is.na(Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Reading),]
Final <- Final[!is.na(Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Writing),]
Final <- Final[!is.na(Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Mathematics),]

Final$School.Type <- as.character(Final$School.Type) 
Final <- Final[(Final$School.Type == 'Public' | 'Catholic')]
Final$School.Type <-as.factor(Final$School.Type)

#Final$Gifted <- NULL


NROW(Final)
#2390 Rows



##########################################################################################
# Write the cleaned up dataframe to file for future use
##########################################################################################

Write Clean to CSV
write.csv(Final,'Clean.csv')

#Write Clean to TXT, tab-delimited
#write.table(Final,"Clean.txt",sep="\t",row.names=FALSE)

############################################################################
# Scale non-percentage numeric values as a precursor to quantile factoring #
############################################################################

Final$Enrolment <- scale(Final$Enrolment)
Final$Enrol_sum_all <- scale(Final$Enrol_sum_all)
Final$Enrol_sum_JKto3 <- scale(Final$Enrol_sum_JKto3)
Final$Enrol_sum_4to6 <- scale(Final$Enrol_sum_4to6)

#Update certain columns
Final$BoardID <- NULL
Final$SchoolID <- NULL
Final$School.Type <-as.factor(Final$School.Type)

#########################################
# Quantile factorize all numeric values #
#########################################

Final$Enrolment <- quantcut(Final$Enrolment,5)
Final$Enrolment <- as.factor(Final$Enrolment)
levels(Final$Enrolment) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Students.Whose.First.Language.Is.Not.English <- quantcut(Final$Percentage.of.Students.Whose.First.Language.Is.Not.English,5)
Final$Percentage.of.Students.Whose.First.Language.Is.Not.English <- as.factor(Final$Percentage.of.Students.Whose.First.Language.Is.Not.English)
levels(Final$Percentage.of.Students.Whose.First.Language.Is.Not.English) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Students.Whose.First.Language.Is.Not.French <- quantcut(Final$Percentage.of.Students.Whose.First.Language.Is.Not.French,5)
Final$Percentage.of.Students.Whose.First.Language.Is.Not.French <- as.factor(Final$Percentage.of.Students.Whose.First.Language.Is.Not.French)
levels(Final$Percentage.of.Students.Whose.First.Language.Is.Not.French) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country <- quantcut(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country,5)
Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country <- as.factor(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country)
levels(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country <- quantcut(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country,5)
Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country <- as.factor(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country)
levels(Final$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Students.Receiving.Special.Education.Services <- quantcut(Final$Percentage.of.Students.Receiving.Special.Education.Services,5)
Final$Percentage.of.Students.Receiving.Special.Education.Services <- as.factor(Final$Percentage.of.Students.Receiving.Special.Education.Services)
levels(Final$Percentage.of.Students.Receiving.Special.Education.Services) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Reading <- quantcut(Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Reading,5)
Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Reading <- as.factor(Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Reading)
levels(Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Reading) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Writing <- quantcut(Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Writing,5)
Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Writing <- as.factor(Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Writing)
levels(Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Writing) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Mathematics <- quantcut(Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Mathematics,5)
Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Mathematics <- as.factor(Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Mathematics)
levels(Final$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Mathematics) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Reading <- quantcut(Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Reading,5)
Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Reading <- as.factor(Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Reading)
levels(Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Reading) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Writing <- quantcut(Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Writing,5)
Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Writing <- as.factor(Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Writing)
levels(Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Writing) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Mathematics <- quantcut(Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Mathematics,5)
Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Mathematics <- as.factor(Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Mathematics)
levels(Final$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Mathematics) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Children.Who.Live.in.Low.Income.Households <- quantcut(Final$Percentage.of.Children.Who.Live.in.Low.Income.Households,5)
Final$Percentage.of.Children.Who.Live.in.Low.Income.Households <- as.factor(Final$Percentage.of.Children.Who.Live.in.Low.Income.Households)
levels(Final$Percentage.of.Children.Who.Live.in.Low.Income.Households) <- c("Lowest","Low","Medium","High","Highest")

Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education <- quantcut(Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education,5)
Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education <- as.factor(Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education)
levels(Final$Percentage.of.Students.Whose.Parents.Have.Some.University.Education) <- c("Lowest","Low","Medium","High","Highest")

Final$Gender.Ratio.M.to.F <- quantcut(Final$Gender.Ratio.M.to.F,5)
Final$Gender.Ratio.M.to.F <- as.factor(Final$Gender.Ratio.M.to.F)
levels(Final$Gender.Ratio.M.to.F) <- c("Lowest","Low","Medium","High","Highest")

Final$Enrol_sum_all <- quantcut(Final$Enrol_sum_all,5)
Final$Enrol_sum_all <- as.factor(Final$Enrol_sum_all)
levels(Final$Enrol_sum_all) <- c("Lowest","Low","Medium","High","Highest")

Final$Enrol_sum_JKto3 <- quantcut(Final$Enrol_sum_JKto3,5)
Final$Enrol_sum_JKto3 <- as.factor(Final$Enrol_sum_JKto3)
levels(Final$Enrol_sum_JKto3) <- c("Lowest","Low","Medium","High","Highest")

Final$Enrol_sum_4to6 <- quantcut(Final$Enrol_sum_4to6,5)
Final$Enrol_sum_4to6 <- as.factor(Final$Enrol_sum_4to6)
levels(Final$Enrol_sum_4to6) <- c("Lowest","Low","Medium","High","Highest")

##########################################################################################
# Write the factored dataframe to file for future use
##########################################################################################

#Write Final to CSV
write.csv(Final,'Factored.csv')
#Write Clean to TXT, tab-delimited
#write.table(Final,"Factored.txt",sep="\t",row.names=FALSE)

################
## APRIORI
################


#Create transactions dataset from Final
trans_Final <- as (Final, "transactions")

#HIGHEST PERFORMANCE



# Identify rules that related to dependent (RHS) columns per category
rules_3R <- apriori (data=trans_Final, parameter=list (supp=0.05,conf = 0.2), appearance = list (default="lhs",rhs="Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Reading=Highest"), control = list (verbose=F)) 
rules_3W <- apriori (data=trans_Final, parameter=list (supp=0.05,conf = 0.2), appearance = list (default="lhs",rhs="Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Writing=Highest"), control = list (verbose=F)) 
rules_3M <- apriori (data=trans_Final, parameter=list (supp=0.05,conf = 0.2), appearance = list (default="lhs",rhs="Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Mathematics=Highest"), control = list (verbose=F)) 
rules_6R <- apriori (data=trans_Final, parameter=list (supp=0.05,conf = 0.2), appearance = list (default="lhs",rhs="Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Reading=Highest"), control = list (verbose=F)) 
rules_6W <- apriori (data=trans_Final, parameter=list (supp=0.05,conf = 0.2), appearance = list (default="lhs",rhs="Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Writing=Highest"), control = list (verbose=F)) 
rules_6M <- apriori (data=trans_Final, parameter=list (supp=0.05,conf = 0.2), appearance = list (default="lhs",rhs="Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Mathematics=Highest"), control = list (verbose=F)) 

plot(rules_3R, measure=c("support","lift"), shading="confidence");


# Sort rules by Lift, decending
rules_bylift_3R <- sort (rules_3R, by="lift", decreasing=TRUE) 
rules_bylift_3W <- sort (rules_3W, by="lift", decreasing=TRUE) 
rules_bylift_3M <- sort (rules_3M, by="lift", decreasing=TRUE) 
rules_bylift_6R <- sort (rules_6R, by="lift", decreasing=TRUE) 
rules_bylift_6W <- sort (rules_6W, by="lift", decreasing=TRUE) 
rules_bylift_6M <- sort (rules_6M, by="lift", decreasing=TRUE) 

#Save Extract LHS per category (contains support, confidence and lift for reference)
write(rules_bylift_3R, file = "lhs_3R_highest.csv", sep = ",", quote = TRUE, row.names = FALSE)
write(rules_bylift_3W, file = "lhs_3W_highest.csv", sep = ",", quote = TRUE, row.names = FALSE)
write(rules_bylift_3M, file = "lhs_3M_highest.csv", sep = ",", quote = TRUE, row.names = FALSE)
write(rules_bylift_6R, file = "lhs_6R_highest.csv", sep = ",", quote = TRUE, row.names = FALSE)
write(rules_bylift_6W, file = "lhs_6W_highest.csv", sep = ",", quote = TRUE, row.names = FALSE)
write(rules_bylift_6M, file = "lhs_6M_highest.csv", sep = ",", quote = TRUE, row.names = FALSE)


#Extract LHS per category
lhs_3R <- as(lhs(rules_bylift_3R), "list")
lhs_3W <- as(lhs(rules_bylift_3W), "list")
lhs_3M <- as(lhs(rules_bylift_3M), "list")
lhs_6R <- as(lhs(rules_bylift_6R), "list")
lhs_6W <- as(lhs(rules_bylift_6W), "list")
lhs_6M <- as(lhs(rules_bylift_6M), "list")

#Flatten and label each dataframe per category
f_3R <- count(unlist(lhs_3R))
colnames(f_3R) <- c("Factor", "Freq")
f_3W <- count(unlist(lhs_3W))
colnames(f_3W) <- c("Factor", "Freq")
f_3M <- count(unlist(lhs_3M))
colnames(f_3M) <- c("Factor", "Freq")
f_6R <- count(unlist(lhs_6R))
colnames(f_6R) <- c("Factor", "Freq")
f_6W <- count(unlist(lhs_6W))
colnames(f_6W) <- c("Factor", "Freq")
f_6M <- count(unlist(lhs_6M))
colnames(f_6M) <- c("Factor", "Freq")

#Create a flat combined LHS list 
lhs_combo <- c(lhs_3R,lhs_3W,lhs_3M,lhs_6R,lhs_6W,lhs_6M)
f_lhs <- count(unlist(lhs_combo))
colnames(f_lhs) <- c("Factor", "Freq")
f_lhs <- f_lhs[order(f_lhs$Freq, decreasing = TRUE),]


#Merge all flat lists with outer join
together <- merge(f_lhs, f_3R, by="Factor",all=TRUE)
together <- merge(together,f_3W, by="Factor", all=TRUE)
together <- merge(together,f_3M, by="Factor", all=TRUE)
together <- merge(together,f_6R, by="Factor", all=TRUE)
together <- merge(together,f_6W, by="Factor", all=TRUE)
together <- merge(together,f_6M, by="Factor", all=TRUE)
colnames(together) <- c("Factor", "Total Freq","3R Freq","3W Freq","3M Freq","6R Freq","6W Freq","6M Freq")
#together[is.na(together)] <- 0

together_highest <- together[order(together$`Total Freq`, decreasing = TRUE),]
together_highest$Set <- 'Highest'

#LOWEST PERFORMANCE


# Identify rules that related to dependent (RHS) columns per category
rules_3R <- apriori (data=trans_Final, parameter=list (supp=0.05,conf = 0.2), appearance = list (default="lhs",rhs="Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Reading=Lowest"), control = list (verbose=F)) 
rules_3W <- apriori (data=trans_Final, parameter=list (supp=0.05,conf = 0.2), appearance = list (default="lhs",rhs="Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Writing=Lowest"), control = list (verbose=F)) 
rules_3M <- apriori (data=trans_Final, parameter=list (supp=0.05,conf = 0.2), appearance = list (default="lhs",rhs="Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Mathematics=Lowest"), control = list (verbose=F)) 
rules_6R <- apriori (data=trans_Final, parameter=list (supp=0.05,conf = 0.2), appearance = list (default="lhs",rhs="Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Reading=Lowest"), control = list (verbose=F)) 
rules_6W <- apriori (data=trans_Final, parameter=list (supp=0.05,conf = 0.2), appearance = list (default="lhs",rhs="Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Writing=Lowest"), control = list (verbose=F)) 
rules_6M <- apriori (data=trans_Final, parameter=list (supp=0.05,conf = 0.2), appearance = list (default="lhs",rhs="Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Mathematics=Lowest"), control = list (verbose=F)) 

# Sort rules by Lift, decending


rules_bylift_3R <- sort (rules_3R, by="lift", decreasing=TRUE) 
rules_bylift_3W <- sort (rules_3W, by="lift", decreasing=TRUE) 
rules_bylift_3M <- sort (rules_3M, by="lift", decreasing=TRUE) 
rules_bylift_6R <- sort (rules_6R, by="lift", decreasing=TRUE) 
rules_bylift_6W <- sort (rules_6W, by="lift", decreasing=TRUE) 
rules_bylift_6M <- sort (rules_6M, by="lift", decreasing=TRUE) 

#Save Extract LHS per category (contains support, confidence and lift for reference)
write(rules_bylift_3R, file = "lhs_3R_lowest.csv", sep = ",", quote = TRUE, row.names = FALSE)
write(rules_bylift_3W, file = "lhs_3W_lowest.csv", sep = ",", quote = TRUE, row.names = FALSE)
write(rules_bylift_3M, file = "lhs_3M_lowest.csv", sep = ",", quote = TRUE, row.names = FALSE)
write(rules_bylift_6R, file = "lhs_6R_lowest.csv", sep = ",", quote = TRUE, row.names = FALSE)
write(rules_bylift_6W, file = "lhs_6W_lowest.csv", sep = ",", quote = TRUE, row.names = FALSE)
write(rules_bylift_6M, file = "lhs_6M_lowest.csv", sep = ",", quote = TRUE, row.names = FALSE)

#Extract LHS per category
lhs_3R <- as(lhs(rules_bylift_3R), "list")
lhs_3W <- as(lhs(rules_bylift_3W), "list")
lhs_3M <- as(lhs(rules_bylift_3M), "list")
lhs_6R <- as(lhs(rules_bylift_6R), "list")
lhs_6W <- as(lhs(rules_bylift_6W), "list")
lhs_6M <- as(lhs(rules_bylift_6M), "list")

#Flatten and label each dataframe per category
f_3R <- count(unlist(lhs_3R))
colnames(f_3R) <- c("Factor", "Freq")
f_3W <- count(unlist(lhs_3W))
colnames(f_3W) <- c("Factor", "Freq")
f_3M <- count(unlist(lhs_3M))
colnames(f_3M) <- c("Factor", "Freq")
f_6R <- count(unlist(lhs_6R))
colnames(f_6R) <- c("Factor", "Freq")
f_6W <- count(unlist(lhs_6W))
colnames(f_6W) <- c("Factor", "Freq")
f_6M <- count(unlist(lhs_6M))
colnames(f_6M) <- c("Factor", "Freq")

#Create a flat combined LHS list 
lhs_combo <- c(lhs_3R,lhs_3W,lhs_3M,lhs_6R,lhs_6W,lhs_6M)
f_lhs <- count(unlist(lhs_combo))
colnames(f_lhs) <- c("Factor", "Freq")


  #Merge all flat lists with outer join
together <- merge(f_lhs, f_3R, by="Factor",all=TRUE)
together <- merge(together,f_3W, by="Factor", all=TRUE)
together <- merge(together,f_3M, by="Factor", all=TRUE)
together <- merge(together,f_6R, by="Factor", all=TRUE)
together <- merge(together,f_6W, by="Factor", all=TRUE)
together <- merge(together,f_6M, by="Factor", all=TRUE)
colnames(together) <- c("Factor", "Total Freq","3R Freq","3W Freq","3M Freq","6R Freq","6W Freq","6M Freq")
#together[is.na(together)] <- 0

together_lowest <- together[order(together$`Total Freq`, decreasing = TRUE),]
together_lowest$Set <- 'Lowest'


#MAKE FINAL FRAME

# Find differences between Highest and Lowest, Inner Join Highest and Lowest with differences
oo <- c(setdiff(together_highest$Fact, together_lowest$Fact), setdiff(together_lowest$Fact, together_highest$Fact))
oo <- as.data.frame(oo)
colnames(oo) <- c("Factor")
together_highest <- merge(oo, together_highest, by="Factor")
together_lowest <- merge(oo,together_lowest, by="Factor")

#Sort by Frequency
together_highest <- together_highest[order(together_highest$`Total Freq`, decreasing = TRUE),]
together_lowest <- together_lowest[order(together_lowest$`Total Freq`, decreasing = TRUE),]

#Identify Top 15 results
together_highest <- head(together_highest, n = 15)
together_lowest <- head(together_lowest, n = 15)

#Output to CSV
write.csv(together_highest,'Highest Performance Factors.csv')
write.csv(together_lowest,'Lowest Performance Factors.csv')



