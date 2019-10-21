# CKME136 - Stan Tomas
#Load all datasets

setwd("C:/_Project/CKME136/Data")
Copy_1 <- read.csv("1_Copy.csv", header = TRUE,sep=",",strip.white=TRUE)
Copy_2 <- read.csv("2_Copy.csv", header = TRUE,sep=",",strip.white=TRUE)
Copy_3 <- read.csv("3_Copy.csv", header = TRUE,sep=",",strip.white=TRUE)
Copy_4 <- read.csv("4_Copy.csv", header = TRUE,sep=",",strip.white=TRUE)

#Remove unnecessary columns from each dataset

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


#Merge datasets

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