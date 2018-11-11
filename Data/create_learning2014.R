#Aliisan kakkosluennon hommat
#11.11.2018
rm(list=ls())
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
dim(lrn14)
str(lrn14)
# one factor variable all other are int.
# Dimension is 183 observations and 60 variable

## use dplyr
install.packages("dplyr")
library(dplyr)

# questions related to deep, surface and strategic learning
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

# select the columns related to deep learning and create column 'deep' by averaging
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

# select the columns related to surface learning and create column 'surf' by averaging
surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)

# select the columns related to strategic learning and create column 'stra' by averaging
strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

# kepp columsn gender, age, attitude, deep, stra, surf and points
keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")

#and create a new dataset
learning2014 <- select(lrn14, one_of(keep_columns))

#exclude observations with points==0
learning2014 <- filter(learning2014, Points != 0 )

#Set the working direcoty
setwd("/Users/aliisako/Documents/GitHub/IODS-project")

#save the new data
write.csv(learning2014, file="data/learning2014.csv", row.names = FALSE) 

#check that the data was saved correctly
learning2014<- read.csv("data/learning2014.csv")
str(learning2014)
head(learning2014)
