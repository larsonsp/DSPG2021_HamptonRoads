library(readr)
library(hash)
library(readxl)
setwd("~/GitPractice/DSPG2021_HamptonRoads")
allEducationData <- read_excel("AllEducationData.xlsx")
blackEducationData <- read_excel("BlackEducationData.xlsx")

allEducationData

barplot(counts, main="General Di",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

