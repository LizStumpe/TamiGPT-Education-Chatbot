library(readxl)
library(tidyverse)
library(ggplot2)

Leap2019 <- read_excel("Entity/Final Project/State Leap/2019 LEAP.xlsx")
head(Leap2019)
tail(Leap2019)
View(Leap2019)

### Cleaning Data
glimpse(Leap2019)
class(Leap2019$`School Code`)

#Renaming Columns
names(Leap2019)[names(Leap2019) == "School System Name"] <- "School_System"
names(Leap2019)[names(Leap2019) == "School Code"] <- "School_code"
names(Leap2019)[names(Leap2019) == "School Name"] <- "School_name"

#Removing unnecessary rows: 
Leap2019_2 <- slice(Leap2019, 1:(n() - 1217))
Leap2019_2[-1,]
view(Leap2019_2)

#Removing unnecessary columns: 
Leap2019_3 <- subset(Leap2019_2, select = -c(School_code, School_name))
view(Leap2019_3)

#Recoding NRs
Leap2019_3$Eng.I.Adv[Leap2019_3$Eng.I.Adv=='NR'] <- 0
view(Leap2019_3)
Leap2019_3$Eng.I.Mas[Leap2019_3$Eng.I.Mas=='NR'] <- 0
Leap2019_3$Eng.I.Basic[Leap2019_3$Eng.I.Basic=='NR'] <- 0
Leap2019_3$Eng.I.App.Basic[Leap2019_3$Eng.I.App.Basic=='NR'] <- 0
Leap2019_3$Eng.I.Uns[Leap2019_3$Eng.I.Uns=='NR'] <- 0
Leap2019_3$Eng.II.Adv[Leap2019_3$Eng.II.Adv=='NR'] <- 0
Leap2019_3$Eng.II.Mas[Leap2019_3$Eng.II.Mas=='NR'] <- 0
Leap2019_3$Eng.II.Basic[Leap2019_3$Eng.II.Basic=='NR'] <- 0
Leap2019_3$Eng.II.App.Basic[Leap2019_3$Eng.II.App.Basic=='NR'] <- 0
Leap2019_3$Eng.II.Uns[Leap2019_3$Eng.II.Uns=='NR'] <- 0
Leap2019_3$Alg.I.Adv[Leap2019_3$Alg.I.Adv=='NR'] <- 0
Leap2019_3$Alg.I.Mas[Leap2019_3$Alg.I.Mas=='NR'] <- 0
Leap2019_3$Alg.I.Basic[Leap2019_3$Alg.I.Basic=='NR'] <- 0
Leap2019_3$Alg.I.App.Basic[Leap2019_3$Alg.I.App.Basic=='NR'] <- 0
Leap2019_3$Alg.I.Uns[Leap2019_3$Alg.I.Uns=='NR'] <- 0
Leap2019_3$Geo.Adv[Leap2019_3$Geo.Adv=='NR'] <- 0
Leap2019_3$Geo.Mas[Leap2019_3$Geo.Mas=='NR'] <- 0
Leap2019_3$Geo.Basic[Leap2019_3$Geo.Basic=='NR'] <- 0
Leap2019_3$Geo.App.Basic[Leap2019_3$Geo.App.Basic=='NR'] <- 0
Leap2019_3$Geo.Uns[Leap2019_3$Geo.Uns=='NR'] <- 0
Leap2019_3$Hist.Adv[Leap2019_3$Hist.Adv=='NR'] <- 0
Leap2019_3$Hist.Mas[Leap2019_3$Hist.Mas=='NR'] <- 0
Leap2019_3$Hist.Basic[Leap2019_3$Hist.Basic=='NR'] <- 0
Leap2019_3$Hist.App.Basic[Leap2019_3$Hist.App.Basic=='NR'] <- 0
Leap2019_3$Hist.Uns[Leap2019_3$Hist.Uns=='NR'] <- 0
Leap2019_3$Bio.Mas[Leap2019_3$Bio.Mas=='NR'] <- 0
Leap2019_3$Bio.Basic[Leap2019_3$Bio.Basic=='NR'] <- 0
Leap2019_3$Bio.App.Basic[Leap2019_3$Bio.App.Basic=='NR'] <- 0
Leap2019_3$Bio.Uns[Leap2019_3$Bio.Uns=='NR'] <- 0
Leap2019_3$Eng.III.Mas[Leap2019_3$Eng.III.Mas=='NR'] <- 0
Leap2019_3$Eng.III.Basic[Leap2019_3$Eng.III.Basic=='NR'] <- 0
Leap2019_3$Eng.III.App.Basic[Leap2019_3$Eng.III.App.Basic=='NR'] <- 0
Leap2019_3$Eng.III.Uns[Leap2019_3$Eng.III.Uns=='NR'] <- 0
view(Leap2019_3)


#Regrouping & re-coding data
#Need to remove all the NR cells, but not showing up as null or NA & can't be dropped as is...
#Count NR Values 
countNR_before <- sum(sapply(Leap2019_3, function(x) length(unique(x[grepl("NR", x)]))))
countNR_before

#Remove NR values
Leap2019_3noNR <- Leap2019_3[!apply(Leap2019_3 == "NR", 1, any),]
Leap2019_3noNR

#Confirm removal of NR values 
countNR_after <- sum(sapply(Leap2019_3noNR, function(x) length(unique(x[grepl("NR", x)]))))
countNR_after


Leap2019_3noNR_sub <- subset(Leap2019_3noNR, Subgroup == "Total Population" & 'Summary Level' == "School System")
view(Leap2019_3noNR_sub)

#English 1
Leap2019_EnglishI <- Leap2019_3[,c("School_System","Eng.I.Adv", "Eng.I.Mas", "Eng.I.Basic", "Eng.I.App.Basic", "Eng.I.Uns")]
countNR_before <- sum(sapply(Leap2019_3, function(x) length(unique(x[grepl("NR", x)]))))
unique(Leap2019_EnglishI$Eng.I.Adv)


#English 2
Leap2019_EnglishII <- Leap2019_3[,c("School_System", "Subgroup", "Eng.II.Adv", "Eng.II.Mas", "Eng.II.Basic", "Eng.II.App.Basic", "Eng.II.Uns")]

#English 3
Leap2019_EnglishIII <- Leap2019_3[,c("School_System", "Subgroup", "Eng.III.Mas", "Eng.III.Basic", "Eng.III.App.Basic", "Eng.III.Uns")]

#Algebra 1
Leap2019_Algebra <- Leap2019_3[,c("School_System", "Subgroup", "Alg.I.Adv", "Alg.I.Mas", "Alg.I.Basic", "Alg.I.App.Basic", "Alg.I.Uns")]

#Geometry
Leap2019_Geometry <- Leap2019_3[,c("School_System", "Subgroup", "Geo.Adv", "Geo.Mas", "Geo.Basic", "Geo.App.Basic", "Geo.Uns")]

#History
Leap2019_History <- Leap2019_3[,c("School_System", "Subgroup", "Hist.Adv", "Hist.Mas", "Hist.Basic", "Hist.App.Basic", "Hist.Uns")]

#Biology
Leap2019_Biology <- Leap2019_3[,c("School_System", "Subgroup", "Bio.Mas", "Bio.Basic", "Bio.App.Basic", "Bio.Uns")]



