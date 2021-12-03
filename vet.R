##################################
## master file calling R scripts##
##################################
# The folder of the project
W = "C:/vet"
WR = paste(W,"results",sep = "/")
WP = paste(W,"programs",sep = "/")
WG = paste(W,"graphs",sep = "/")
WD = paste(W,"data",sep = "/")
WRD = paste(W,"raw_data",sep = "/")

# Libraries
library(haven)
library(gdata)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)

# data loading
setwd(WRD)
#####
# 1 # pinakas me dedomena galaktos
#####
tblmilk=read.csv("tablemilk.csv",sep = '\t')
#####
# 2 # mastiko (Doppler)
#####
tblmastdop=read.csv("tblmastdop.csv",sep = '\t')
#####
# 3 #pylaia fleva (Doppler)
#####
tblpyldop=read.csv("tblpyldop.csv",sep = '\t')

#####
# 4 # ergastiriakes metriseis apo deigmata aimatos
#####
tblblood=read.csv("tblblood.csv",sep = '\t')

#####
# 5 # mastiko pregxyma ixogeneia
#####
tblmastpar=read.csv("tblmastpar.csv",sep = '\t')

#####
# 6 # ipatiko pregxyma ixogeneia
#####
tbllivpar=read.csv("tbllivpar.csv",sep = '\t')

#####
# 7 # somatometrika? (BCS, BW & Other Metrics)
#####
tblbody=read.csv("tblbody.csv",sep = '\t')

#####
# 8 # basikos pinakas 
#####
setwd(WRD)
tblbas=read.csv("tblbas.csv",sep = '\t')
#################################################################################

#################################################################################

# DIORTHOSIS
#####
# 1 #
#####

######################################
### diorthosi tou dataset tblmilk###
######################################

# kenes eggrafes kai me "-" tis vazo NA
tblmilk$Date[tblmilk$Date==""]=NA
tblmilk$LastMilkYield[tblmilk$LastMilkYield==""]=NA
tblmilk$Genre[tblmilk$Genre=="-"]=NA
tblmilk$Number[tblmilk$Number=="-"]=NA
tblmilk$Species[tblmilk$Species=="-"]=NA
tblmilk$SCM[tblmilk$SCM=="Ξ”Ξ•Ξ Ξ±ΟΞΌΞ?Ξ³ΞµΟ„Ξ±ΞΉ"]=NA
tblmilk$SCM[tblmilk$SCM==""]=NA


# diorthosi ton NA se CowID kai Date

tblmilk=tblmilk[order(tblmilk$measur),]
tblmilk=tblmilk  %>% mutate(CowID=na.locf(CowID, na.rm=FALSE))
tblmilk=tblmilk  %>% mutate(Date=na.locf(Date, na.rm=FALSE))
tblmilk=tblmilk  %>% mutate(LastMilkYield=na.locf(LastMilkYield, na.rm=FALSE))
#tblmilk$Date=gsub("-","/",tblmilk$Date)

tblmilk$Date=ymd(tblmilk$Date)
tblmilk$LastMilkYield=gsub(",",".",tblmilk$LastMilkYield)
tblmilk$LastMilkYield=as.numeric(tblmilk$LastMilkYield)
tblmilk$SCC=as.numeric(tblmilk$SCC)

# apothikeysh pinaka me metriseis galatos
#setwd(WD)
#write.csv(tblmilk,file="tblmilk",row.names = F)


#####
# 2 #
#####

#######################################
### diorthosi tou dataset tblmastdop###
#######################################

# kenes eggrafes tis vazo NA
tblmastdop$Date[tblmastdop$Date==""]=NA
tblmastdop$CowID[tblmastdop$CowID==""]=NA

# petao tis eggrafes ton quarter B kai D

#tblmastdop=tblmastdop[!(is.na(tblmastdop$CowID) & tblmastdop$Quarter=='B' | (is.na(tblmastdop$CowID) & tblmastdop$Quarter=='D' )),]
#tblmastdop=tblmastdop[!(tblmastdop$Quarter=='B'),]
#tblmastdop$CowID[tblmastdop$D1==5.9 & tblmastdop$D2==25.8 & tblmastdop$FVI==3.57 & tblmastdop$PI==0.60]=2136

# gemizo ta NA tou tblmastdop

#tblmastdop=tblmastdop  %>% mutate(measur=na.locf(measur, na.rm=FALSE))
#tblmastdop=tblmastdop  %>% mutate(CowID=na.locf(CowID, na.rm=FALSE))
#tblmastdop=tblmastdop  %>% mutate(Date=na.locf(Date, na.rm=FALSE))


#merge scm indicator


tblmastdop=merge(tblmastdop,tblbas[,c("CowID","scm")],by='CowID',all.x = T)
# dates


tblmastdop$Date=gsub("/","-",tblmastdop$Date)
tblmastdop$Date=dmy(tblmastdop$Date)

#vazw thn plhroforia gia SCM
#tbl1=tablemilk[!duplicated(tablemilk$CowID),]
#tbl=merge(tblmastdop,tbl1[,c("CowID","SCM")],by="CowID",all = T)

# apothikeysh pinaka me metriseis mastikou Doppler
#setwd(WD)
#write.csv(tblmastdop,file="tblmastdop.csv",row.names = F)

#####
# 3 #
#####

#######################################
### diorthosi tou dataset tblpyldop ###
#######################################
# gemizo ta NA tou tblpyldop

tblpyldop=tblpyldop  %>% mutate(measur=na.locf(measur, na.rm=FALSE))

# apothikeysh pinaka me metriseis mastikou Doppler
#setwd(WD)
#write.csv(tblpyldop,file="tblpyldop.csv",row.names = F)


#####
# 4 #
#####

#######################################
### diorthosi tou dataset tblblood  ###
#######################################
tblblood$Total_Protein=gsub(",",".",tblblood$Total_Protein)
tblblood$Albumin=gsub(",",".",tblblood$Albumin)
tblblood$Total_Bilirubin=gsub(",",".",tblblood$Total_Bilirubin)
tblblood$NEFA=gsub(",",".",tblblood$NEFA)

tblblood$Total_Protein=as.numeric(tblblood$Total_Protein)
tblblood$Albumin=as.numeric(tblblood$Albumin)
tblblood$Total_Bilirubin=as.numeric(tblblood$Total_Bilirubin)
tblblood$NEFA=as.numeric(tblblood$NEFA)
#####
# 5 #
#####

#######################################
### diorthosi tou dataset tblmastpar  ###
#######################################
# gemizo ta NA tou tblmastpar
tblmastpar$Date[tblmastpar$Date==""]=NA

tblmastpar=tblmastpar  %>% mutate(measur=na.locf(measur, na.rm=FALSE))
tblmastpar=tblmastpar  %>% mutate(CowID=na.locf(CowID, na.rm=FALSE))
tblmastpar=tblmastpar  %>% mutate(Date=na.locf(Date, na.rm=FALSE))
# apothikeysh pinaka me metriseis mastikou Doppler
tblmastpar$SCM=NULL
tblmastpar=merge(tblmastpar,tblbas[,c("CowID","scm")],by="CowID",all.x = T)
#setwd(WD)
#write.csv(tblmastpar,file="tblmastpar.csv",row.names = F)

#####
# 6 #
#####

#######################################
### diorthosi tou dataset tbllivpar  ###
#######################################
# gemizo ta NA tou tbllivpar
tbllivpar$Date[tbllivpar$Date==""]=NA

tbllivpar=tbllivpar  %>% mutate(measur=na.locf(measur, na.rm=FALSE))
tbllivpar=tbllivpar  %>% mutate(CowID=na.locf(CowID, na.rm=FALSE))
tbllivpar=tbllivpar  %>% mutate(Date=na.locf(Date, na.rm=FALSE))

# allagi komma se .
tbllivpar$mean=gsub(",",".",tbllivpar$mean)
tbllivpar$Mean_Value=gsub(",",".",tbllivpar$Mean_Value)
tbllivpar$St_Deviation=gsub(",",".",tbllivpar$St_Deviation)
tbllivpar$Skewness=gsub(",",".",tbllivpar$Skewness)
tbllivpar$Excess=gsub(",",".",tbllivpar$Excess)
tbllivpar$X005_Quantil=gsub(",",".",tbllivpar$X005_Quantil)
tbllivpar$X095_Quantil=gsub(",",".",tbllivpar$X095_Quantil)
tbllivpar$Gradient_Mean_value=gsub(",",".",tbllivpar$Gradient_Mean_value)
tbllivpar$Gradient_Variance=gsub(",",".",tbllivpar$Gradient_Variance)
tbllivpar$Percentage_non.zero_Gradients=gsub(",",".",tbllivpar$Percentage_non.zero_Gradients)
tbllivpar$Contrast=gsub(",",".",tbllivpar$Contrast)
tbllivpar$Correlation=gsub(",",".",tbllivpar$Correlation)
tbllivpar$Entropy=gsub(",",".",tbllivpar$Entropy)
tbllivpar$Homogenity=gsub(",",".",tbllivpar$Homogenity)
tbllivpar$Run_Percentage=gsub(",",".",tbllivpar$Run_Percentage)
tbllivpar$Long.run_Emphasis=gsub(",",".",tbllivpar$Long.run_Emphasis)
tbllivpar$Gray_Value_Distribution=gsub(",",".",tbllivpar$Gray_Value_Distribution)
tbllivpar$Runlenght_Distribution=gsub(",",".",tbllivpar$Runlenght_Distribution)

tbllivpar=tbllivpar[!(tbllivpar$mean==""),]


tbllivpar$Date=dmy(tbllivpar$Date)
tbllivpar=tbllivpar[!is.na(tbllivpar$Date),]
# apothikeysh pinaka me metriseis mastikou Doppler
#setwd(WD)
#write.csv(tbllivpar,file="tbllivpar.csv",row.names = F)


#####
# 7 #
#####



#######################################
### diorthosi tou dataset tblbody  ###
#######################################
# gemizo ta NA tou tblbody
tblbody$Date[tblbody$Date==""]=NA

tblbody=tblbody  %>% mutate(measur=na.locf(measur, na.rm=FALSE))
tblbody=tblbody  %>% mutate(CowID=na.locf(CowID, na.rm=FALSE))
tblbody=tblbody  %>% mutate(Date=na.locf(Date, na.rm=FALSE))
# apothikeysh pinaka me metriseis mastikou Doppler
setwd(WD)
write.csv(tblbody,file="tblbody.csv",row.names = F)

#####
# 8 # Βασικός πίνακας 
#####
tblbas$Exam_D=dmy(tblbas$Exam_D)
tblbas$Birth_D=dmy(tblbas$Birth_D)

#tapply(tblmastdop$CowID,tblmastdop$measur,length)

#auto einai to prvto arxeio pou tha anoiksei