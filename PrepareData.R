# drop all the unnecesary cows from all datasets
# these are 2128, 2172, 2045

tblbas=tblbas[!(tblbas$CowID=="2128" | tblbas$CowID=="2172" | tblbas$CowID=="2045"),]
tblblood=tblblood[!(tblblood$CowID=="2128" | tblblood$CowID=="2172" | tblblood$CowID=="2045"),]
tbllivpar=tbllivpar[!(tbllivpar$CowID=="2128" | tbllivpar$CowID=="2172" | tbllivpar$CowID=="2045"),]
tblmastdop=tblmastdop[!(tblmastdop$CowID=="2128" | tblmastdop$CowID=="2172" | tblmastdop$CowID=="2045"),]
tblmastpar=tblmastpar[!(tblmastpar$CowID=="2128" | tblmastpar$CowID=="2172" | tblmastpar$CowID=="2045"),]
tblmilk=tblmilk[!(tblmilk$CowID=="2128" | tblmilk$CowID=="2172" | tblmilk$CowID=="2045"),]
tblpyldop=tblpyldop[!(tblpyldop$CowID=="2128" | tblpyldop$CowID=="2172" | tblpyldop$CowID=="2045"),]
tblbody=tblbody[!(tblbody$CowID=="2128" | tblbody$CowID=="2172" | tblbody$CowID=="2045"),]


# declare all right types of data
#########
#tblbody#
#########
tblbody$Date=dmy(tblbody$Date)
tblbody$HeartGirth=as.numeric(tblbody$HeartGirth)


########
#tblliv#
########
tbllivpar=merge(tbllivpar,tblbas[,c("CowID","scm")],by='CowID',all.x = T)
tbllivpar$SCM=NULL
# diwxnx ta Mean 
tbllivpar=tbllivpar[!(tbllivpar$mean=="Mean"),]
# kanw ta character --> numeric
tbllivpar[-c(1:3)] <- unlist(lapply(tbllivpar[-c(1:3)], function(x) as.numeric(x)))


############
#tblmastdop#
############
tblmastdop[-c(1:3)] <- unlist(lapply(tblmastdop[-c(1:3)], function(x) gsub(",",".",x)))
tblmastdop[-c(1:3)] <- unlist(lapply(tblmastdop[-c(1:3)], function(x) as.numeric(x)))


############
#tblmastpar#
############

tblmastpar$Date=dmy(tblmastpar$Date)
tblmastpar=tblmastpar[!is.na(tblmastpar$Date),]

#########
#tblmilk#
#########

# tipota anixneysimo, opoia xreiastei tha mpoun ws as.factor

###########
#tblpyldop#
###########
tblpyldop$Date=dmy(tblpyldop$Date)
tblpyldop$EDV=as.numeric(tblpyldop$EDV)

