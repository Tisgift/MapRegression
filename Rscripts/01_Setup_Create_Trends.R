library("formattable")
library("lubridate")

# Choose index to analyze
Index<-read.table("../infiles/OMX30SClose.csv", skip = 1 , sep = ";", dec=",")
Index<-read.table("../infiles/OMXN402002Cut.csv", skip = 1 , sep = ";", dec=".")
colnames(Index)<-c("Date","Index")
# For N40 data only
Index[,"Index"]<-as.numeric(gsub(",", "", Index[,"Index"], fixed=TRUE))

# Read fake index.
Index<-read.table("FakeIndex.csv")

####Do Not use if loading FakeIndex.csv
#Reverse Order and rename to index
Index[,"Index"]<-rev(Index[,"Index"])
Index[,"Date"]<-rev(Index[,"Date"])
#Remove Zero Index Datapoints only if it has zero, for S30
Index<-Index[-which(Index[,"Index"]==0),]
####End Do Not use if loading FakeIndex.csv

StartDate<-Index[1,"Date"]
EndDate<-Index[length(Index[,"Date"]),"Date"]
NoDates<-dim(Index)[1]
NoYears<-year(EndDate)-year(StartDate)
StartValue<-Index[1,"Index"]

plot(as.Date(Index[,"Date"]),Index[,"Index"], type = "l",col="black",xlab="Date",ylab="Index", xlim=c(as.Date(StartDate),as.Date(EndDate)),xaxt='n')
axis(1, as.Date(StartDate)+years(0:NoYears), format(as.Date(StartDate)+years(0:NoYears),"%Y"),cex.axis=0.8)


#Create rate changes
Days<-1
Day1rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Day1rate<-c(rep(1,Days),Day1rate)
Days<-2
Day2rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Day2rate<-c(rep(1,Days),Day2rate)
Days<-3
Day3rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Day3rate<-c(rep(1,Days),Day3rate)
Days<-7
Day7rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Day7rate<-c(rep(1,Days),Day7rate)
Days<-14
Day14rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Day14rate<-c(rep(1,Days),Day14rate)
Days<-30
Day30rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Day30rate<-c(rep(1,Days),Day30rate)
Days<-90
Day90rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Day90rate<-c(rep(1,Days),Day90rate)
Days<-180
Day180rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Day180rate<-c(rep(1,Days),Day180rate)
Days<-365
Day365rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Day365rate<-c(rep(1,Days),Day365rate)
Days<-730
Day730rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Day730rate<-c(rep(1,Days),Day730rate)

plot(as.Date(Index[,"Date"]),Index[,"Index"], type = "p",pch=21, cex=0.1, col=c("black","red")[as.numeric(Day30rate<1)+1],xlab="Date",ylab="Index", xlim=c(as.Date(StartDate),as.Date(EndDate)),xaxt='n')

#Average up or down index movements in specified time periods
Days<-7
MeanUp7<-vector(mode="numeric", length=length(Index[,"Date"]))
MeanDown7<-vector(mode="numeric", length=length(Index[,"Date"]))
MeanUp7[1:Days]<-1
MeanDown7[1:Days]<-1
for(Day in (Days+1):length(Index[,"Date"]))
{
  if(any((Day1rate[(Day-(1:Days))])>1))
  {
    MeanUp7[Day]<-mean(Day1rate[(Day-(1:Days))][which(Day1rate[(Day-(1:Days))]>1)])
  } else
  {
    MeanUp7[Day]<-1
  }
  if(any((Day1rate[(Day-(1:Days))])<1))
  {
    MeanDown7[Day]<-mean(Day1rate[(Day-(1:Days))][which(Day1rate[(Day-(1:Days))]<1)])
  } else
  {
    MeanDown7[Day]<-1
  }
}
lines(as.Date(Index[,"Date"]),MeanUp7*StartValue,col="green")
lines(as.Date(Index[,"Date"]),MeanDown7*StartValue,col="red")


Days<-14
MeanUp14<-vector(mode="numeric", length=length(Index[,"Date"]))
MeanDown14<-vector(mode="numeric", length=length(Index[,"Date"]))
MeanUp14[1:Days]<-1
MeanDown14[1:Days]<-1
for(Day in (Days+1):length(Index[,"Date"]))
{
  if(any((Day1rate[(Day-(1:Days))])>1))
  {
    MeanUp14[Day]<-mean(Day1rate[(Day-(1:Days))][which(Day1rate[(Day-(1:Days))]>1)])
  } else
  {
    MeanUp14[Day]<-1
  }
  if(any((Day1rate[(Day-(1:Days))])<1))
  {
    MeanDown14[Day]<-mean(Day1rate[(Day-(1:Days))][which(Day1rate[(Day-(1:Days))]<1)])
  } else
  {
    MeanDown14[Day]<-1
  }
}
lines(as.Date(Index[,"Date"]),MeanUp14*StartValue,col="green")
lines(as.Date(Index[,"Date"]),MeanDown14*StartValue,col="red")


Days<-30
MeanUp30<-vector(mode="numeric", length=length(Index[,"Date"]))
MeanDown30<-vector(mode="numeric", length=length(Index[,"Date"]))
MeanUp30[1:Days]<-1
MeanDown30[1:Days]<-1
for(Day in (Days+1):length(Index[,"Date"]))
{
  if(any((Day1rate[(Day-(1:Days))])>1))
  {
    MeanUp30[Day]<-mean(Day1rate[(Day-(1:Days))][which(Day1rate[(Day-(1:Days))]>1)])
  } else
  {
    MeanUp30[Day]<-1
  }
  if(any((Day1rate[(Day-(1:Days))])<1))
  {
    MeanDown30[Day]<-mean(Day1rate[(Day-(1:Days))][which(Day1rate[(Day-(1:Days))]<1)])
  } else
  {
    MeanDown30[Day]<-1
  }
}
lines(as.Date(Index[,"Date"]),MeanUp30*StartValue,col="green")
lines(as.Date(Index[,"Date"]),MeanDown30*StartValue,col="red")


#Generate lowest highest numeric in specified time ranges
Days<-7
Lowest7<-vector(mode="numeric", length=length(Index[,"Date"]))
Lowest7[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Lowest7[Day]<-as.numeric(min(Index[(Day-(1:Days)),"Index"])>Index[(Day),"Index"])
}
Days<-14
Lowest14<-vector(mode="numeric", length=length(Index[,"Date"]))
Lowest14[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Lowest14[Day]<-as.numeric(min(Index[(Day-(1:Days)),"Index"])>Index[(Day),"Index"])
}
Days<-30
Lowest30<-vector(mode="numeric", length=length(Index[,"Date"]))
Lowest30[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Lowest30[Day]<-as.numeric(min(Index[(Day-(1:Days)),"Index"])>Index[(Day),"Index"])
}
Days<-90
Lowest90<-vector(mode="numeric", length=length(Index[,"Date"]))
Lowest90[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Lowest90[Day]<-as.numeric(min(Index[(Day-(1:Days)),"Index"])>Index[(Day),"Index"])
}
Days<-180
Lowest180<-vector(mode="numeric", length=length(Index[,"Date"]))
Lowest180[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Lowest180[Day]<-as.numeric(min(Index[(Day-(1:Days)),"Index"])>Index[(Day),"Index"])
}
Days<-365
Lowest365<-vector(mode="numeric", length=length(Index[,"Date"]))
Lowest365[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Lowest365[Day]<-as.numeric(min(Index[(Day-(1:Days)),"Index"])>Index[(Day),"Index"])
}
Days<-730
Lowest730<-vector(mode="numeric", length=length(Index[,"Date"]))
Lowest730[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Lowest730[Day]<-as.numeric(min(Index[(Day-(1:Days)),"Index"])>Index[(Day),"Index"])
}

Days<-7
Highest7<-vector(mode="numeric", length=length(Index[,"Date"]))
Highest7[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Highest7[Day]<-as.numeric(max(Index[(Day-(1:Days)),"Index"])<Index[(Day),"Index"])
}
Days<-14
Highest14<-vector(mode="numeric", length=length(Index[,"Date"]))
Highest14[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Highest14[Day]<-as.numeric(max(Index[(Day-(1:Days)),"Index"])<Index[(Day),"Index"])
}
Days<-30
Highest30<-vector(mode="numeric", length=length(Index[,"Date"]))
Highest30[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Highest30[Day]<-as.numeric(max(Index[(Day-(1:Days)),"Index"])<Index[(Day),"Index"])
}
Days<-90
Highest90<-vector(mode="numeric", length=length(Index[,"Date"]))
Highest90[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Highest90[Day]<-as.numeric(max(Index[(Day-(1:Days)),"Index"])<Index[(Day),"Index"])
}
Days<-180
Highest180<-vector(mode="numeric", length=length(Index[,"Date"]))
Highest180[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Highest180[Day]<-as.numeric(max(Index[(Day-(1:Days)),"Index"])<Index[(Day),"Index"])
}
Days<-365
Highest365<-vector(mode="numeric", length=length(Index[,"Date"]))
Highest365[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Highest365[Day]<-as.numeric(max(Index[(Day-(1:Days)),"Index"])<Index[(Day),"Index"])
}
Days<-730
Highest730<-vector(mode="numeric", length=length(Index[,"Date"]))
Highest730[1:Days]<-FALSE 
for(Day in (Days+1):length(Index[,"Date"]))
{
  Highest730[Day]<-as.numeric(max(Index[(Day-(1:Days)),"Index"])<Index[(Day),"Index"])
}

plot(as.Date(Index[,"Date"]),Index[,"Index"], type = "p",pch=21, cex=0.1, col=c("black","red")[Highest14+1],xlab="Date",ylab="Index", xlim=c(as.Date(StartDate),as.Date(EndDate)),xaxt='n')

#All time high. Current low since last all time high 
AllTimeHigh<-vector(mode = "numeric", length = length(Index[,"Date"]))
DayAllTimeHigh<-vector(mode = "numeric", length = length(Index[,"Date"]))
CurrTimeLow<-vector(mode = "numeric", length = length(Index[,"Date"]))
DayCurrTimeLow<-vector(mode = "numeric", length = length(Index[,"Date"]))
RateAllTimeHigh<-vector(mode = "numeric", length = length(Index[,"Date"]))
RateCurrTimeLow<-vector(mode = "numeric", length = length(Index[,"Date"]))
AllTimeHigh[1]<-Index[1,"Index"]
DayAllTimeHigh[1]<-1
CurrTimeLow[1]<-Index[1,"Index"]
DayCurrTimeLow[1]<-1
RateAllTimeHigh[1]<-1
RateCurrTimeLow[1]<-1
for(Day in 2:length(Index[,"Date"]))
{
if(Index[Day,"Index"]>AllTimeHigh[Day-1])
{
  AllTimeHigh[Day]<-Index[Day,"Index"]
  DayAllTimeHigh[Day]<-Day
  CurrTimeLow[Day]<-Index[Day,"Index"]
  DayCurrTimeLow[Day]<-Day
} else 
{
  AllTimeHigh[Day]<-AllTimeHigh[Day-1]
  DayAllTimeHigh[Day]<-DayAllTimeHigh[Day-1]
  
  if(Index[Day,"Index"]<CurrTimeLow[Day-1])  
  {
    CurrTimeLow[Day]<-Index[Day,"Index"]
    DayCurrTimeLow[Day]<-Day
  } else
  {
    CurrTimeLow[Day]<-CurrTimeLow[Day-1]
    DayCurrTimeLow[Day]<-DayCurrTimeLow[Day-1]
  }
  
}
RateAllTimeHigh[Day]<-Index[Day,"Index"]/AllTimeHigh[Day]
RateCurrTimeLow[Day]<-Index[Day,"Index"]/CurrTimeLow[Day]
}

lines(as.Date(Index[,"Date"]),AllTimeHigh)
lines(as.Date(Index[,"Date"]),CurrTimeLow,col="red")
lines(as.Date(Index[,"Date"]),RateAllTimeHigh*StartValue)
lines(as.Date(Index[,"Date"]),RateCurrTimeLow*StartValue,col="green")

###Future rates Skip per day normalization
Days<-7
Future7rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Future7rate<-c(Future7rate,rep(1,Days))
lines(as.Date(Index[,"Date"]),Future7rate*StartValue)

Days<-14
Future14rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Future14rate<-c(Future14rate,rep(1,Days))
lines(as.Date(Index[,"Date"]),Future14rate*StartValue)

Days<-30
Future30rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Future30rate<-c(Future30rate,rep(1,Days))
lines(as.Date(Index[,"Date"]),Future30rate*StartValue)

Days<-90
Future90rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Future90rate<-c(Future90rate,rep(1,Days))
lines(as.Date(Index[,"Date"]),Future90rate*StartValue,col="blue")

Days<-180
Future180rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Future180rate<-c(Future180rate,rep(1,Days))
lines(as.Date(Index[,"Date"]),Future180rate*StartValue,col="green")

Days<-365
Future365rate<-Index[(Days+1):length(Index[,"Index"]),"Index"]/(Index[1:(length(Index[,"Index"])-Days),"Index"])
Future365rate<-c(Future365rate,rep(1,Days))
lines(as.Date(Index[,"Date"]),Future365rate*StartValue,col="green")

#SummaryTable<-cbind(Index,Future7rate,Future14rate,Future30rate,Future90rate,Future180rate,Future365rate,Day1rate,Day2rate,Day3rate,Day7rate,Day14rate,Day30rate,Day90rate,Day180rate,Day365rate,Day730rate,MeanUp7,MeanDown7,MeanUp14,MeanDown14,MeanUp30,MeanDown30,RateAllTimeHigh,RateCurrTimeLow,Highest7,Lowest7,Highest14,Lowest14,Highest30,Lowest30,Highest90,Lowest90,Highest180,Lowest180,Highest365,Lowest365,Highest730,Lowest730)
# Use log2
SummaryTable<-cbind(Index,log2(Future7rate),log2(Future14rate),log2(Future30rate),log2(Future90rate),log2(Future180rate),log2(Future365rate),log2(Day1rate),log2(Day2rate),log2(Day3rate),log2(Day7rate),log2(Day14rate),log2(Day30rate),log2(Day90rate),log2(Day180rate),log2(Day365rate),log2(Day730rate),log2(MeanUp7),log2(MeanDown7),log2(MeanUp14),log2(MeanDown14),log2(MeanUp30),log2(MeanDown30),log2(RateAllTimeHigh),log2(RateCurrTimeLow),Highest7,Lowest7,Highest14,Lowest14,Highest30,Lowest30,Highest90,Lowest90,Highest180,Lowest180,Highest365,Lowest365,Highest730,Lowest730)


#Trim dataset due to no data in predictions and historical
SummaryTable<-SummaryTable[-(1:730),]#Remove first 730 rows
SummaryTable<-SummaryTable[-((dim(SummaryTable)[1]-365):dim(SummaryTable)[1]),]#Remove last 365 rows
# Choose filename to write
write.table(SummaryTable,file="../outfiles/SummaryTable_OMXN40.csv")
write.table(SummaryTable,file="../outfiles/SummaryTablelog2_FakeIndex.csv")


StartDate<-SummaryTable[1,"Date"]
EndDate<-SummaryTable[length(SummaryTable[,"Date"]),"Date"]
NoYears<-year(EndDate)-year(StartDate)
plot(as.Date(SummaryTable[,"Date"]),SummaryTable[,"Index"], type = "l",col="black",xlab="Date",ylab="Index", xlim=c(as.Date(StartDate),as.Date(EndDate)),xaxt='n')
axis(1, as.Date(StartDate)+years(0:NoYears), format(as.Date(StartDate)+years(0:NoYears),"%Y"),cex.axis=0.8)

