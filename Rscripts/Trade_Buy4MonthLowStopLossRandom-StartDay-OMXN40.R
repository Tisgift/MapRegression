library("formattable")
library("lubridate")

#####Observe order of dates, often start with newest in files. Can flip x-axis is plot!

OMXN40<-read.table("OMXN402002Cut.csv", skip = 1 , sep = ";")
colnames(OMXN40)<-c("Date","OMXN40")
#Reverse Order 
OMXN40[,"OMXN40"]<-rev(comma(OMXN40[,"OMXN40"]))
OMXN40[,"Date"]<-rev(OMXN40[,"Date"])

#Create daily rate change
OMXN40Rates<-OMXN40[2:length(OMXN40[,"OMXN40"]),"OMXN40"]/OMXN40[1:length(OMXN40[,"OMXN40"])-1,"OMXN40"]
OMXN40Rates<-c(1,OMXN40Rates)

plot(as.Date(OMXN40[,"Date"]),OMXN40[,"OMXN40"], type = "l",col="black",xlab="Date",ylab="Index", ylim=c(0,3000), xlim=c(as.Date("2001-12-31"),as.Date("2021-02-01")),xaxt='n')
axis(1, as.Date(ymd(20010101)+years(0:20)), format(as.Date(ymd(20010101)+years(0:20)),"%Y"),cex.axis=0.8)

#Generate Trend so it does not have to run every time in main loop
Trend<-vector(mode="numeric", length=length(OMXN40[,"Date"]))
Trend[1:30]<-0 #Do not calculate trend for first 30 Days
for(Day in 31:length(OMXN40[,"Date"]))
{
  Trend[Day]<-mean((OMXN40[Day,"OMXN40"]-OMXN40[(Day-1:30),"OMXN40"])/1:30)
}
TrendColor<-rep("grey",length(Trend))
TrendColor[Trend<(-1)]<-"red"
TrendColor[Trend>1]<-"green"
plot(as.Date(OMXN40[,"Date"]),OMXN40[,"OMXN40"], type = "p",pch=16,cex=0.2 ,col=TrendColor,xlab="Date",ylab="Index", ylim=c(0,3000), xlim=c(as.Date("2001-12-31"),as.Date("2021-02-01")),xaxt='n')
for(Day in 1:length(OMXN40[,"Date"]))
{
  lines(as.Date(OMXN40[Day:(Day+1),"Date"]),OMXN40[Day:(Day+1),"OMXN40"], type="l",col=TrendColor[Day])
}
#Trend colored Line below to zoom in plot
plot(as.Date(OMXN40[,"Date"])[1:2000],OMXN40[,"OMXN40"][1:2000], type = "p",pch=16,cex=0.2 ,col=TrendColor,xlab="Date",ylab="Index",xaxt='n')
for(Day in 1:2000)
{
  lines(as.Date(OMXN40[Day:(Day+1),"Date"]),OMXN40[Day:(Day+1),"OMXN40"], type="l",col=TrendColor[Day])
}
axis(1, as.Date(ymd(20010101)+years(0:20)), format(as.Date(ymd(20010101)+years(0:20)),"%Y"),cex.axis=0.8)

#Generate 4 month lowest so it does not have to run every time in main loop
Lowest<-vector(mode="logical", length=length(OMXN40[,"Date"]))
Lowest[1:120]<-FALSE #Do not calculate trend for first 14 Days
for(Day in 121:length(OMXN40[,"Date"]))
{
  Lowest[Day]<-min(OMXN40[(Day-(1:120)),"OMXN40"])>OMXN40[(Day),"OMXN40"]
}
LowestColor<-rep("grey",length(Lowest))
LowestColor[Lowest==TRUE]<-"green"
points(as.Date(OMXN40[,"Date"])[Lowest],OMXN40[,"OMXN40"][Lowest], type = "p",pch=16,cex=0.5 ,col=LowestColor[Lowest])

###Run several cycles of trading from random startdays 2 to 600
TestCycles<-10
StartDays<-sample(121:600,TestCycles) #StartDay 1 (and also ->30) is not allowed it breaks range recalculation in plotting via negative subscripting ie [-(1:1)]
#StartDays[1]<-2# Test if you want to test entire range but day 1
MeanDiffVector<-vector(mode = "numeric", length = TestCycles)
#Cycle<-1 #For testing
for(Cycle in 1:TestCycles)
{
print(paste("Cycle",Cycle))

###Set parameters and Start Trading
Courtage<-0 #Fee for transaction

Value<-OMXN40[StartDays[Cycle],"OMXN40"]*(1-Courtage)#First transaction Buy stocks first time to same as index makes curves start (exactly) the same
CurrentHigh<-Value
Stock<-1 #Keeps track if money are in bank 0 or stock 1
Transactions<-1 #Counts number of buy and sell
length(OMXN40[,"Date"])
Values<-vector(mode="numeric", length=(length(OMXN40[,"Date"])-StartDays[Cycle]+1))
Values[1]<-Value
BuyDay<-StartDays[Cycle]
SellDay<-StartDays[Cycle] #The startday is bot actually a Sellday
PercentBuy<-0.98
PercentSell<-1.035
SumDiff<-0


#Main Trading Loop
ValueDay<-1
#Day<-StartDays[Cycle]+1 #For Testing
for(Day in (StartDays[Cycle]+1):length(OMXN40[,"Date"]))
{
  ValueDay<-ValueDay+1
#Estimate trends change buy sell values
# if(Trend[Day]<2&&Trend[Day]>(-2))
# {
# PercentBuy<-0.99
# PercentSell<-1.01
# }
# if(Trend[Day]<(-2))
# {
# PercentBuy<-0.98
# PercentSell<-1.01
# }
# if(Trend[Day]>2)
# {
# PercentBuy<-0.99
# PercentSell<-1.02
# }
#Change Value if Stock keep value if Bank
if(Stock==1)
{
  #Value<-Values[BuyDay]*OMXN40[Day,"OMXN40"]/OMXN40[BuyDay,"OMXN40"]
  Value<-Value*OMXN40Rates[Day]
  if(Value>CurrentHigh) CurrentHigh<-Value
}
Values[ValueDay]<-Value
#Set stop loss (lock in bank)
#Set force buy (Buy even if normal conditions buy has not been set)

#Sell
if(Stock==1&&((Value/CurrentHigh)<0.8))
{
  Stock<-0
  Value<-Value*(1-Courtage)
  Values[ValueDay]<-Value
  SellDay<-Day
  Transactions<-Transactions+1
  print(paste("Transaction",Transactions,"Loss",1-Value/CurrentHigh,sep = " "))
}

#Buy
if(Stock==0&&mean(Trend[(Day-120):Day])>1)
{
  Stock<-1
  Value<-Value*(1-Courtage)
  Values[ValueDay]<-Value
  CurrentHigh<-Value
  BuyDay<-Day
  Transactions<-Transactions+1
}
SumDiff<-SumDiff+Values[ValueDay]-OMXN40[Day,"OMXN40"]
}
plot(as.Date(OMXN40[,"Date"]),OMXN40[,"OMXN40"], type = "l",col="black",xlab="Date",ylab="Index", ylim=c(0,3000), xlim=c(as.Date("2001-12-31"),as.Date("2021-02-01")),xaxt='n')
axis(1, as.Date(ymd(20010101)+years(0:20)), format(as.Date(ymd(20010101)+years(0:20)),"%Y"),cex.axis=0.8)
points(as.Date(OMXN40[,"Date"])[Lowest],OMXN40[,"OMXN40"][Lowest], type = "p",pch=16,cex=0.5 ,col=LowestColor[Lowest])
lines(as.Date(OMXN40[,"Date"])[-(1:(StartDays[Cycle]-1))],Values, type = "l",col="blue")
MeanDiff<-SumDiff/length(Values)
MeanDiffVector[Cycle]<-MeanDiff
title(main=paste("MeanDiff OMXN40",MeanDiff,"Buy",PercentBuy,"Sell",PercentSell,"Courtage",Courtage,"Transactions",Transactions,sep=" "))
}
mean(MeanDiffVector)

