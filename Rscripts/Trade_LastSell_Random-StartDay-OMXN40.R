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

#To test actually reversed order
#OMXN40[,"OMXN40"]<-rev(OMXN40[,"OMXN40"])

plot(as.Date(OMXN40[,"Date"]),OMXN40[,"OMXN40"], type = "l",col="black",xlab="Date",ylab="Index", ylim=c(0,3000), xlim=c(as.Date("2001-12-31"),as.Date("2021-02-01")),xaxt='n')
axis(1, as.Date(ymd(20010101)+years(0:20)), format(as.Date(ymd(20010101)+years(0:20)),"%Y"),cex.axis=0.8)

#Generate Trend so it does not have to run every time in main loop
Trend<-vector(mode="numeric", length=length(OMXN40[,"Date"]))
Trend[1:30]<-0 #Do not calculate trend for first 30 Days
for(Day in 31:length(OMXN40[,"Date"]))
{
  Trend[Day]<-mean((OMXN40[Day,"OMXN40"]-OMXN40[(Day-1:30),"OMXN40"])/1:30)
}
TrendColor<-rep("yellow",length(Trend))
TrendColor[Trend<(-1)]<-"red"
TrendColor[Trend>1]<-"green"
plot(as.Date(OMXN40[,"Date"]),OMXN40[,"OMXN40"], type = "p",pch=16,cex=0.2 ,col=TrendColor,xlab="Date",ylab="Index", ylim=c(0,3000), xlim=c(as.Date("2001-12-31"),as.Date("2021-02-01")),xaxt='n')
#Line below to zoom in plot
plot(as.Date(OMXN40[,"Date"])[1:500],OMXN40[,"OMXN40"][1:500], type = "p",pch=16,cex=1 ,col=TrendColor,xlab="Date",ylab="Index",xaxt='n')
axis(1, as.Date(ymd(20010101)+years(0:20)), format(as.Date(ymd(20010101)+years(0:20)),"%Y"),cex.axis=0.8)

#Generate 14 Days lowest so it does not have to run every time in main loop
# Lowest<-vector(mode="logical", length=length(OMXN40[,"Date"]))
# Lowest[1:14]<-FALSE #Do not calculate trend for first 14 Days
# for(Day in 15:length(OMXN40[,"Date"]))
# {
#   Lowest[Day]<-min(OMXN40[(Day-1:14),"OMXN40"])>OMXN40[(Day),"OMXN40"]
# }


###Run several cycles of trading from random startdays 2 to 600
TestCycles<-10
StartDays<-sample(2:600,TestCycles) #StartDay 1 is not allowed it breaks range recalculation in plotting via negative subscripting ie [-(1:1)]
StartDays[1]<-2# Test if you want to test entire range but day 1
MeanDiffVector<-vector(mode = "numeric", length = TestCycles)
#Cycle<-1 #For testing
for(Cycle in 1:TestCycles)
{
print(paste("Cycle",Cycle))

###Set parameters and Start Trading
Courtage<-0.0025 #Fee for transaction

Value<-OMXN40[StartDays[Cycle],"OMXN40"]*(1-Courtage)#First transaction Buy stocks first time to same as index makes curves start (exactly) the same
Stock<-1 #Keeps track if money are in bank 0 or stock 1
Transactions<-1 #Counts number of buy and sell
length(OMXN40[,"Date"])
Values<-vector(mode="numeric", length=(length(OMXN40[,"Date"])-StartDays[Cycle]+1))
Values[1]<-Value
BuyDay<-StartDays[Cycle]
SellDay<-StartDays[Cycle] #The startday is bot actually a Sellday
PercentBuy<-0.99
PercentSell<-1.01
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
}
Values[ValueDay]<-Value
#Set stop loss (lock in bank)
#Set force buy (Buy even if normal conditions buy has not been set)

#Sell
if(Stock==1&&OMXN40[Day,"OMXN40"]/OMXN40[SellDay,"OMXN40"]>PercentSell)
{
  Stock<-0
  Value<-Value*(1-Courtage)
  Values[ValueDay]<-Value
  SellDay<-Day
  Transactions<-Transactions+1
}
#Buy
if(Stock==0&&OMXN40[Day,"OMXN40"]/OMXN40[SellDay,"OMXN40"]<PercentBuy)
{
  Stock<-1
  Value<-Value*(1-Courtage)
  Values[ValueDay]<-Value
  BuyDay<-Day
  Transactions<-Transactions+1
}
SumDiff<-SumDiff+Values[ValueDay]-OMXN40[Day,"OMXN40"]
}
plot(as.Date(OMXN40[,"Date"]),OMXN40[,"OMXN40"], type = "l",col="black",xlab="Date",ylab="Index", ylim=c(0,3000), xlim=c(as.Date("2001-12-31"),as.Date("2021-02-01")),xaxt='n')
axis(1, as.Date(ymd(20010101)+years(0:20)), format(as.Date(ymd(20010101)+years(0:20)),"%Y"),cex.axis=0.8)
lines(as.Date(OMXN40[,"Date"])[-(1:(StartDays[Cycle]-1))],Values, type = "l",col="blue")
MeanDiff<-SumDiff/length(Values)
MeanDiffVector[Cycle]<-MeanDiff
title(main=paste("MeanDiff OMXN40",MeanDiff,"Buy",PercentBuy,"Sell",PercentSell,"Courtage",Courtage,"Transactions",Transactions,sep=" "))
}
mean(MeanDiffVector)
