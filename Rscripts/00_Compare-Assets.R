library("formattable")
library("lubridate")

#####Observe order of dates!!!!

OMXN40<-read.table("../infiles/OMXN402002Cut.csv", skip = 1 , sep = ";")
colnames(OMXN40)<-c("Date","OMXN40")
OMXN40[,"OMXN40"]<-comma(OMXN40[,"OMXN40"])
plot(as.Date(OMXN40[,"Date"]),OMXN40[,"OMXN40"], type = "l",col="black",xlab="Date",ylab="Price or Index", ylim=c(100,5000))

NordicFoods<-read.table("../infiles/NordicFoodsEtc2000Cut.csv", skip = 1 , sep = ";")
colnames(NordicFoods)<-c("Date","FoodEtc")
NordicFoods[,"FoodEtc"]<-comma(NordicFoods[,"FoodEtc"])
lines(as.Date(NordicFoods[,"Date"]),NordicFoods[,"FoodEtc"], type = "l",col="blue")

BrentOil<-read.table("../infiles/BrentOil.csv", header=TRUE , sep = ",")
lines(as.Date(BrentOil[,"Date"], "%m/%d/%Y"),10*BrentOil[,"Close.Last"], type = "l",col="red")

Gold<-read.table("../infiles/Gold.csv", header=TRUE , sep = ",")
lines(as.Date(Gold[,"Date"], "%m/%d/%Y"),Gold[,"Close.Last"], type = "l",col="orange")

SweAgro<-read.table("../infiles/SweAgro.csv", header=TRUE , sep = ";")
lines(as.Date(SweAgro[,"Date"]),SweAgro[,"kSekHa"]/20, type = "l",col="brown")

Interest1p<-data.frame(ymd(20011231) + years(0:19),500*(1.01**(0:19)))
Interest3p<-data.frame(ymd(20011231) + years(0:19),500*(1.03**(0:19)))
Interest5p<-data.frame(ymd(20011231) + years(0:19),500*(1.05**(0:19)))
Interest10p<-data.frame(ymd(20011231) + years(0:19),500*(1.10**(0:19)))

lines(as.Date(Interest1p[,1]),Interest1p[,2], type = "l",col="green")
lines(as.Date(Interest3p[,1]),Interest3p[,2], type = "l",col="green")
lines(as.Date(Interest5p[,1]),Interest5p[,2], type = "l",col="green")
lines(as.Date(Interest10p[,1]),Interest10p[,2], type = "l",col="green")

MoneyM1<-read.table("../infiles/PenningM12004.csv", header=TRUE , sep = "\t")
lines(as.Date(ymd(20040101)+months(0:203)),MoneyM1[1,]/2000, type = "l",col="gray")

legend(x="topleft",legend=c("OMXN40 Index","NordicFoods Etc Index ","BrentOil*10","Gold","SweAgri/20","Interest 1,2,5,10","M1 Money"), text.col=c("black","blue","red","orange","brown","green","gray"), cex=0.8)

##Normalized

OMXN40Days2011<-match(ymd(20110101)+days(0:364),as.Date(OMXN40[,"Date"]))
OMXN40Average2011<-mean(OMXN40[OMXN40Days2011,"OMXN40"],na.rm=TRUE)
plot(as.Date(OMXN40[,"Date"]),OMXN40[,"OMXN40"]/OMXN40Average2011, type = "l",col="black",xlab="Date",ylab="Year 2011 Normalized", ylim=c(0,3), xlim=c(as.Date("2011-01-01"),as.Date("2021-02-01")))
axis(1, as.Date(ymd(20110101)+years(0:10)), format(as.Date(ymd(20110101)+years(0:10)),"%Y"))

NordicFoodsDays2011<-match(ymd(20110101)+days(0:364),as.Date(NordicFoods[,"Date"]))
NordicFoodsAverage2011<-mean(NordicFoods[NordicFoodsDays2011,"FoodEtc"],na.rm=TRUE)
lines(as.Date(NordicFoods[,"Date"]),NordicFoods[,"FoodEtc"]/NordicFoodsAverage2011, type = "l",col="blue")

BrentOilDays2011<-match(ymd(20110101)+days(0:364),as.Date(BrentOil[,"Date"],"%m/%d/%Y"))
BrentOilAverage2011<-mean(BrentOil[BrentOilDays2011,"Close.Last"],na.rm=TRUE)
lines(as.Date(BrentOil[,"Date"], "%m/%d/%Y"),BrentOil[,"Close.Last"]/BrentOilAverage2011, type = "l",col="red")

GoldDays2011<-match(ymd(20110101)+days(0:364),as.Date(Gold[,"Date"],"%m/%d/%Y"))
GoldAverage2011<-mean(Gold[GoldDays2011,"Close.Last"],na.rm=TRUE)
lines(as.Date(Gold[,"Date"], "%m/%d/%Y"),Gold[,"Close.Last"]/GoldAverage2011, type = "l",col="orange")

SweAgroAverage2011<-mean(SweAgro[c(3,4),2])#31/12 2010 2011
lines(as.Date(SweAgro[,"Date"]),SweAgro[,"kSekHa"]/SweAgroAverage2011, type = "l",col="brown")

Interest1p<-data.frame(ymd(20110101) + years(0:11),1*(1.01**(0:11)))
Interest3p<-data.frame(ymd(20110101) + years(0:11),1*(1.03**(0:11)))
Interest5p<-data.frame(ymd(20110101) + years(0:11),1*(1.05**(0:11)))
Interest10p<-data.frame(ymd(20110101) + years(0:11),1*(1.10**(0:11)))
lines(as.Date(Interest1p[,1]),Interest1p[,2], type = "l",col="green")
lines(as.Date(Interest3p[,1]),Interest3p[,2], type = "l",col="green")
lines(as.Date(Interest5p[,1]),Interest5p[,2], type = "l",col="green")
lines(as.Date(Interest10p[,1]),Interest10p[,2], type = "l",col="green")

legend(x="topleft",legend=c("OMXN40","NordicFoodsBeveragesTobacco ","BrentOil","Gold","SweAgriFields","Interest 1,2,5,10%","M1 Money"), text.col=c("black","blue","red","orange","brown","green","gray"), cex=0.8)
MoneyM1201101<-MoneyM1[1,which(colnames(MoneyM1)=="X2011M01")]
which(colnames(MoneyM1)=="X2011M01")
length(colnames(MoneyM1))
which(colnames(MoneyM1)=="X2011M01"):length(colnames(MoneyM1))
lines(as.Date(ymd(20040101)+months(which(colnames(MoneyM1)=="X2011M01"):length(colnames(MoneyM1)))),MoneyM1[1,which(colnames(MoneyM1)=="X2011M01"):length(colnames(MoneyM1))]/MoneyM1201101, type = "l",col="gray")
