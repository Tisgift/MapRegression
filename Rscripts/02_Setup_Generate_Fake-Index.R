#Use variables from real index

FakeIndex<-matrix(nrow=NoDates, ncol=2)
colnames(FakeIndex)<-c("Date","Index")
FakeIndex[,"Date"]<-Index[,"Date"]
Koef<-(Index[which(Index[,"Date"]==EndDate),"Index"]-Index[which(Index[,"Date"]==StartDate),"Index"])/NoDates
StartValue<-Index[which(Index[,"Date"]==StartDate),"Index"]
ZeroDate<-ceiling(StartValue/Koef)
FIndex<-StartValue+(0:(NoDates-1))*Koef
FIndex<-FIndex+0.4*(Koef*(ZeroDate:(NoDates+ZeroDate-1)))*sin(2*pi*(0:(NoDates-1))/730)
FIndex<-FIndex+0.1*(Koef*(ZeroDate:(NoDates+ZeroDate-1)))*sin(2*pi*(0:(NoDates-1))/180)
FIndex<-FIndex+0.04*(Koef*(ZeroDate:(NoDates+ZeroDate-1)))*sin(2*pi*(0:(NoDates-1))/30)
FIndex<-FIndex+0.01*(Koef*(ZeroDate:(NoDates+ZeroDate-1)))*sin(2*pi*(0:(NoDates-1))/7)
FIndex<-FIndex+(Koef*(ZeroDate:(NoDates+ZeroDate-1)))*0.005*runif(NoDates, 0, 1)
FakeIndex[,"Index"]<-FIndex
write.table(../infiles/FakeIndex,file="FakeIndex.csv")
plot(as.Date(FakeIndex[,"Date"]),FakeIndex[,"Index"], type="l",xaxt='n')
axis(1, as.Date(StartDate)+years(0:NoYears), format(as.Date(StartDate)+years(0:NoYears),"%Y"),cex.axis=0.8)
