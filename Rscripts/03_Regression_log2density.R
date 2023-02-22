library('plot.matrix')
library('ggplot2')
# Choose data for prediction and evaluation
SummaryTable<-read.table("../outfiles/SummaryTable_FakeIndex.csv")
SummaryTable<-read.table("../outfiles/SummaryTablelog2_FakeIndex.csv")
SummaryTable<-read.table("../outfiles/SummaryTable_OMXS30.csv")
SummaryTable<-read.table("../outfiles/SummaryTablelog2_OMXS30.csv")
SummaryTable<-read.table("../outfiles/SummaryTable_OMXN40.csv")
SummaryTable<-read.table("../outfiles/SummaryTablelog2_OMXN40.csv")

StartDate<-SummaryTable[1,"Date"]
EndDate<-SummaryTable[length(SummaryTable[,"Date"]),"Date"]
NoYears<-year(EndDate)-year(StartDate)
plot(as.Date(SummaryTable[,"Date"]),SummaryTable[,"Index"], type = "l",col="black",xlab="Date",ylab="Index", xlim=c(as.Date(StartDate),as.Date(EndDate)),xaxt='n')
axis(1, as.Date(StartDate)+years(0:NoYears), format(as.Date(StartDate)+years(0:NoYears),"%Y"),cex.axis=0.8)

# sample randomly
EvaluateRows<-sample(1:nrow(SummaryTable),round(nrow(SummaryTable)*0.2))
#PractiseTable<-SummaryTable#Use all data for practice
#EvaluateTable<-SummaryTable#Use all data for evaluation
PractiseTable<-SummaryTable[-EvaluateRows,]
EvaluateTable<-SummaryTable[EvaluateRows,]

#Sample historically
PractiseRows<-1:round(nrow(SummaryTable)*0.50) # 0.5
EvaluationRows<-(max(PractiseRows)+10):(max(PractiseRows)+210) # 10 210 works with non log
PractiseTable<-SummaryTable[PractiseRows,]
EvaluateTable<-SummaryTable[-PractiseRows,]
EvaluateTable<-SummaryTable[EvaluationRows,]

#Test specific outcome
#Outcome<-"log2.Future7rate."

for(Outcome in colnames(SummaryTable)[3:8])
{
SummaryTable<-PractiseTable
OutSplit<-20
OutWidth<-(max(SummaryTable[,Outcome])-min(SummaryTable[,Outcome]))/OutSplit
OutMin<-min(SummaryTable[,Outcome])
OutMax<-max(SummaryTable[,Outcome])
OutCount<-vector(mode="numeric",length=OutSplit)
for (Index in ceiling((SummaryTable[,Outcome]-OutMin)/OutWidth))
{
  if (Index==0)Index<-1 #Prevent lowest Outcome out of bounds
  OutCount[Index]<-OutCount[Index]+1
}
OutCount[OutCount==0]<-0.1# Give 0.1 count to zero counts, to prevent INF when normalizing later
OutDensity<-OutCount/sum(OutCount)

####Create density matrices per condition
PredMinList<-list()
PredMaxList<-list()
PredWidthList<-list()
NormDensityList<-list()
for(Predictor in colnames(SummaryTable)[9:40])
{
print(Predictor)
PredSplit<-20
PredWidth<-(max(SummaryTable[,Predictor])-min(SummaryTable[,Predictor]))/PredSplit
PredWidthList[[Predictor]]<-PredWidth
PredMin<-min(SummaryTable[,Predictor])
PredMinList[[Predictor]]<-PredMin
PredMax<-max(SummaryTable[,Predictor])
PredMaxList[[Predictor]]<-PredMax

if(PredWidth!=0)
{
Count<-matrix(0,nrow=OutSplit,ncol=PredSplit)
PlotCount<-matrix(0,nrow=OutSplit,ncol=PredSplit)
for(row in 1:length(SummaryTable[,Predictor]))
{
PredIndex<-ceiling((SummaryTable[row,Predictor]-PredMin)/PredWidth)
OutIndex<-ceiling((SummaryTable[row,Outcome]-OutMin)/OutWidth)
if (PredIndex==0) PredIndex<-1
if (OutIndex==0) OutIndex<-1
PlotCount[OutSplit+1-OutIndex,PredIndex]<-PlotCount[OutSplit+1-OutIndex,PredIndex]+1
Count[OutIndex,PredIndex]<-Count[OutIndex,PredIndex]+1
}
PlotCount[PlotCount==0]<-0.1# Give 0.1 count to zero counts, to prevent INF when normalizing later
Count[Count==0]<-0.1
Density<-scale(Count, center=FALSE, scale=colSums(Count)) #Normalize to colsum
PlotDensity<-scale(PlotCount, center=FALSE, scale=colSums(PlotCount))
NormDensity<-sweep(Density, 1, OutDensity, FUN="/")
#NormDensity<-Density 
NormPlotDensity<-sweep(PlotDensity, 1, OutDensity, FUN="/")
#NormPlotDensity<-PlotDensity
NormDensity<-log2(NormDensity)
NormPlotDensity<-log2(NormPlotDensity)
#NormDensity[NormDensity<0.05]<-0#Cutoff depends on normalization
#NormPlotDensity[NormPlotDensity<0.05]<-0
#NormDensity[NormDensity<2]<-0#Cutoff depends on normalization
#NormPlotDensity[NormPlotDensity<2]<-0
#NormDensity[is.na(NormDensity)]<-0
#NormPlotDensity[is.na(NormPlotDensity)]<-0
NormDensityList[[Predictor]]<-NormDensity
#plot(SummaryTable[,Predictor],SummaryTable[,Outcome],xlab = Predictor, ylab=Outcome)
#plot(NormPlotDensity, main=Predictor)
}
}

######Evaluate Practice set
SummaryTable<-EvaluateTable
DensitySum<-vector(mode="numeric",length=OutSplit)
PredictedOutcome<-matrix(nrow=length(SummaryTable[,Outcome]),ncol=2)
colnames(PredictedOutcome)<-c("Estimate","Weight")

for(row in 1:length(SummaryTable[,Outcome]))
{
  DensitySum<-rep(0,length=OutSplit)
for(Predictor in names(NormDensityList))
{
  PredIndex<-ceiling((SummaryTable[row,Predictor]-PredMinList[[Predictor]])/PredWidthList[[Predictor]])
  if (PredIndex<1) PredIndex<-1
  if (PredIndex>20) PredIndex<-20
  DensitySum<-DensitySum+NormDensityList[[Predictor]][,PredIndex]
}
DensitySum<-2**DensitySum
SumDensity<-sum(DensitySum)
PredictedOutcome[row,"Weight"]<-log(SumDensity)
SumWeightDensity<-0
for(Index in 1:OutSplit)
{
  SumWeightDensity<-SumWeightDensity+(Index-0.5)*DensitySum[Index] #Subtract 0.5 
}
RangeEstimate<-(SumWeightDensity/SumDensity)
Estimate<-OutMin+OutWidth*RangeEstimate
PredictedOutcome[row,"Estimate"]<-Estimate
}

GrayPal<-colorRampPalette(c("white","black"))
GrayWeight<-GrayPal(100)[cut(PredictedOutcome[,"Weight"],breaks = 100,labels = FALSE)]
plot(SummaryTable[,Outcome],PredictedOutcome[,"Estimate"],main=Outcome, xlab="Actual Outcome", ylab="Predicted Outcome")
plot(SummaryTable[,Outcome],PredictedOutcome[,"Estimate"],col=GrayWeight,main=Outcome, xlab="Actual Outcome", ylab="Predicted Outcome")
}
 
