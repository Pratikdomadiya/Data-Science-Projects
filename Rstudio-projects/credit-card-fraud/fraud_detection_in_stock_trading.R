# risk analysis

# Business Objective
# Financial Loses because pf fraud and risky users.
# classifying the user based on their efficiency.

# Trading companies and stocks

#SOLUTION
#we are going to segment customer based on the basis of ranking.
#Negative Impact of Customer

#Data Envelopment Analysis.(DEA)

install.packages("lpSolveAPI")
library(lpSolveAPI)
install.packages("rDEA")
library(rDEA)
install.packages("Benchmarking")
library(Benchmarking)


data= read.csv("specify the path of dataset file")
 dim(data)
 
x1<-data[c(2,4,5,6)]#input column number in dataset
y1<-data[c(7)]#output profit column number in dataset

x2<-data[c(2,4,5,6)]#input column number in dataset
y2<-data[c(8)]#output loss column number in dataset

efficiency1<-dea(x1,y1,RTS="CRS",ORIENTATION="in") #DEA with Input for Profit(CRS)
efficiency2<-dea(x2,y2,RTS="CRS",ORIENTATION="in") #DEA with Input for loss(CRS)

efficiency3<-dea(x1,y1,RTS="VRS",ORIENTATION="in") #DEA with Input for Profit(VRS)
efficiency4<-dea(x2,y2,RTS="VRS",ORIENTATION="in") #DEA with Input for loss(VRS)

efficiency1<-efficiency1$eff
efficiency2<-efficiency2$eff
efficiency3<-efficiency2$eff
efficiency4<-efficiency4$eff

#weighted efficiency
# crs efficiency/vrs efficiency

efficiency_profit=efficiency1/efficiency3
efficiency_loss=efficiency2/efficiency4

final_efficiency= efficiency_profit+(1-efficiency_loss)
final_data=cbind(data[c(1,2,3,4,5,6,7,8)],final_efficiency)

write.csv(final_data,"path where you have to store the data")

#CRS

name=data[1]
inputs<- data[c(2,4,5,6)]
output<-data[c(7,8)]

N<-dim(data)[1]
I<-dim(inputs)[2]
O<-dim(output)[2]

rhs.constraints<-c(rep(0,N),1)
direction.constraints<-c(rep("<=",N),"=")

mat.constraints<-cbind(-1*inputs,output)

for (i in 1:N){
  function_objective<-c(0*rep(1,I),output[i,])
  names(rhs.constraints)<-names(c(inputs[i,],rep(0,O)))
  identical(names(rhs.constraints),names(c(inputs[i,],rep(0,O))))
  function_constraints<-rbind(rhs.constraints,c(inputs[i,],rep(0,O)))
  
  results<-lp("max",function_objective,function_constraints,direction.constraints,rhs.constraints,scale=1,compute.sense=TRUE)
  multipliers<-results$solution
  efficiency<-results$objval
  duals<-results$duals
  
  if(i==1)
  {
    weights<-multipliers
    effcrs<-efficiency
    lambdas<-duals[seq(1,n)]
  }
  else{
    weights<-rbind(weights,multipliers)
    effcrs<-rbind(effcrs,efficiency)
    lambdas<-rbind(lambdas,duals[seq(1,n)])
  }
}

report<- cbind(effcrs,weights)
colnames(report)<-c('efficiency',names(inputs),names(output))
 