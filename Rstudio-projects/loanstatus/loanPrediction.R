#import the dataset of loan status
data <- read.csv("specify your file path")

summary(data)#to see all the information regarding data
str(data)
xtabs(~ loan_status+ grade,data = data)
data$grade<-factor(data$grade)

datasort<-sort(sample(nrow(data),nrow(data()*.75)))
train <- data[datasort,]#asssign 75% of the total data to train set
test <- data[-datasort,]#assign remaining 25% of the total dataset to train dataset

nrow(train)#print no. of rows to train dataset
nrow(test)# print no. of rows in test dataset

logistic_reg <- glm(loan_status ~.,data = train,family = "binomial") 
summary(logistic_reg)$coefficient # find coefficient of the data set

#perform step wise logistic regression
spt_logit = step(logistic_reg)

summary_step=summary(spt_logit)$coefficient
summary_step

Oddratios<-exp(coef(spt_logit))
summary_coeff<-cbind(variable=row.names(summary_step),Oddratios,summary_step)

row.names(summary_coeff)=NULL
stz.ciff<- function(logitmodel){
  b<-summary(logitmodel)$coef[-1,1]
  sx<-sapply(logitmodel$model[-1],sd)
  beta<- (3^(1/2))/pi*sx*b
  
}

std.coff=data.frame(StandardCoeff=stz.ciff(spt_logit))
std.coff=cbind(variable=row.names(std.coff),std.coff)
row.names(std.coff)=NULL

final=merge(summary_coeff,std.coff,by="variable",all.x=TRUE)
pred=predict(spt_logit,test,type="response")

finaldata=cbind(test,pred)

library(ROCR)
pred_val=prediction(pred,finaldata$loan_status)
acc<-performance(pred_val,"acc")
ind<- which.max(slot(acc,"y.values")[[1]])
acc1<-slot(acc,"y.values")[[1]][ind]
cutoff<-slot(acc,"x.values")[[1]][ind]

print(c(accuracy=acc1,cutoff=cutoff))

perf_val<-performance(pred_val,"auc")
perf_val

plot(performance(pred_val,measure="lift",x.measure="rpp"),colorize=TRUE)
perf_roc<-performance(pred_val,"tpr","fpr")
plot(perf_roc,col=green,lwd=1.5)

ks1=max(attr(perf_roc,"y.values")[[1]]-attr(perf_roc,"x.values")[[1]]))
