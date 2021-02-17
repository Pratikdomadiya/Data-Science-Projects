#IMPORT THE DATASET
RawDataSet <- read.csv("D:/python-program/ML- COURSE-UDEMY/SHIPPING AND TIME ESTIMATION/Wholesale customers data.csv")
RawDataSet

#data exploration
str(RawDataSet)

# get summary of dataset
summary(RawDataSet)

#spending distribution
par(mfrow=c(2,3))
for (i in c(3:8)) {
  hist(RawDataSet[,c(i)], breaks = 200, main = colnames(RawDataSet)[i],xlab="Annunal Spendings", ylab="Number of cust")
  
}

#normalization and discritization
NormalizedData <- cbind(RawDataSet[,c(2,1)],scale(RawDataSet[,c(3:8)]))
summary(NormalizedData)

par(mfrow=c(2,3))
for (i in c(3:8)) 
  hist(NormalizedData[,c(i)], breaks = 200, main = colnames(RawDataSet)[i],xlab="normalized Annunal Spendings", ylab=" Number of cust")
  
  RawDataSet <- within(RawDataSet,FreshQuantile <- as.integer(cut(Fresh,quantile(Fresh,probs=0:5/5),includes.lowest=TRUE)))
  RawDataSet <- within(RawDataSet,MilkQuantile <- as.integer(cut(Milk,quantile(Fresh,probs=0:5/5),includes.lowest=TRUE)))
  RawDataSet <- within(RawDataSet,GroceryQuantile <- as.integer(cut(Grocery,quantile(Fresh,probs=0:5/5),includes.lowest=TRUE)))
  RawDataSet <- within(RawDataSet,FrozenQuantile <- as.integer(cut(Frozen,quantile(Fresh,probs=0:5/5),includes.lowest=TRUE)))
  RawDataSet <- within(RawDataSet,Detergents_PaperQuantile <- as.integer(cut(Detergents_Paper,quantile(Fresh,probs=0:5/5),includes.lowest=TRUE)))
  RawDataSet <- within(RawDataSet,DelicassenQuantile <- as.integer(cut(Delicassen,quantile(Fresh,probs=0:5/5),includes.lowest=TRUE)))
  par(mfrow=c(2,3))
  for (i in c(9:14)) {
    hist(RawDataSet[,c(i)], breaks = 200, main = colnames(RawDataSet)[i],xlab="descritised Annunal Spendings", ylab=" Number of cust")
  }
  
# hirarchical clustering
  
  require(fastcluster)
  require(graphics)
 require(dendextend) #dendogram ploating 
require(gplots)
dist<-dist(RawDataSet[,c(1,2,9:14)]) 
clust<-hclust(dist,method = "average")
plot(clust)

# segmentation of rawdata into 3 diff clusters

seg <- cutree(clust,k=3)
data<- data.frame(RawDataSet,cluster=seg)

#heatmap

dend <- as.dendrogram(clust)
dend<- rotate(dend,1:440)
dend<-color_branches(dend,k=3)
dend<-hang.dendrogram(dend,hang_height = 0.1)
dend<- set(dend,what = c("labels_cex"),0.5)

par(mfrow=c(1,1))
col_func<- function(n) rev(colorspace::heat_hcl(n,c=c(80,30),l=c(30,90),power=c(1/5,1.5)))
gplots::heatmap.2(as.matrix(RawDataSet[,c(1,2,9:14)],
                  main = "heatmap for descritized dataset",
                  srtCol = 20,
                  dendrogram = "row",
                  Rowv = dend,
                  Colv = "NA",
                  trace = "none",
                  margins = c(5.5,0.5),
                  key.xlab = "Cm",
                  denscol = "grey",
                  density.info = "density",
                  col = col_func))

#comparing dendograms

d_raw<-dist(RawDataSet[,c(1:8)])
d_nom<-dist(NormalizedData)
d_dis<-dist(RawDataSet[,c(1,2,9:14)])

hc_raw<-hclust(d_raw,method = "complete")
hc_nom<-hclust(d_nom,method = "complete")
hc_dic_com<-hclust(d_dis,method = "complete")

par(mfrow=c(1,3))
plot(hc_raw)
plot(hc_nom)
plot(hc_dic_com)

# different method
hc_dis_single<-hclust(d_dis,method = "single")
hc_dis_average<-hclust(d_dis,method = "average")
hc_dis_centroid<-hclust(d_dis,method = "centroid")

par(mfrow=c(1,2))
plot(hc_dic_com)
plot(hc_dis_average)


par(mfrow=c(1,2))
plot(hc_dis_centroid)#just like k-mean clustering method
plot(hc_dis_single)#just like k-nearest neighbour clustering method

#visualization technique
wsc_dendlist<-dendlist()
wsc_dendlist<-dendlist(wsc_dendlist,as.dendrogram(hc_dic_com))
wsc_dendlist<- dendlist(wsc_dendlist,as.dendrogram(hc_dis_average))

names(wsc_dendlist)<- c("complete","average")

par(mfrow=c(1,1))
wsc_dendlist %>% dendlist(which = c(1,2)) %>% ladderize %>%
  set("rank_branches") %>%
  tanglegram(common_subtrees_clusters=TRUE)


length(unique(common_subtrees_clusters(wsc_dendlist[[1]],wsc_dendlist[[2]]))[-1])

#final visualization

dend<-as.dendrogram(hc_dic_com)
dend<- rotate(dend,1:440)
dend<-color_branches(dend,k=3)
dend<-hang.dendrogram(dend,hang_height = 0.1)
dend<-set(dend,what = c("labels_cex"),0.5)

par(mfrow=c(1,1))
col_func<-function(n) rev(colorspace::heat_hcl(n,c=c(80,30),l=c(30,30),power=c(1/5,4.5)))
gplots::heatmap.2(as.matrix(RawDataSet[,c(1,2,9:14)]),
                  main="heat map for descritised dataset",
                  srtCol = 20,
                  dendrogram = "row",
                  Rowv = dend,
                  Colv = "NA",
                  trace="none",
                  margins = c(5.5,0.5),
                  key.xlab = "Cm",
                  denscol = "grey",
                  density.info = "density",
                  col=col_func
                  
                  )
