## All packages needed for this code
#install.packages("PerformanceAnalytics")
#install.packages("plotrix")
#install.packages("andrews")
#install.packages(c("factoextra", "dendextend"))
#install.packages("caret")

library(andrews)
library(PerformanceAnalytics)
library(scales)
library(plotrix)
library(pls)
library(cluster)
library(factoextra)
library(rpart)
library(caret)


## Set working directory
setwd("C://Users//Aditya//Desktop//AppliedMultivariateStatisticalAnalysis//Project/updated_data")

############################################################################################################
#Show total view trend till date
total_views <- read.csv("total_views.csv", header=T)
head(total_views)

x = total_views$Date
y = total_views$Views
plot(x,y,col=heat.colors(5),main="View trend",xlab="Date",ylab="Views")
lines(lowess(x, y), col = "blue", lwd = 3)
legend("topleft", legend = c("Raw data", "Smooth fit line"),
       lwd = 3, col = c("black", "blue"))

########################################--DEVICES ANALYSIS--##############################################################
## Read data from devices data file

dev_view <- read.csv("device_view.csv", header=T)
head(dev_view[,c(2:6)])

## Visualize correlation

# Center the dataframe
d<-apply(dev_view[,c(2:6)], 2, scale, scale=FALSE, center=TRUE)

cor_devices = cor(d)
chart.Correlation(cor_devices, histogram = TRUE, method = "pearson")

## Generate Pie Chart of total view coverage

#Extract required data
dev_stat <- read.csv("device_sum.csv", header=T)
head(dev_stat[,c(1,3,4)])

#Extract the important columns
dev_stat <- dev_stat[,c(1,3,4)]

xaxisTitles<- colnames(dev_stat)

win.graph()
par(mfrow=c(2,1))
require(ggplot2)
se <- function(x) sqrt(var(x)/length(x))
plot <- ggplot(dev_stat, aes(Device_type, Views, fill = Device_type)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_errorbar(aes(ymin = Views - se(Views), 
                    ymax = Views + se(Views), 
                    color = Device_type), 
                width = .2) + 
  scale_y_continuous(breaks = 0:nlevels(dev_stat$Device_type)) +
  theme_gray() + scale_x_discrete(labels = xaxisTitles)+  
  ggtitle("Device contribution to views") +
  geom_text(aes(label = paste0(Views, " views \n", Watch_time_hours," hrs")), position = position_stack(vjust=0.5))+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(color="red"),
        axis.line = element_blank())
plot

###########################---------------PCA

##Analyze with PCA model
#To decide whether to use cor=T/F, check if min/max in dataframe are comparable [if no, cor=T]
summary(d)
pca_devices = princomp(d,cor=T,scores=T)
summary(pca_devices)
par(mfrow=c(2,1))
plot(pca_devices,col="#FFF8E7", main="PCA analysis: DEVICES DATA")
mtext("Principal components", side=1) 
plot(pca_devices,type="l",main='Screeplot')

##Look at the loadings of first 2 PCs since they explain 93% of the data [0.832, 0.099 resp]
pca_devices$loadings[,1:2]

###########################---------------BIPLOT

#Can directly do biplot(pca_devices) but its easier to reconstruct
par(mfrow=c(1,1))
scalefactor<-0.07
scores<-pca_devices$scores*scalefactor
win.graph()
plot(scores[,2]~scores[,1],,xlim=c(-0.2,1.1),ylim=c(-2,2),main='Biplot: DEVICES',xlab='PC1-83% explained variance',ylab='PC2-10% explained variance',col="blue")

for (i in seq(1,nrow(pca_devices$loadings),by=1))
  arrows(0,0,pca_devices$loadings[i,1],pca_devices$loadings[i,2],lwd=2)
text(pca_devices$loadings[,1]+.16,pca_devices$loadings[,2]+0.02*2,
     as.character(dimnames(cor_devices)[[2]]),font=3)

draw.circle(0,0,1,border='black')
legend("bottomright",legend=c("PC scores","PC eigenvectors"),col=c("blue","black"),pch=16)

########################################--TRAFFIC ANALYSIS--##############################################################
## Read data from traffic data file
traffic_view <- read.csv("traffic_view.csv", header=T)
head(traffic_view[,c(2:12)])

par(mfrow=c(1,1))
## Visualize correlation

#Center the dataframe
t<-apply(traffic_view[,c(2:12)], 2, scale, scale=FALSE, center=TRUE)

cor_traffic = cor(t)
chart.Correlation(cor_traffic, histogram = TRUE, method = "pearson")

#Another way to view correlation between plots
library(GGally)
ggpairs(traffic_view[,c(2:12)])

###########################
## Display proportion of data contributed by each attribute
#Extract required data
traffic_stat <- read.csv("traffic_sum.csv", header=T)
head(traffic_stat)

#Extract the important columns
traffic_stat <- traffic_stat[,c(1,2,3,5,6)]
traffic_stat[is.na(traffic_stat)] = 0

xaxisTitles<- colnames(traffic_stat)

require(ggplot2)
se <- function(x) sqrt(var(x)/length(x))
plot <- ggplot(traffic_stat, aes(Traffic_source, Views, fill = Traffic_source)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_errorbar(aes(ymin = Views - se(Views), 
                    ymax = Views + se(Views), 
                    color = Traffic_source), 
                width = .2) + 
  scale_y_continuous(breaks = 0:nlevels(traffic_stat$Traffic_source)) +
  theme_gray() + scale_x_discrete(labels = xaxisTitles)+  
  ggtitle("Traffic source for views (inside: Impressions by attribute)") +
  geom_text(aes(label = paste0(Impressions)), position = position_stack(vjust=0.5))+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(color="red"),
        axis.line = element_blank())
plot+ coord_polar()

###########################---------------PCA

##Analyze with PCA model
pca_traffic = princomp(t,cor=T)
summary(t)
plot(pca_traffic,col="#FFF8E7", main="PCA analysis: TRAFFIC SOURCE")
mtext("Principal components", side=1) 
plot(pca_traffic,type="l",main='Screeplot')

summary(pca_traffic)
##Look at the loadings of first 5 PCs since they explain 87.9% of the data [0.591,0.097,0.075,0.063,0.053 resp]
pca_traffic$loadings[,1:5]

###########################---------------BIPLOT

#Try biplot even though it is useless since more than 2 PCs are significant here
par(mfrow=c(1,1))
scalefactor<-0.07
scores<-pca_traffic$scores*scalefactor
win.graph()
plot(scores[,2]~scores[,1],,xlim=c(-0.2,1.1),ylim=c(-2,2),main='Biplot: TRAFFIC',xlab='PC1-60% explained variance',ylab='PC2-10% explained variance',col="blue")

for (i in seq(1,nrow(pca_traffic$loadings),by=1))
  arrows(0,0,pca_traffic$loadings[i,1],pca_traffic$loadings[i,2],lwd=2)
text(pca_traffic$loadings[,1]+.16,pca_traffic$loadings[,2]+0.02*2,
     as.character(dimnames(cor_traffic)[[2]]),font=3)

draw.circle(0,0,1,border='black')
legend("bottomright",legend=c("PC scores","PC eigenvectors"),col=c("blue","black"),pch=16)


########################################--COMBINED META DATA ANALYSIS--##########################################################

mega_devtra = cbind(d,t)
#See how these variables affect the view trend
view_all <- cbind(y,mega_devtra)
colnames(view_all)[colnames(view_all) == "y"] <- "Views"
chart.Correlation(cor(view_all), histogram = F, method = "pearson")

m<-apply(mega_devtra, 2, scale, scale=FALSE, center=TRUE)

summary(mega_devtra)

###########################---------------PCA

##Analyze with PCA model
pca_mega = princomp(mega_devtra,cor=T)

par(mfrow=c(2,1))
plot(pca_mega,col="#FFF8E7", main="PCA analysis: DEVICE + TRAFFIC ")
mtext("Principal components", side=1) 
plot(pca_mega,type="l",main='Screeplot')

summary(pca_mega)
##Look at the loadings of first 2 PCs since they explain 66% & 7% variance resp [0.659,0.068]
pca_mega$loadings[,1:2]


###########################---------------FACTOR ANALYSIS

## FACTOR ANALYSIS
fam <- factanal(m,factors=4,scores="Bartlett",rotation="varimax")
#See effect of individual variables on each factor
fam$loadings
#Check psi (specific variances)
sp_var <- as.data.frame(fam$uniquenesses)
plot(c(1:16),sp_var$`fam$uniquenesses`,main="Factor Analysis",xlab="Variable",ylab="FA psi score",xlim=c(0,17),ylim=c(0,0.8),col= ifelse(sp_var$`fam$uniquenesses` >= 0.1, "red", "black"))
text(sp_var,labels=dimnames(sp_var)[[1]],cex=0.7,pos=3)
legend("topright",legend=c(">10% variance","<10% variance"),col=c("red","black"),pch=16)

###########################---------------PCR

## TRY PCR
#Modify all 0's to 1 so that the model can run
view_all_rep = view_all
pcr_o1 <- pcr(y ~ view_all_rep[,c(2:17)], data = as.data.frame(view_all_rep), scale=F,validation = "CV")
summary(pcr_o1)
plot(RMSEP(pcr_o1), legendpos = "topright", main="PCR: Components needed")
predplot(pcr_o1)

###########################---------------PLSR

## TRY PLSR
pls_o1 <- plsr(y ~ view_all[,c(2:17)] , ncomp = 10, data = as.data.frame(view_all), validation = "LOO")
#Check in LOOCV there is no significant change after 6th component
summary(pls_o1)
#2 or 1 components seem to be enough to describe the data using PLSR
plot(RMSEP(pls_o1), legendpos = "topright", main="PLSR: Components needed")
#Plot the loadings of the first 2 components
plot(pls_o1, "loadings", comps = 1:2, legendpos = "topleft", xlab = "nm")
abline(h = 0)
#1 component is enough

colnames(view_all_rep)[colnames(view_all_rep) == "y"] <- "Views"

#I am overlaying the PC component of Views on the mega biplot to confirm my 
#hypothesis that majority of variables are contributing to it positively
pca_all<-princomp(view_all_rep,cor=T)


###########################---------------BIPLOT

#Try biplot 
par(mfrow=c(1,1))
scalefactor<-0.06
scores<-pca_mega$scores*scalefactor
#win.graph()
plot(scores[,2]~scores[,1],xlim=c(-0.2,1.1),ylim=c(-1,1),main='Biplot: DEVICE + TRAFFIC',xlab='PC1-66% explained variance',ylab='PC2-7% explained variance',col="green")

for (i in seq(1,nrow(pca_mega$loadings),by=1))
  arrows(0,0,pca_mega$loadings[i,1],pca_mega$loadings[i,2],lwd=1,col="orange")

draw.circle(0,0,1,border='black')
arrows(0,0,pca_all$loadings[1,1],pca_all$loadings[1,2],lwd=2)
legend("bottomright",legend=c("PC scores","PC eigenvectors","Views"),col=c("green","orange","black"),pch=16)

summary(pca_all)
#Plot the loadings of the first 2 components since they explain 67% & 6% variance respectively
pca_all$loadings[,1:2]

###########################---------------HIERARCHICAL CLUSTER ANALYSIS

# Cluster
#Treat all records as profiles
plot(c(0,16),c(-100,500),type="n",ylab="Range",xlab="Variables",main="Profile Plot")
# Use a loop to generate a profile line for each observation.
for (k in (1:160))
{
  points(1:16,mega_devtra[k,],type="l")
}
#Try Andrews plot
andrews(mega_devtra,type=4,ymax=2,clr=2)
#Not as good as traditional points above

#Ward.D method
par(mfrow=c(1,1))
clust_mega_ward <- hclust(dist(mega_devtra), method="ward.D")
plot(clust_mega_ward,xlab="Device + Traffic data",ylab="Ward.D method")
extract_clust_mega_ward <- cutree(clust_mega_ward,k=3)
table(extract_clust_mega_ward)
clusplot(mega_devtra,extract_clust_mega_ward,stand=TRUE,labels=3,main="Ward D method")

# Centroid method
win.graph()
par(mfrow=c(1,1))
clust_mega <- hclust(dist(mega_devtra), method="centroid")
plot(clust_mega,xlab="Device & Traffic data",ylab="Centroid method",sub="")
extract_clust_mega <- cutree(clust_mega,k=3)
table(extract_clust_mega)
clusplot(mega_devtra,extract_clust_mega,stand=TRUE,labels=3,main="Centroid method")

#Display dendogram more nicely
win.graph()
fviz_dend( hcut(mega_devtra, k = 3, stand = TRUE), k_colors = "aaas",tittle = "Lower", ggtheme = theme_classic(),cex = 0.5, k = 3, rect = T)

###########################---------------TBM

#Run Tree building methodology
#Add another variable, categorical "videos added"-> UPLOADED/NOT_UPLOADED
new_videos <- read.csv("videos_added.csv", header=T)
#If number of videos uploaded is not 0, mark it as uploaded
new_videos$Video_added[new_videos['Video_added'] > 0] <- "UPLOADED"
new_videos$Video_added[new_videos['Video_added']== 0] <- "NOT_UPLOADED"
added_labels <- as.factor(new_videos$Video_added)

new_mega_devtra <- as.data.frame(cbind(added_labels, mega_devtra))

# Fit the model

#TRAIN & TEST SETS
indexes = createDataPartition(new_mega_devtra$added_labels, p = .85, list = F)
train = new_mega_devtra[indexes, ]
test = new_mega_devtra[-indexes, ]

test_x = test[, 2:17]
test_y = test[,1]

#Choose method as class since we are trying to predict a factor variable
tree_mega <- rpart(added_labels ~.,data=train,method="class")
rpart.control(xval=10)
summary(tree_mega)

win.graph()
plot(tree_mega,uniform=T)         
text(tree_mega,splits=T,all=T)  
title("Video upload behaviour")
legend("topleft",legend=c("1: NOT_UPLOADED","2: UPLOADED"))

# Results of cross validation 
printcp(tree_mega)
plotcp(tree_mega)

#Plot cost complexity vs # splits
plot(tree_mega$cptable[,2],tree_mega$cptable[,1],xlab='Number of splits',ylab='Cost complexity param (cp)',type='l',main="Performance of tree")
abline(0.013,0,col="red",lty=2)
legend("topright",legend=c("cp for particular split", "chosen cp to prune tree"),col=c("black","red"),lty=1)

# Pruning the tree at a specific cost-complexity parameter cp
tree_prune<-prune(tree_mega,cp=0.013) 

summary(tree_prune)

# Plot the pruned tree
win.graph()
plot(tree_prune)
text(tree_prune)
title("Pruned tree at cp=0.013")
legend("topright",legend=c("1: NOT_UPLOADED","2: UPLOADED"))
#No difference observed before/after pruning [too few variables]

#Check accuracy
pred_y = as.data.frame(predict(tree_prune, data.frame(test_x)))
pred_label <- ifelse(pred_y$'1' > pred_y$'2', 1,2)

print(data.frame(test_y, pred_label))

mse = mean((test_y - pred_label)^2)
mae = caret::MAE(test_y, pred_label)
rmse = caret::RMSE(test_y, pred_label)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)
x = 1:length(test_y)

plot(x, test_y, col = "red", type = "l", lwd=2, main = "test data prediction")
lines(x, pred_label, col = "blue", lwd=2)
legend("topright",  legend = c("original label", "predicted label"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))
grid()
acc <- length(test_y[test_y == pred_label])/length(test_y)
############################################################################################################