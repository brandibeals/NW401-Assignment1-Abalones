# Beals, Brandi
# Exploratory Data Analysis Assignment 1
# PREDICT 401 DL-55

# Set working directory for either work or home computer
# setwd("C:/Users/bbeals/Dropbox (Personal)/Masters in Predictive Analytics/401-DL-55/Data Analysis Assignment 1")
setwd("C:/Users/Brara/Dropbox/Masters in Predictive Analytics/401-DL-55/Data Analysis Assignment 1")
# Read in the data
mydata <- read.csv("abalones.csv",sep=",")
str(mydata)
# Calculating fields VOLUME and RATIO
mydata$VOLUME <- mydata$LENGTH * mydata$DIAM * mydata$HEIGHT
mydata$RATIO <- mydata$SHUCK / mydata$VOLUME
# Summary statistics and a quick review of the data
summary(mydata)
head(mydata)
# Table of record counts for SEX and CLASS columns with margins
sexclass<-table(mydata$CLASS,mydata$SEX)
addmargins(sexclass)
classsex<-table(mydata$SEX,mydata$CLASS)
addmargins(classsex)
# Bar charts
jpeg(filename="sexclassbarplot.jpg")
barplot(sexclass,main="Distribution of CLASS by SEX",ylab="Number of Records",ylim=c(0,200),xlab="SEX",beside=TRUE,legend=rownames(sexclass),args.legend=list(horiz = TRUE))
dev.off()
jpeg(filename="classsexbarplot.jpg")
barplot(classsex,main="Distribution of SEX by CLASS",ylab="Number of Records",ylim=c(0,200),xlab="CLASS",beside=TRUE,legend=rownames(classsex),args.legend=list(horiz = TRUE))
dev.off()
# Simple random sample of 200 and scatterplot
set.seed(123)
work<-mydata[sample(1:nrow(mydata),200,replace=FALSE),]
jpeg(filename="workscatterplot.jpg",width=700,height=700)
plot(work[,2:6])
dev.off()
# Plot WHOLE versus VOLUME
jpeg(filename="wholevolumescatterplot.jpg")
plot(mydata$WHOLE,mydata$VOLUME,main="Scatterplot Comparison of WHOLE to VOLUME",xlab="WHOLE",ylab="VOLUME")
dev.off()
# Plot SHUCK verus WHOLE
jpeg(filename="shuckwholescatterplot.jpg")
plot(mydata$SHUCK,mydata$WHOLE,main="Scatterplot Comparison of SHUCK to WHOLE",xlab="SHUCK",ylab="WHOLE")
abline(0,max(mydata$WHOLE/mydata$SHUCK))
dev.off()
# Charts displaying RATIO by SEX
jpeg(filename="ratiosexmixedplots.jpg",width=700,height=700)
par(mfrow = c(3,3)) 
hist(mydata$RATIO[mydata$SEX=="F"],main="Female RATIO",xlab="")
hist(mydata$RATIO[mydata$SEX=="I"],main="Infant RATIO",xlab="",col="grey")
hist(mydata$RATIO[mydata$SEX=="M"],main="Male RATIO",xlab="",col="dark grey")
boxplot(mydata$RATIO[mydata$SEX=="F"],main="Female RATIO")
boxplot(mydata$RATIO[mydata$SEX=="I"],main="Infant RATIO",col="grey")
boxplot(mydata$RATIO[mydata$SEX=="M"],main="Male RATIO",col="dark grey")
qqnorm(mydata$RATIO[mydata$SEX=="F"],main="Female RATIO")
qqline(mydata$RATIO[mydata$SEX=="F"])
qqnorm(mydata$RATIO[mydata$SEX=="I"],main="Infant RATIO",col="grey")
qqline(mydata$RATIO[mydata$SEX=="I"])
qqnorm(mydata$RATIO[mydata$SEX=="M"],main="Male RATIO",col="dark grey")
qqline(mydata$RATIO[mydata$SEX=="M"])
dev.off()
par(mfrow = c(1,1))
# Boxplots of VOLUME and WHOLE differentiated by CLASS
jpeg(filename="volumewholemixedplots.jpg",width=700,height=700)
par(mfrow = c(2,2))
boxplot(mydata$VOLUME ~ mydata$CLASS,ylab="VOLUME")
boxplot(mydata$WHOLE ~ mydata$CLASS,ylab="WHOLE")
plot(mydata$RINGS,mydata$VOLUME,ylab="VOLUME",xlab="RINGS")
plot(mydata$RINGS,mydata$WHOLE,ylab="WHOLE",xlab="RINGS")
dev.off()
par(mfrow = c(1,1))
# Aggregate VOLUME, SHUCK, and RATIO by SEX and CLASS
volume_matrix<-aggregate(mydata$VOLUME~mydata$SEX+mydata$CLASS,FUN=mean)
volume_matrix<-matrix(volume_matrix[,3],nrow=3,ncol=5,dimnames=list(c("F","I","M"),c("A1","A2","A3","A4","A5")))
shuck_matrix<-aggregate(mydata$SHUCK~mydata$SEX+mydata$CLASS,FUN=mean)
shuck_matrix<-matrix(shuck_matrix[,3],nrow=3,ncol=5,dimnames=list(c("F","I","M"),c("A1","A2","A3","A4","A5")))
ratio_matrix<-aggregate(mydata$RATIO~mydata$SEX+mydata$CLASS,FUN=mean)
ratio_matrix<-matrix(ratio_matrix[,3],nrow=3,ncol=5,dimnames=list(c("F","I","M"),c("A1","A2","A3","A4","A5")))
# ggplots of RATIO, VOLUME, and SHUCK versus SEX and CLASS
jpeg(filename="ratioggplot.jpg",height=370)
ggplot(data=aggregate(RATIO~SEX+CLASS,data=mydata,FUN=mean),aes(x=CLASS,y=RATIO,group=SEX,color=SEX))+geom_line()+geom_point(size=2)+ggtitle("Figure 8: Mean Ratio per Class")
dev.off()
jpeg(filename="volumeggplot.jpg",height=370)
ggplot(data=aggregate(VOLUME~SEX+CLASS,data=mydata,FUN=mean),aes(x=CLASS,y=VOLUME,group=SEX,color=SEX))+geom_line()+geom_point(size=2)+ggtitle("Figure 9: Mean Volumn per Class")
dev.off()
jpeg(filename="shuckggplot.jpg",height=370)
ggplot(data=aggregate(SHUCK~SEX+CLASS,data=mydata,FUN=mean),aes(x=CLASS,y=SHUCK,group=SEX,color=SEX))+geom_line()+geom_point(size=2)+ggtitle("Figure 10: Mean Shuck Weight per Class")
dev.off()