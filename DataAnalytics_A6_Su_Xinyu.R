setwd('/Users/suxinyu/Documents/data analytics/project')
df<- read.csv('data.csv')
head(df)
View(df)

data1<-df

library(dplyr)          
library(magrittr)       
library(caret)          
library(ggplot2)
library(ggthemes) 
library(lubridate)
library(reshape)
## number
print(paste("users receive coupon but don't redeem it:", nrow(subset(df,Date == 'null'& Coupon_id!='null'))))
print(paste("purchase happened but no coupon had been received:", nrow(subset(df,Date != 'null'& Coupon_id=='null'))))
print(paste("users receive coupon and redeem it:", nrow(subset(df,Date != 'null'& Coupon_id!='null'))))
nrow(subset(df,Date_received == 'null'))
#data cleaning & feature selection
nrow(data1)
nrow(data1[data1$Coupon_id != 'null',])

head(data1)
set.seed(1234)
data1 <- sample_n(data1,size = 40000)
nrow(data1)
View(data1)
# distance convert null to -1
names(data1)
data1$Distance <- as.character(data1$Distance)
data1$Distance[data1$Distance == 'null'] <- -1

View(data1)
## minimum,maximum,average,median distance
str(data1)

data1 %>% group_by(User_id ) %>%
  summarise(min_dis = min(as.numeric(Distance)),
            max_dis = max(as.numeric(Distance)),
            ave_dis = round(mean(as.numeric(Distance)),1),
            medi_dis = median(as.numeric(Distance))) ->c

da1<- as.data.frame(c)
class(da1)
head(da1)
# discount rate
unique(data1$Discount_rate)
nrow(data1)
head(data1)
d<-data1[c(1,4)]
for (i in (1:nrow(d))){
  if (d$Discount_rate[i] == 'null'){
    d$rate[i] = 1
  } else if (grepl(':',d$Discount_rate[i]) == TRUE){
    ls1 <- strsplit(as.character(d$Discount_rate[i]),':')
    ra1 <- as.numeric(unlist(ls1)[1])
    ra2 <- as.numeric(unlist(ls1)[2])
    d$rate[i] = 1- round(ra2/ra1,2)
    
  } else {
    d$rate[i] = d$Discount_rate[i]
  }
}


#weekday type
data1$Date_received <- as.Date(data1$Date_received,'%Y%m%d')
data1$Date <- as.Date(data1$Date,'%Y%m%d')

nrow(subset(data1,is.na(Date_received) == TRUE))

data1$dayreceived <- wday(data1$Date_received)
data1$dayconsum <- wday(data1$Date)
data1$diffday <- difftime(data1$Date, data1$Date_received, units = "days")
names(data1)


data1$weekdaytype[data1$dayreceived == 1 | data1$dayreceived == 7 ] <- 1
data1$weekdaytype[data1$dayreceived != 1 & data1$dayreceived != 7 ] <- 0
#label
data1$label[is.na(data1$Date_received) == TRUE] <- -1 #not recieve
data1$label[data1$diffday<=30 & is.na(data1$diffday) != TRUE & is.na(data1$Date_received) != TRUE & is.na(data1$Date ) != TRUE] <- 1
data1$label[is.na(data1$Date_received) != TRUE & is.na(data1$Date) == TRUE|data1$diffday> 30]<- 0
data1[is.na(data1$Date) != TRUE,]

#merge column
head(d)
head(da1)
head(data1)
d<-d[-2]
data1 <- merge(data1,da1,by='User_id')
data1 <- merge(data1,d,by='User_id')

View(data1)
names(data1)
#plot
theme_set(theme_bw())

#probability of customer using coupons
#The number of coupons customer received and used in different dates
df_recei <- data1[is.na(data1$Date_received) != TRUE,]
df_recei_count <- as.data.frame(table(df_recei$Date_received))
head(df_recei_count)
df_recei_count$Var1 <- as.Date(df_recei_count$Var1)

df_use <- data1[is.na(data1$Date) != TRUE,]
nrow(data1[is.na(data1$Date) != TRUE,])
df_use_count <- as.data.frame(table(df_recei$Date))
head(df_use_count)
df_use_count$Var1 <- as.Date(df_use_count$Var1)

ggplot(df_recei_count, aes(x = Var1,y= Freq,fill='number of coupon people received')) + 
  geom_bar(stat='identity',position= position_dodge())+
  ylim (0,300)+
  geom_bar(data= df_use_count,aes(x = Var1,y= Freq,fill='number of counpon people used'),stat='identity',position= position_dodge())+
  xlab('The number of coupons customer received and used in different dates')+
  ylab('count')
# weekdaytype & use
names(data1)
data1$weekdaytype
head(df_use)
df_use <- df_use[is.na(df_use$weekdaytype)!=TRUE,]
df_use$Date <- as.Date(df_use$Date)
ggplot(df_use, aes(Date,fill=factor(weekdaytype)))+
  geom_bar(position="stack")+
  facet_grid(weekdaytype ~ .)+
  labs(title="The number of coupons used in different weekdays")+
  scale_fill_manual(values =alpha(c("#56B4E9", "#E69F00")),name = 'weekdaytype')
table(data1$weekdaytype)
df_weekday_count <-as.data.frame(table(data1$weekdaytype))

nrow(df_weekday_count)
df_weekday_count <- mutate(df_weekday_count,percentage = c(round(700457/(700457+352825),2),round(352825/(700457+352825),2)))
ggplot(df_weekday_count,aes(x=Var1,y=percentage, fill=Var1) )+ 
  geom_bar(stat='identity',position= position_dodge(),size = 0.5)+
  geom_text(aes(label = percentage), position = position_dodge(0.9),size = 5)+
  xlab('Weekday')+
  labs(title="The percentage of coupon used in weekends or weekdays",fill='weekdaytype')

#cor
library(corrplot)
par(mfrow = c(1,1))
e <- data1[-c(1:4,6,7,9)]
e<- e[is.na(e$diffday) != TRUE,]
head(e)
str(e)
e$Distance<- as.numeric(e$Distance)
e$diffday<- as.numeric(e$diffday)
data_cor<-cor(e)
class(data_cor)
corrplot(data_cor) 

#split
#for customer use probability
names(data1)
#is.na(data1$Date_received ) !=TRUE,
df_cus<- data1[c(1,5,8:17)]
names(df_cus)
df_cus$dayconsum[is.na(df_cus$dayconsum)==TRUE]<--1
df_cus$dayreceived[is.na(df_cus$dayreceived) == TRUE] <- -1
df_cus$weekdaytype[is.na(df_cus$weekdaytype) == TRUE] <- -1
df_cus$diffday[is.na(df_cus$diffday)==TRUE] <- -1
df_cus$diffday <- as.numeric(df_cus$diffday)
df_cus$Distance<- as.numeric(df_cus$Distance)
View(df_cus)
## for coupon use probability
df_coupon <- data1[is.na(data1$Date_received ) !=TRUE,c(3,5,8:17)]
table(df_coupon$label)
df_coupon$dayconsum[is.na(df_coupon$dayconsum)==TRUE]<--1
df_coupon$diffday[is.na(df_coupon$diffday)==TRUE] <- -1
df_coupon$weekdaytype[is.na(df_coupon$weekdaytype)==TRUE] <- -1
df_coupon$dayreceived[is.na(df_coupon$dayreceived) == TRUE] <- -1

df_coupon$diffday <- as.numeric(df_coupon$diffday)
df_coupon$Distance<- as.numeric(df_coupon$Distance)
#drop duplicated id
a <- df_cus
df_spl<-df_cus[-1]
a %>% distinct(User_id, .keep_all = TRUE) -> da1
names(da1)
names(df_cus)
df_spl <- da1[-1]
nrow(df_spl)
View(df_spl)
table(df_spl$label)
#split
library(caTools)
df_spl <- df_cus
spl = sample.split(df_spl$label, 0.8)
train = subset(df_spl, spl == TRUE)
test=subset(df_spl,spl==FALSE)
head(train)
names(train)
nrow(train)
##coupon
names(df_coupon)
df_cou_spl <- df_coupon[-1]
spl = sample.split(df_cou_spl$label, 0.8)
train1 = subset(df_cou_spl, spl == TRUE)
test1=subset(df_cou_spl,spl==FALSE)
head(train1)
names(train1)
nrow(train1)
library(e1071)
library(pROC)


##lasso
library(lars)
x.train <- as.matrix(train[-6])
x.test<-as.matrix(train[6])
y.train <- as.matrix(test[-6])
str(x.train)
lar1 <-lars(x.train,x.test,type = "lasso")
plot(lar1)
summary(lar1) #minimum cp
lar1$Cp[which.min(lar1$Cp)]
pre_lasso <-predict(lar1,y.train,s=8)
summary(pre_lasso$fit)
table(test$label,round(pre_lasso$fit,0))
nrow(test)

lasso_roc <- roc(test$label,as.numeric(round(pre_lasso$fit,0)),levels = c(0,1))
plot(lasso_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='lasso ROC')

##
library(rpart)
par(mfrow = c(1,1))
data_rp<-rpart(label~.,data = train)
plot(data_rp,uniform = T,branch = 0.6,margin = 0.05)
text(data_rp,all = T,use.n = T)

#
pre2<-predict(data_rp,test)
table(test$label,round(pre2,0))

rpart_roc <- roc(test$label,as.numeric(round(pre2,0)),levels = c(0,1))
plot(rpart_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='rpart ROC')

df_pro1<-as.data.frame(pre2)
length(df_pro1$pre2[df_pro1$pre2==1])
pro1 <- round(length(df_pro1$pre2[df_pro1$pre2==1])/length(df_pro1$pre2),2)
pro1
#knn
library(kknn)
knn_fit <- kknn(label ~ ., train,test,k=7,distance = 2)
pre_knn <- fitted(knn_fit)
table(test$label, round(pre_knn,0),dnn=c("real value","predicted value"))
knn_roc <- roc(test$label,as.numeric(round(pre_knn,0)),levels = c(0,1))
plot(knn_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='knn ROC')


#####customer

#ridge
library(glmnet)
x.train1 <- as.matrix(train1[-6])
x.test1<-as.matrix(train1[6])
y.train1 <- as.matrix(test1[-6])
ridge <- glmnet(x.train1, x.test1, family = "binomial", alpha = 0)
plot(ridge, xvar = "lambda", label = TRUE)
ridge.y <- predict(ridge, newx = y.train1, type = "response", s=0.05)
y <- as.data.frame(y.train1)
log_roc <- roc(test1$label,as.numeric(round(ridge.y,0)),levels = c(0,1))
plot(log_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='Logistic ROC')
#svm coupon

svmml <- svm(label~.,data=train1,kernel = 'radial')
pre_svm <- predict(svmml,test1)
table(test1$label, round(pre_svm,0))
nrow(as.data.frame(pre_svm))
nrow(test1)
svm_roc <- roc(test1$label,as.numeric(round(pre_svm,0)),levels = c(0,1))
plot(svm_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='SVM ROC')

#logistical
pre_log=glm(label~.,data=train1,family=binomial(link="logit"))
summary(pre_log)

predict=predict.glm(pre_log,type="response",newdata=test1)
table(test1$label, round(predict,0))
log_roc <- roc(test1$label,as.numeric(round(predict,0)),levels = c(0,1))
plot(log_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='Logistic ROC')

