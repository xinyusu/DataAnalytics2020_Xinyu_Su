days<-c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')
temp<-c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed<-c('T','T','F','F','T','T','F')
help("data.frame")
RPI_Weather_Week <- data.frame(days,temp,snowed)
RPI_Weather_Week
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)
RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c('days','temp')]
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset = snowed==TRUE)

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]
dec.snow<-order(-RPI_Weather_Week$temp)
dec.snow
empty.DataFrame <- data.frame()
v1<-1:10
v1
letters
v2<-letters[1:10]
df<-data.frame(col.name.1=v1,col.name.2=v2)
write().csv(df,file='saved_df1.csv')
df2<- read.csv('saved_df1.csv')
df2

#exercise
EPI2020<-read.csv('2010EPI_data.csv')
EPI2020
View(EPI2020)
attach(EPI2020)
fix(EPI2020)
EPI
tf<-is.na(EPI)
E <- EPI[!tf]
summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)

GRUMP_data <-read.csv('GPW3_GRUMP_SummaryInformation_2010.csv')

#distribution
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(EPI); qqline(EPI)

x <- rt(250, df = 5)
qqnorm(x); qqline(x)

qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
boxplot(EPI,DALY)
help(distributions)
#filtering
EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
#GPW3_GRUMP
GRUMP_data <-read.csv('GPW3_GRUMP_SummaryInformation_2010.csv')

#exercise

GRUMP_data
View(GRUMP_data)
attach(GRUMP_data)
fix(GRUMP_data)
Resolution
tf<-is.na(Resolution)
E <- Resolution[!tf]
summary(Resolution)
fivenum(Resolution,na.rm=TRUE)
stem(Resolution)
hist(Resolution)
hist(Resolution, seq(0, 400, 1.0), prob=TRUE)
lines(density(Resolution,na.rm=TRUE,bw=1.))
rug(Resolution)


#distribution
plot(ecdf(Resolution), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(Resolution); qqline(Resolution)

x <- rt(250, df = 5)
qqnorm(x); qqline(x)

qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
boxplot(Resolution,DALY)
#filtering
ReLand<-Resolution[!Landlock]
Rland <- ReLand[!is.na(ReLand)]
hist(Rland)
hist(Rland, seq(0, 300, 1.0), prob=TRUE)



