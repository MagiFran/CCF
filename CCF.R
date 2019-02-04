library("forecast")
library("tseries")

Data <- read.csv("D:/R/CCF/TrafficData.csv")
#Data <- ts(Data)

ts()

Sys.Date()


#Data <- ts(Data,start = "2017-07-01")
#View(Data)

X <- ts(Data, frequency = 7)

#X
#X <- ts(X[,c("TRF.IN.ECOMM","TRF.IN.BnM"], start = "2017-07-01", frequency = 7)
#X <- ts(X[,c("TRF.IN.ECOMM","TRF.IN.BnM")], start = "2017-07-01", frequency = 7)
#(X[,c("TRF.IN.ECOMM","TRF.IN.BnM")]
#)

Test <- X[,c("TRF.IN.ECOMM","TRF.IN.BnM")]


#X <- ts(X, start = "2017-07-01", frequency = 7)
#X <- ts(X, start = "2017-07-01", end = "2018-06-25")
#X <- ts(X, start = c("2017-07-01"))
#X <- ts(X, start = c(2017.5))
#View(X)
#X
#X <- ts(X[,c("TRF.IN.ECOMM","TRF.IN.BnM")], start = c(2017.5),frequency = 7)
#remove(Test)
#View(X)
#View(X)
#X
#X <- ts(X[,c("TRF.IN.ECOMM","TRF.IN.BnM")], start = c(2017,07,01),frequency = 1)

head(X,10)
tail(X,10)

#X <- ts(X[,c("TRF.IN.ECOMM","TRF.IN.BnM")], start = c(2017,07,01),frequency = 365)
#X <- ts(X[,c("TRF.IN.ECOMM","TRF.IN.BnM")], start = c(2017,07,01),end = c(2018,06,25))
X <- ts(X[,c("TRF.IN.ECOMM","TRF.IN.BnM")], start = c(2017,07,01),end = c(2018,06,25),frequency = 7)

#X1 <- ts(X[,c("TRF.IN.ECOMM","TRF.IN.BnM")], start = c(2017,07,01),end = c(2018,06,25),frequency = 7)
X1 <- ts(X[,c("TRF.IN.ECOMM","TRF.IN.BnM")], start = c(2017,07,01),end = c(2018,06,25),frequency = 360)


#X1 <- ts(X[,c("TRF.IN.ECOMM","TRF.IN.BnM")])

Acf(X1)

#BnM <- ts(X[,"TRF.IN.BnM"])
#BnM <- ts(X[,c("TRF.IN.BnM")])

#Ecom <- ts(X[,c("TRF.IN.ECOMM")])
#Acf(Ecom)
#Ecom <- ts(X[,c("TRF.IN.ECOMM")],frequency = 7)
#Acf(Ecom)
Ccf(Ecom,BnM)
BnM <- ts(X[,c("TRF.IN.BnM")], start = 1, end = 365, frequency = 1)
Ccf(Ecom,BnM)
Acf(Ecom)

Acf(Ecom)
plot(Ecom)
stl(Ecom)
stl(Ecom, s.window = 7)
plot(stl(Ecom, s.window = 7)
)
decompose((Ecom, type= c('multimplicative')))
decompose(Ecom, type = c("additive", "multiplicative"), filter = NULL)
ecomd <-decompose(Ecom, type = c("additive", "multiplicative"), filter = NULL)
plot(Ecomd)
ecomd <-decompose(Ecom, type = c("additive", "multiplicative"), filter = NULL)
plot(Ecomd)
plot(ecomd)
ecomd <-decompose(Ecom, type = c("multiplicative"), filter = NULL)
plot(ecomd)
ecomd <-decompose(Ecom, type = c("additive"), filter = NULL)
plot(ecomd)
plot(stl(Ecom, s.window = "periodic"))
plot(stl(BnM, s.window = "periodic"))
ecomd <-stl(Ecom, s.window = "periodic") #decompose(Ecom, type = c("additive"), filter = NULL)
plot(ecomd)
BnMd <- stl(BnM, s.window = "periodic")
BnMd
ecomd[,"seasonal"]
View(BnMd)
ecomd$time.series[,"seasonal"]
EcomDseason <- Ecom- ecomd$time.series[,"seasonal"]
plot(EcomDseason)
BnMDseason <- BnM - BnMd$time.series[,"seasonal"]
plot(BnMDseason)
par(new=TRUE)
plot(EcomDseason, col='blue')
Acf(BnMDseason)
Acf(EcomDseason)
Ccf(BnMDseason,EcomDseason)
Ccf(BnMd$time.series[,"seasonal"],ecomd$time.series[,"seasonal"])
Ccf(BnMd$time.series[,"seasonal"],ecomd$time.series[,"seasonal"], xlim = range(-7,7))
Ccf(BnMd$time.series[,"seasonal"],ecomd$time.series[,"seasonal"], ylim = range(-1,1), xlim = range(-7,7))
Store <-Ccf(BnMd$time.series[,"seasonal"],ecomd$time.series[,"seasonal"], ylim = range(-1,1), xlim = range(-7,7))
View(Store)
View(Store)
Store[["acf"]]
X <- read.csv("TrafficData.csv")
head(x,10)
head(X,10)
X1 <- ts(X[,c("TRF.IN.ECOMM","TRF.IN.BnM","TRF.IN.ECOMM.Diff","TRF.IN.BnM.Diff")])
Ecom <- ts(X[,c("TRF.IN.ECOMM")],frequency = 7)
EcomDiff <- ts(X[,c("TRF.IN.ECOMM.Diff")],frequency = 7)
Acf(Ecom)
Acf(EcomDiff)
plot(EcomDiff)
stl(EcomDiff, s.window = "periodic")
plot(stl(EcomDiff, s.window = "periodic"))
Acf(EcomDiff)
remove(Store,EcomDseason,ecomd,BnMd,BnMDseason)
plot(EcomDiff)
remove(X1)
BnM <- ts(X[,c("TRF.IN.BnM")],frequency = 7)
BnMDiff <- ts(X[,c("TRF.IN.BnM.Diff")],frequency = 7)
Acf(BnM)
Acf(BnMDiff)


adf.test(BnMDiff)
adf.test(Ecom)
adf.test(BnM)
Acf(Ecom)


install.packages("plotly")
install.packages("forecast")
Acf(Ecom)



Acf(Ecom)
Acf(EcomDiff)
plot(EcomDiff)
BnM <- ts(X[,c("TRF.IN.BnM")],frequency = 7)
BnMDiff <- ts(X[,c("TRF.IN.BnM.Diff")],frequency = 7)
Acf(BnM)
Acf(BnMDiff)
adf.test(BnMDiff, lag
Ccf(Ecom, BnM, lag.max = 7, type = c("correlation","covariance", plot = TRUE))
Ccf(?)
help(Ccf)
Ccf(Ecom, BnM, lag.max = 7, type = c("correlation","covariance", plot = TRUE), na.action = na.contiguous)
Ccf(Ecom, lag.max = 7, type = c("correlation","covariance", plot = TRUE), na.action = na.contiguous)
Ccf(Ecom, BnM, lag.max = 7, type = c("correlation","covariance"), plot = TRUE, na.action = na.contiguous)
Ccf(EcomDiff, BnMDiff, lag.max = 7, type = c("correlation","covariance"), plot = TRUE, na.action = na.contiguous)
Ccf(BnMDiff, EcomDiff, lag.max = 7, type = c("correlation","covariance"), plot = TRUE, na.action = na.contiguous)
plot(BnMDiff)
plot(EcomDiff)
plot(BnM)
plot(Ecom)
