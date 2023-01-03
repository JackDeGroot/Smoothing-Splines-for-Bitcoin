
library(readxl)
Bitcoin_One_Hour_Data <- read_excel("C:/Users/degro/Downloads/Bitcoin One Hour Data.xlsx", 
                                    col_types = c("date", "text", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric"))
BC.One.Hour <- (Bitcoin_One_Hour_Data)
BC.One.Hour

plot(BC.One.Hour$date, BC.One.Hour$close)

Week.of.Feb_7th <- BC.One.Hour[BC.One.Hour$date >= "2018-02-04 19:00:00" & BC.One.Hour$date <= "2018-02-07 18:00:00", ]

Week.of.Feb7 <- read_excel("C:/Users/degro/Downloads/week7.xlsx", 
                           col_types = c("date", "text", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric"))


plot(Week.of.Feb7$DayNumber, Week.of.Feb7$close, ylab = "Price", xlab = "Date", main = "Bitcoin Price Chart")



#####################################################
##################### GCV CV. #######################
#####################################################

y<- Week.of.Feb7$close
x<-Week.of.Feb7$DayNumber
plot(x,y)


lam <- seq(0.001, 10, 0.005)
nlam<-length(lam)

K=(72-0)/4  ##K=18
knots<-quantile(x, 1:K/(K+1))
length(knots)

#' I determined the number of knots (in this case Knots=18) by our Semi-nonparametric book
#' where in section 5.5.3 where the author states "A reasonable default is to 
#' choose the knots to ensure that there are a fixed number of unique 
#' observations, say 4–5, between each knot. For large data sets this can
#' lead to an excessive number of knots, so a maximum number of allowable knots
#' (say, 20–40 total) is recommended.



n<-length(x)
p<-length(knots)+2
X<-matrix(1, nrow=n, ncol=p)
X[,2]<-x
for (j in 1:(p-2)){
  X[,(j+2)]<-(x-knots[j])*(x>knots[j])	
}

GCV = vector("numeric", nlam)
CV = vector("numeric", nlam)

for (k in 1:nlam){
  D<-diag(c(0,0, rep(1, p-2)))
  yhat<-X%*%solve(t(X)%*%X+lam[k]^2*D)%*%t(X)%*%y
  L<-diag(X%*%solve(t(X)%*%X+lam[k]^2*D)%*%t(X))
  v<-sum(L)
  GCV[k]<-sum((y-yhat)^2)*n/((n-v)^2)
  CV[k]<-sum((y-yhat)^2/(1-L)^2)/n
}

plot(log(lam),GCV , type='l', lty=2, ylab= "CV(lambda) and GCV(lambda)", xlab= "log(lambda)", xlim = c(-6,2.5))

lines(log(lam), CV, type = 'l', lty = 1 )

legend(x = "topleft", legend=c("CV", "GCV"), 
       lty = c(1,2)
)

plot(log(lam), GCV, type="l", lty=1, col="blue")
lamgcv<-log(lam[GCV==min(GCV)])
lamgcv
abline(v=lamgcv, lty=2, col=2)

#' This is saying that our optimal log(lamda) is -0.4292456 which converts to lambda equal to lambda = 0.651
#' We will now use this lambda in our models



#####################################################
## Penalized Linear Spline Regression Based on GCV ##
#####################################################

#' From GCV Curve we know that our model is optimized when lambda = 0.651

lam<-0.651
D<-diag(c(0,0, rep(1, p-2)))
yhat.gcv<-X%*%solve(t(X)%*%X+lam^2*D)%*%t(X)%*%y
plot(x, y)
abline(v=knots, col="red", lty=6)
lines(x, yhat.gcv, col="blue",lty=1,  lwd=2)



#####################################################
############ Linear Spline Regression  ##############
#####################################################

K=(72-0)/4  ##K=18
knots<-quantile(x, 1:K/(K+1))
length(knots)

n<-length(x)
p<-length(knots)+2   #### p=20, K=18
X<-matrix(1, nrow=n, ncol=p)
X[,2]<-x
for (j in 1:(p-2)){
  X[,(j+2)]<-(x-knots[j])*(x>knots[j])	
}
fit.linS<-lm(y~X-1)
betas<-as.vector(coef(fit.linS))
fhat1S<-fit.linS$fitted
plot(x,y)
abline(v=knots, col="red", lty=4)
lines(x, fhat1S, col="blue", lwd=2)  



#####################################################
####### Spline Estimate parameter Set UP  ###########
#####################################################

n<-length(x)
knots<-quantile(x, 1:K/(K+1))

p<-length(knots)+4  
X<-matrix(1, nrow=n, ncol=p)
X[,2]<-x
X[,3]<-x^2
X[,4]<-x^3
for (j in 1:(p-4)){
  X[,(j+4)]<-((x-knots[j])^3)*(x>knots[j])	
}



#####################################################
########### Cubic Spline Estimate Code  #############
#####################################################

fit.cub<-lm(y~X-1)
fhat.cubic<-fit.cub$fitted
plot(x, y)
abline(v=knots, col="red", lty=4)
lines(x, fhat.cubic, col="green", lwd=2)



#####################################################
#### B-spline corresponding to Cubic Spline Code ####
#####################################################

library(splines)
XB<-bs(x, degree=3, knots=c(seq(0, 72, by=5)))
fit.cubB<-lm(y~XB)
fhat.cubic.B.Spline<-fit.cubB$fitted
plot(x, y)
abline(v=knots, col="red")
lines(x, fhat.cubic.B.Spline, col="purple", lwd=3)



#####################################################
######### Natural Cubic Spline Code Below  ##########
#####################################################

XN<-ns(x, knots=c(quantile(x, 1:K/(K+1))))
fitN<-lm(y~XN)
fhat.Natural.Cubic<-fitN$fitted
plot(x,y)
abline(v=knots, col="red", lty=10)
lines(x, fhat.Natural.Cubic, col="blue", lty=1, lwd=2)



#####################################################
###### Comparison of Different Splines Below  #######
#####################################################

par(mfrow= c(2,3))

#' Penalized Linear Spline Regression Based on GCV
plot(x, y, ylab = "Price", xlab = "Observation Number", main = "GCV based Penalized Linear Spline")
abline(v=knots, col="red", lty=6)
lines(x, yhat.gcv, col="blue",lty=1,  lwd=2)

#' Linear Spline Regression Estimate Plot
plot(x,y,  ylab = "Price", xlab = "Observation Number", main = "Linear Spline")
abline(v=knots, col="red", lty=4)
lines(x, fhat1S, col="blue", lwd=2) 

#' Cubic Spline Estimate Plot
plot(x, y, ylab = "Price", xlab = "Observation Number", main = "Cupic Spline")
abline(v=knots, col="red", lty=4)
lines(x, fhat.cubic, col="green", lwd=2)

#' B Spline B-spline corresponding to cubic spline Plot
plot(x, y, ylab = "Price", xlab = "Observation Number", main = "B-Spline to Cubic")
abline(v=knots, col="red", lty=4)
lines(x, fhat.cubic.B.Spline, col="red", lty=3, lwd=3)

#' Natural Cubic Spline Plot
plot(x, y, ylab = "Price", xlab = "Observation Number", main = "Natural Cubic Spline")
abline(v=knots, col="red", lty=4)
lines(x, fhat.Natural.Cubic, col="blue", lty=1, lwd=2)


par(mfrow= c(1,1))



#####################################################
################### Analysis  #######################
#####################################################

#'Moving-Up-Stream charting type I: E1 is the minimum, E1 < E3 < E5, E2 < E4
#' V1 > V3, V2 < V4

#' We check for volume

V1 <- Week.of.Feb7[42,]["Volume USD"]
V3 <- Week.of.Feb7[31,]["Volume USD"]

V2 <- Week.of.Feb7[35,]["Volume USD"]
V4 <- Week.of.Feb7[28,]["Volume USD"]

V1 > V3  
V2 < V4

#' We confirm that conditions are met and we are observing a moving up stream pattern 1 like in the article

#' We Observe that we want knot 8 which is day 31, row 42 gives us day 31.

Week.of.Feb7[42,]

#' Observing row 42 we see that we have a close price On Feburary 6 2018 for 6:00 am at $6,225

Intial.Value = Week.of.Feb7[42,]["close"]

#' We will exit the trade after the next maximum of E5 which is knot 16
#' We observe that knot 18 is day 61 which gives us row 12

Current.Value = Week.of.Feb7[12,]["close"]

#' Observing row 12 we see that we have a close price on February 7 2018 for 12 in the afternoon at $8377. 

(Return = abs((Current.Value-Intial.Value)/Intial.Value))

#' 34.57% ROI or a profit of $2,152











#' Case Two



plot(BC.One.Hour$date, BC.One.Hour$close)


Buy1 <- BC.One.Hour[BC.One.Hour$date >= "2021-05-01 19:00:00" & BC.One.Hour$date <= "2022-01-05 18:00:00", ]


plot(Buy1$date, Buy1$close)

#' Lets get specific

Buy2 <- BC.One.Hour[BC.One.Hour$date >= "2021-05-01 19:00:00" & BC.One.Hour$date <= "2021-07-05 18:00:00", ]


plot(Buy2$date, Buy2$close)

Buy <- read_excel("C:/Users/degro/Downloads/Buydata.xlsx", 
                  col_types = c("date", "text", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))


plot(Buy$date, Buy$close)

plot(Buy$DayNumber, Buy$close, ylab = "Price", xlab = "Date", main = "Bitcoin Price Chart")


#####################################################
##################### GCV CV. #######################
#####################################################

y<- Buy$close
x<-Buy$DayNumber
plot(x,y)

lam <- seq(0.001, 10, 0.005)
nlam<-length(lam)

K=(1560-0)/39  ##K=40
knots<-quantile(x, 1:K/(K+1))
length(knots)

#' I determined the number of knots (in this case Knots=40) by our Semi-nonparametric book
#' where in section 5.5.3 where the author states "A reasonable default is to 
#' choose the knots to ensure that there are a fixed number of unique 
#' observations, say 4–5, between each knot. For large data sets this can
#' lead to an excessive number of knots, so a maximum number of allowable knots
#' (say, 20–40 total) is recommended.



n<-length(x)
p<-length(knots)+2
X<-matrix(1, nrow=n, ncol=p)
X[,2]<-x
for (j in 1:(p-2)){
  X[,(j+2)]<-(x-knots[j])*(x>knots[j])	
}

GCV = vector("numeric", nlam)
CV = vector("numeric", nlam)

for (k in 1:nlam){
  D<-diag(c(0,0, rep(1, p-2)))
  yhat<-X%*%solve(t(X)%*%X+lam[k]^2*D)%*%t(X)%*%y
  L<-diag(X%*%solve(t(X)%*%X+lam[k]^2*D)%*%t(X))
  v<-sum(L)
  GCV[k]<-sum((y-yhat)^2)*n/((n-v)^2)
  CV[k]<-sum((y-yhat)^2/(1-L)^2)/n
}

plot(log(lam),GCV , type='l', lty=2, ylab= "CV(lambda) and GCV(lambda)", xlab= "log(lambda)", xlim = c(-6,3))

lines(log(lam), CV, type = 'l', lty = 1 )

legend(x = "topleft", legend=c("CV", "GCV"), 
       lty = c(1,2)
)

plot(log(lam), GCV, type="l", lty=1, col="blue")
lamgcv<-log(lam[GCV==min(GCV)])
lamgcv
abline(v=lamgcv, lty=2, col=2)

#' This is saying that our optimal log(lamda) is 2.044591 which converts to lambda equal to lambda = 7.72599
#' We will now use this lambda in our models



#####################################################
## Penalized Linear Spline Regression Based on GCV ##
#####################################################

#' From GCV Curve we know that our model is optimized when lambda = 7.72599

lam<-7.72599
D<-diag(c(0,0, rep(1, p-2)))
yhat.gcv.Day<-X%*%solve(t(X)%*%X+lam^2*D)%*%t(X)%*%y
plot(x, y)
abline(v=knots, col="red", lty=6)
lines(x, yhat.gcv.Day, col="blue",lty=1,  lwd=2)


#####################################################
############ Linear Spline Regression  ##############
#####################################################

K=(1560-0)/49  ##K=40
knots<-quantile(x, 1:K/(K+1))
length(knots)

n<-length(x)
p<-length(knots)+2   #### p=42, K=40
X<-matrix(1, nrow=n, ncol=p)
X[,2]<-x
for (j in 1:(p-2)){
  X[,(j+2)]<-(x-knots[j])*(x>knots[j])	
}
fit.linS<-lm(y~X-1)
betas<-as.vector(coef(fit.linS))
fhat1S.Day<-fit.linS$fitted
plot(x,y)
abline(v=knots, col="red", lty=4)
lines(x, fhat1S.Day, col="green", lwd=2)  



#####################################################
####### Spline Estimate parameter Set UP  ###########
#####################################################

n<-length(x)
knots<-quantile(x, 1:K/(K+1))

p<-length(knots)+4  
X<-matrix(1, nrow=n, ncol=p)
X[,2]<-x
X[,3]<-x^2
X[,4]<-x^3
for (j in 1:(p-4)){
  X[,(j+4)]<-((x-knots[j])^3)*(x>knots[j])	
}



#####################################################
########### Cubic Spline Estimate Code  #############
#####################################################

fit.cub<-lm(y~X-1)
fhat.cubic.Day<-fit.cub$fitted
plot(x, y)
abline(v=knots, col="red", lty=4)
lines(x, fhat.cubic.Day, col="green", lwd=2)



#####################################################
#### B-spline corresponding to Cubic Spline Code ####
#####################################################

library(splines)
XB<-bs(x, degree=3, knots=c(seq(0, 72, by=5)))
fit.cubB<-lm(y~XB)
fhat.cubic.B.Spline.Day<-fit.cubB$fitted
abline(v=knots, col="red")
lines(x, fhat.cubic.B.Spline.Day, col="red", lty=3, lwd=3)



#####################################################
######### Natural Cubic Spline Code Below  ##########
#####################################################

XN<-ns(x, knots=c(quantile(x, 1:K/(K+1))))
fitN<-lm(y~XN)
fhat.Natural.Cubic.Day<-fitN$fitted
abline(v=knots, col="red", lty=10)
lines(x, fhat.Natural.Cubic.Day, col="blue", lty=1, lwd=2)


#####################################################
###### Comparison of Different Splines Below  #######
#####################################################

par(mfrow= c(2,3))

#' Penalized Linear Spline Regression Based on GCV
plot(x, y, ylab = "Price", xlab = "Observation Number", main = "GCV based Penalized Linear Spline")
abline(v=knots, col="red", lty=6)
lines(x, yhat.gcv.Day, col="green",lty=1,  lwd=2)

#' Linear Spline Regression Estimate Plot
plot(x,y,  ylab = "Price", xlab = "Observation Number", main = "Linear Spline")
abline(v=knots, col="red", lty=4)
lines(x, fhat1S.Day, col="blue", lwd=2) 

#' Cubic Spline Estimate Plot
plot(x, y, ylab = "Price", xlab = "Observation Number", main = "Cubic Spline")
abline(v=knots, col="red", lty=4)
lines(x, fhat.cubic.Day, col="green", lwd=2)

#' B Spline B-spline corresponding to cubic spline Plot
plot(x, y, ylab = "Price", xlab = "Observation Number", main = "B-Spline to Cubic", ylim = c(30000,63000))
abline(v=knots, col="red", lty=4)
lines(x, fhat.cubic.B.Spline.Day, col="green", lwd=3)

#' Natural Cubic Spline Plot
plot(x, y, ylab = "Price", xlab = "Observation Number", main = "Natural Cubic Spline")
abline(v=knots, col="red", lty=4)
lines(x, fhat.Natural.Cubic.Day, col="green", lty=1, lwd=2)


par(mfrow= c(1,1))

Buy[685,]['DayNumber']
Buy[685,]

#' This is the day that we would have bought Bitcoin which was at the 23th knot which 
#' was day 876 or June 7, 2021 at 10:00am and it was at a closing price of $36,531

Buy[646,]['DayNumber']
Buy[646,]

#' We would have exited the trade at the next knot which was the 24th knot which
#' was day 915 or June 9th at 1:00am and it was a closing price of 32,526
#' 

CV=32526
IC=36531
(Return = abs((CV-IC)/IC))

3269*0.0001451


par(mfrow=c(1,2))

#' Natural Cubic Spline Plot
plot(x, y, ylab = "Price", xlab = "Number of Days", main = "Natural Cubic Spline")
abline(v=knots, col="red", lty=4)
lines(x, fhat.Natural.Cubic.Day, col="green", lty=1, lwd=2)

plot(Buy1$date, Buy1$close, type = 'l',  ylab = "Price", xlab = "Number of Days", main = "Bitcoin the Months After")

par(mfrow=c(1,1))
