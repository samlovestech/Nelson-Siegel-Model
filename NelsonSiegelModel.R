# data processing
library(xts)
library(plotly)
library(YieldCurve)
yield.data <- read.csv("/Users/xxxx/yield.csv", header = T)
init <- as.matrix(yield.data[,c(2:4)])
init.rates <- matrix(as.numeric(init), ncol = 3)/100
rest <- as.matrix(yield.data[,c(5:10)])
rest.rates <- matrix(as.numeric(rest), ncol = 6)/100
maturities <- c(0.25, 0.5, 1, 2, 3, 5, 7, 10, 20)

# function to calculate zero rate for maturities[3]
zero.rate.cal <- function(yields, maturities) {
  # maturities includes 3 elements...
  coupon.rate <- yields[,3]/2
  y.rate1 <- yields[,1]
  y.rate2 <- yields[,2]
  y.rate3 <- (y.rate2 - y.rate1)*(maturities[3] - maturities[2])/(maturities[2] - maturities[1]) + y.rate2
  #known.expression <- coupon.rate*exp(-y.rate1*maturities[1]) + coupon.rate*exp(-y.rate2*maturities[2]) + coupon.rate*exp(-y.rate3*(maturities[2] + maturities[3])/2)
  known.expression <- coupon.rate*exp(-y.rate1*0.5) + coupon.rate*exp(-y.rate2*1.0) + coupon.rate*exp(-y.rate3*1.5)
  # why 1 + coupon.rate
  zero.rate <- -log((1 - known.expression)/(1 + coupon.rate))/2
  return(zero.rate)
}

# testing for only 1 loop
#test <- as.matrix(yield.data[,c(3:5)])
#test.rates <- matrix(as.numeric(test), ncol = 3)/100
#test <- zero.rate.cal(test.rates, c(0.5,1.0,2.0))

# get the result
rows <- dim(init.rates)[1]
rest.cols <- dim(rest.rates)[2]
result <- matrix(NA, nrow = rows, ncol = (rest.cols + 1))
# make a iterate rates always with 3 columns, and for each loop, replace the last column by next col in rest.rates
iter.rates <- init.rates
for(i in 1:rest.cols) {
  zero.rate <- zero.rate.cal(iter.rates, maturities[i:(i + 2)])
  result[,i] <- zero.rate
  # refresh iter.rates
  iter.rates <- cbind(iter.rates[,2], zero.rate, rest.rates[,i])
}
result[,(rest.cols + 1)] <- zero.rate.cal(iter.rates, maturities[(rest.cols + 1):(rest.cols + 3)])
#combine the first two column yield rates into result. this is the final result
rates <- cbind(init.rates[,1], init.rates[,2], result)


# fitting plot
y <- rates[1,]
x <- maturities
plot(x,y,main = "zero rate curve")
# maximum the fomula, get beta0 beta1, beta2 and lamda
ns.fit <- Nelson.Siegel(y,x)
d <- as.Date(1:nrow(ns.fit))
ns.xts <- xts(ns.fit, order.by = d)
# plot the fitted line....
fit.x <- seq(0,20,0.01)
fit.y <- NSrates(ns.xts,fit.x)
lines(fit.x, fit.y, col = "red")



## get beta0, beta1, beta2
beta_0 <- rep(NA, rest.cols)
beta_1 <- rep(NA, rest.cols)
beta_2 <- rep(NA, rest.cols)
lambda <- rep(NA, rest.cols)
for(d in 1:rows) {
  y <- rates[d,]
  x <- maturities
  ns.fit <- Nelson.Siegel(y,x)
  beta_0[d] <- ns.fit[1]
  beta_1[d] <- ns.fit[2]
  beta_2[d] <- ns.fit[3]
  lambda[d] <- ns.fit[4]
}

# plot beta0, beta1, beta2 VS days
# x00 <- seq(1,length(lambda),1)
# plot(x00, lambda, type='l',ylim=c(-0.1,1), col = "black",xlab = "Days", ylab = "lambda")
# par(new=TRUE)
x0 <- seq(1,length(beta_0),1)
plot(x0, beta_0, type='l',ylim=c(-0.1,0.15),col = "blue", main = "Beta VS days",xlab = "Days", ylab = "Beta")
par(new=TRUE)

x1 <- seq(1,length(beta_1),1)
plot(x1, beta_1, type='l',ylim=c(-0.1,0.15),col='red',xlab = "Days", ylab = "Beta")
par(new=TRUE)

x2 <- seq(1,length(beta_2),1)
plot(x2, beta_2, type='l',ylim=c(-0.1,0.15),col='green',xlab = "Days", ylab = "Beta")

legend("topleft",legend=c("beta0","beta1","beta2"),
       col=c(1,2,3),lty=1)

beta2.max <- which.max(beta_2)
beta2.lambda <- lambda[beta2.max]

# legend("topleft",legend=c("lambda", "beta0","beta1","beta2"),
#        col=c(1,2,3,4),lty=1)

p0=as.matrix(rates)
Yield=colnames(rates)
dates=rownames(rates)
#3D plot on yield curve
plot_ly(z = ~p0,x=Yield,y=dates) %>% add_surface()


