# Ozone data
install.packages("ElemStatLearn")
library(ElemStatLearn)
data(ozone, package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

# Bagged loess - example - predicting temperature by ozone variable
# loess fits smooth linear curve, similar to splines

# create empty matrix
ll <- matrix(NA,nrow=10,ncol=155)
# iterate 10 times
for(i in 1:10){
        # create sample from data with replacement
        ss <- sample(1:dim(ozone)[1],replace=T)
        # draw sample from the dataa and reorder rows based on ozone
        ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
        # fit loess function through data (similar to spline)
        loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)
        # prediction from loess curve for the same values each time
        ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}

# Plotting the values + models + average model
plot(ozone$ozone, ozone$temperature, pch=19, cex=0.5)
for(i in 1:10){lines(1:155, ll[i, ], col='grey', lwd=2)}
lines(1:155, apply(ll,2,mean), col='red', lwd=2)


# Creating your own bagging function
install.packages("party")
library(party)
predictors <- data.frame(ozone=ozone$ozone)
temperature <- ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))
# plot data points
plot(ozone$ozone,temperature,col='lightgrey',pch=19)
# plot the first fit
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
# plot the aggregated predictions
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")

# Parts of bagging function
ctreeBag$fit
ctreeBag$pred
ctreeBag$aggregate
