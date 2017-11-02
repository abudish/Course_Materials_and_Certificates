library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data("state")

# 1 #
str(statesMap)
length(table(statesMap$group)) #1.1 63

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")
        #1.2 color of outline - color attribute in geom_polygon()



# 2 #
polling <- read.csv('PollingImputed.csv')
Train <- subset(polling, Year == 2004 | Year == 2008)
Test <- subset(polling, Year == 2012)
mod2 <- glm(Republican ~ SurveyUSA + DiffCount, data=Train, family="binomial")
TestPrediction <- predict(mod2, newdata=Test, type="response")
TestPredictionBinary <- as.numeric(TestPrediction > 0.5)
predictionDataFrame <- data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(TestPredictionBinary) #2.1  22
mean(TestPrediction)#2.1 0.4852626

predictionDataFrame$region <- tolower(predictionDataFrame$Test.State)
predictionMap <- merge(statesMap, predictionDataFrame, by = "region")
predictionMap <- predictionMap[order(predictionMap$order), ]
dim(predictionMap)#2.2 15034
dim(statesMap)#2.2 15537

# Number of observations changed - Why?
#2.3 Because we only make predictions for 45 states, we no longer have observations for some of the states.
#    These observations were removed in the merging process.

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
# The states appear light blue and dark blue in this map. Which color represents a Republican prediction?
#2.4 Light Blue

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
        geom_polygon(color = "black") + 
        scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),
                            labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
        geom_polygon(color = "black") + 
        scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),
                            labels = c("Democrat", "Republican"), name = "Prediction 2012")
#2.5 The two maps look very similar. This is because most of our predicted probabilities are close to 0 or close to 1


# 3 #
#3.1 We incorrectly predicted this state by predicting that it would be won by the Republican party.

#3.2 What was our predicted probability for the state of Florida?
predictionMap[predictionMap$Test.State == 'Florida', ]# 0.9640395
#    What does this imply?
#    Our prediction model did not do a very good job of correctly predicting the state of Florida, 
#    and we were very confident in our incorrect prediction.


# Problem 4 - Parameter Settings #

# Plot 1:
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
        geom_polygon(color = "black", linetype = 3) + 
        scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),
                            labels = c("Democrat", "Republican"), name = "Prediction 2012")

# Plot 2:
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
        geom_polygon(color = "black", size=3) + 
        scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),
                            labels = c("Democrat", "Republican"), name = "Prediction 2012")

# Plot 3:
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
        geom_polygon(color = "black", alpha=0.3) + 
        scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),
                            labels = c("Democrat", "Republican"), name = "Prediction 2012")
# 4.1 linetype and size
# 4.2 alpha

