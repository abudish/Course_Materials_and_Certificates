install.packages("ISLR")
library(ISLR)
library(ggplot2)
library(caret)

data("Wage")
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dim(training); dim(testing)

# Featur plot (caret)
featurePlot(x=training[, c("age", "education", "jobclass")],
            y = training$wage,
            plot="pairs")
# qplot
qplot(age, wage, data=training)
qplot(education, wage, data=training)

# qplot with color
qplot(age, wage, colour = jobclass, data=training)

# add regression smoothers
qq <- qplot(age, wage, colour=education, data=training)
qq + geom_smooth(method="lm", formula=y~x)

# cut2, making factors (Hmisc package)
library(Hmisc)
cutWage <- cut2(training$wage, g=3)
table(cutWage)

# boxplots with cut2
p1 <- qplot(cutWage, age, data=training, fill=cutWage,
           geom=c("boxplot"))
p1

# boxplots with points overlayed
p2 <- qplot(cutWage, age, data=training, fill=cutWage,
            geom=c("boxplot", "jitter"))
library(gridExtra)
grid.arrange(p1, p2, ncol=2)

# Tables
t1 <- table(cutWage, training$jobclass)
t1
# Proportion table
prop.table(t1, 1)


# Density plots
qplot(wage, colour=education, data=training, geom='density')

