
  
 



  

library(dplyr)
library(scatterplot3d)
library(psych)
library(ggplot2)
library(car)
library(gclus)
library(GGally)
library(fields)
library(corrplot)

deathrate <- read.table("x28_NoHeader", header = FALSE)

colnames(deathrate) <- c("Index",
                         "A1",
                         "A2",
                         "A3",
                         "A4",
                         "A5",
                         "A6",
                         "A7",
                         "A8",
                         "A9",
                         "A10",
                         "A11",
                         "A12",
                         "A13",                         
                         "A14",
                         "A15",
                         "B")

# Index
# A1 average annual precipitation in inches
# A2 average January temperature in degrees Fahrenheit
# A3 average July temperature in degrees Fahrenheit
# A4 percent of 1960 SMSA population 65 years old or older
## A5 household size, 1960
## A6 schooling for persons over 22
# A7 household with full kitchens
## A8 population per square mile in urbanized areas
## A9 percent nonwhite population
# A10 percent office workers
## A11 poor families (annual income under $3000)
# A12 relative pollution potential of hydrocarbons
# A13 relative pollution potential of oxides of Nitrogen
# A14 relative pollution of Sulfur Dioxide
# A15 percent relative humidity, annual average at 1pm.
# B death rate



#Here we will create a subset of the original dataframe with the desired features.
# We are starting with "A5", "A6", "A9", "A11" as independent variables, and B as the dependent variable.

myvars <- c("A5", "A6", "A9", "A11", "B")
deathrate_1 <- deathrate[myvars]



#Making a Basic scatter plot matrix

scatterplotMatrix(~B+A5+A6+A9+A11,data=deathrate_1, smooth = FALSE, ellipse=
                    FALSE, main="Simple Scatterplot Matrix")


#Plotting correlations between variables.

dta.r <- abs(cor(deathrate_1)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(deathrate_1, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

dta.r










# 3D Scatterplot with Coloring and Vertical Lines
# and Regression Plane 

attach(deathrate_1) 
s3d <-scatterplot3d(A6,A9,B, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
fit <- lm(B ~ A6+ A9, data = deathrate_1) 
s3d$plane3d(fit)

A6A9 <- ggplot(deathrate_1, aes(x = A6, y = A9)) + geom_point() + geom_point(aes(color = B)) 
A6A9

attach(deathrate_1) 
s3d <-scatterplot3d(A5,A6,B, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
fit <- lm(B ~ A5+ A6, data = deathrate_1) 
s3d$plane3d(fit)

A5A6 <- ggplot(deathrate_1, aes(x = A5, y = A6)) + geom_point() + geom_point(aes(color = B)) 
A5A6

attach(deathrate_1) 
s3d <-scatterplot3d(A6,A11,B, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
fit <- lm(B ~ A6+ A11, data = deathrate_1) 
s3d$plane3d(fit)

A6A11 <- ggplot(deathrate_1, aes(x = A6, y = A11)) + geom_point() + geom_point(aes(color = B)) 
A6A11


#Finding correlation coeff r with p-values
#install.packages("psych")

corr.test(deathrate_1, y = NULL, use =
            "pairwise",method="pearson",adjust="holm",alpha=.05)


ggcorr(deathrate_1, palette = "RdYlGn", name = "rho", 
       label = FALSE, label_color = "black")

# Correlation plot
ggcorr(deathrate_1, palette = "RdBu", label = TRUE)



ggpairs(deathrate_1, columns = 1:ncol(deathrate_1), title = "",  
        axisLabels = "show", columnLabels = colnames(deathrate_1[, 1:5]))

## A5 household size, 1960
## A6 schooling for persons over 22
# A7 household with full kitchens
## A8 population per square mile in urbanized areas
## A9 percent nonwhite population
# A10 percent office workers
## A11 poor families (annual income under $3000)

# The Model we will use is 


ggpairs(deathrate_1, columns = 2:ncol(deathrate_1), title = "",  
        axisLabels = "show", columnLabels = colnames(deathrate_1[, 2:5]))


** Y^i = A5X1 + A6X2 + A9X3 + A11X4 + error^i, error is normal rv with mean=o and constant varaince for all i. **
  
  
fit1 <- lm(formula = deathrate_1$B ~ deathrate$A5 + deathrate_1$A6 + deathrate_1$A9 + deathrate_1$A11)

fit2 <- lm(formula = deathrate_1$B ~deathrate_1$A6 + deathrate_1$A9 + deathrate_1$A11)

summary(fit1)
anova(fit1, fit2)
summary(fit2)


fit1 <- lm(formula = deathrate_1$B ~ deathrate_1$A5 + deathrate_1$A6 + deathrate_1$A9 + deathrate_1$A11)

fit2 <- lm(formula = deathrate_1$B ~ deathrate_1$A6 + deathrate_1$A9 + deathrate_1$A11)

summary(fit1)
anova(fit1, fit2)




shapiro.test(fit2$residuals)



layout(matrix(c(1,2,3,4),2,2)) 
plot(fit2)


hist(fit2$residuals)





"A6"
summary(deathrate_1$A6)
"A9"
summary(deathrate_1$A9)
"A11"
summary(deathrate_1$A11)




results = lm(B ~ A6 + A9 + A11, data=deathrate_1)

"Prediction: If the minimum value for all the predictors occured"
newdat1 = data.frame(A6 = 9, A9=0.8, A11 = 9.4)
predict(results, newdata = newdat1, interval="confidence")

"Prediction: If the mean of all the predictors occured"
newdat2 = data.frame(A6 = 10.97, A9=11.87, A11 = 14.37)
predict(results, newdata = newdat2, interval="confidence")

"Prediction: If the max of all the predictors occured"
newdat3 = data.frame(A6 = 12.3, A9=38.50, A11 = 26.40)
predict(results, newdata = newdat3, interval="confidence")

"Prediction: max min min"
newdat4 = data.frame(A6 = 12.3, A9=0.8, A11 = 9.4)
predict(results, newdata = newdat4, interval="confidence")
