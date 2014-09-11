# Some R code for running through various aspects of (really simple) linear models.
# Written by Stewart Macdonald <stewart.macdonald@my.jcu.edu.au>
# Useful links:
# http://onlinestatbook.com/2/regression/intro.html
# http://blog.minitab.com/blog/adventures-in-statistics/regression-analysis-how-do-i-interpret-r-squared-and-assess-the-goodness-of-fit
# http://blog.minitab.com/blog/adventures-in-statistics/why-you-need-to-check-your-residual-plots-for-regression-analysis
# http://en.wikipedia.org/wiki/Coefficient_of_determination#Adjusted_R2
# Thanks to Ben Phillips for clarification of a couple of key points.

#STRAIGHT LINES
# This is a simple example where y = x
testX <- 0:4
testY <- testX
plot(testX, testY, pch=16, xlim=c(0,10), ylim=c(0, 10))
lines(testX, testY)

# The y-intercept is the value of y when x=0. In the above case, the intercept is 0.
# We can shift the intercept by adding a constant amount to x. Here, the intercept is now 1:
testY <- testX + 1
plot(testX, testY, pch=16, xlim=c(0,10), ylim=c(0, 10))
lines(testX, testY)

# The above examples have a gradient (slope, or 'b') of 1. For every increase in the x-axis of 1, we get an increase in the y-axis of 1.
# To calculate the slope, you divide the change in y (Δy) by the change in x (Δx).
# Δy/Δx = b
#   1/1 = 1:
(testY[2] - testY[1]) / (testX[2] - testX[1])

# We can make the slope steeper:
testY <- 2 * testX + 1
plot(testX, testY, pch=16, xlim=c(0,10), ylim=c(0, 10))
lines(testX, testY)
# In the above, the gradient is now 2:
(testY[2] - testY[1]) / (testX[2] - testX[1])

# Shallow slope:
testY <- 0.5 * testX + 1
plot(testX, testY, pch=16, xlim=c(0,10), ylim=c(0, 10))
lines(testX, testY)
# The gradient is now 0.5:
(testY[2] - testY[1]) / (testX[2] - testX[1])

# The general equation for a straight line is:
# y = bx + A
# where b is the slope and A is the y-intercept

# Straight lines are important in linear models.




# LINEAR MODELS
# The next examples use the cars dataset. See ?cars for more info.
# "The data give the speed of cars and the distances taken to stop."

# Create a blank plot, sized to fit our data, with appropriate axes.
plot(cars$speed, cars$dist, xlab='Car speed (mph)', ylab='Stopping distance (ft)', main='Car speed vs stopping distance', pch=NA)

# Now add in data points.
points(cars$speed, cars$dist)

# There is a clear positive relationship between car speed and stopping distance (cars travelling faster need more distance in which to stop).
# Create a linear model object (called 'mod' here) that tells us about the relationship between speed (the predictor variable) and stopping distance (the response variable)
mod <- lm(cars$dist ~ cars$speed)

# Side note: See that lm() uses formula notation (R uses a variant of Wilkinson–Rogers notation).
# We say that we're modelling "distance as a function of speed".
# Many other R functions can understand formula notation. Here, we replot the data using a formula (changing the plotting character to a solid circle so you can see that it worked):
points(cars$dist ~ cars$speed, pch=16)

# we can view a summary of this model object
summary(mod)
##################################################################
# Call:
# lm(formula = cars$dist ~ cars$speed)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -29.069  -9.525  -2.272   9.215  43.201 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -17.5791     6.7584  -2.601   0.0123 *  
# cars$speed    3.9324     0.4155   9.464 1.49e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.38 on 48 degrees of freedom
# Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 
# F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12
##################################################################

# In a model with a response variable and a single predictor, we get a y-intercept value and a single slope value. These are the 'Estimates':
#       Intercept (A) = -17.5791
# Slope for speed (b) =   3.9324

# we can use the output of the linear model to plot a straight line (that's the 'linear' part of this) through our data, using the intercept and slope:
abline(a=coef(mod)['(Intercept)'], b=coef(mod)['cars$speed'])

# the abline function is smart enough to extract the slope and intercept from a regression object:
abline(reg=mod, col='red')

# Essentially, a linear model is fitting a 'line of best fit' through your data and then giving you information about that fit.
# The line is trying to approximate your data, enabling you to predict a value of y for any other value of x.
# So if we have an x-value of 10.5, our expected y-value would be:
# y = bx + A
y <- 3.9324 * 10.5 + -17.5791

# For each set of observations, you'll have a real value of y and a 'fitted' value (often denoted as y'). This is the value of y that sits on the line at x.
# We'll examine one data point (point 23, for no particular reason) more closely:
points(cars[23, ], cex=4)

# look at the actual value of y (=dist):
cars[23, ] # speed = 14 and dist = 80

# look at the 'fitted' value of y:
fitted(mod)[23] # 37.47463

# We can assess how close our real value is to the fitted value by subtracting them:
cars$dist[23] - fitted(mod)[23] # 42.52537
# Our real value is 42.5 (feet, in this case) higher than the fitted value. This difference is called the 'residual' (i.e., what value is left over when we've fitted the data).
# Our model object contains these residuals:
residuals(mod)[23] # 42.52537

# We can visualise this difference by drawing a line from the real value to the fitted value:
lines(c(cars$speed[23], cars$speed[23]), c(cars$dist[23], fitted(mod)[23]))

# The regression line we plotted with the abline() function is the line that minimises the sum of these squared residuals.
sum(residuals(mod)^2) # 11353.52

# We can plot the residuals against the fitted values:
plot(fitted(mod), residuals(mod))
abline(h=0)
# They should be evenly distributed around the 0 line, which indicates that the errors of prediction (the residuals) are random.
# Non-random errors might occur when our predictor variables aren't capturing some explanatory power, which is instead 'leaking' into the residuals.

# Replot the speed vs dist data:
plot(cars$speed, cars$dist, xlab='Car speed (mph)', ylab='Stopping distance (ft)', main='Car speed vs stopping distance', pch=16)
abline(reg=mod, col='red')

# We could fit other lines to our data, but all other lines would have higher sums of squares.
# This type of regression is called an 'ordinary least squares' regression, because it's minimising the sum of the squares of the residuals.

# A linear model calculates the slope (b) with the following equation:
# b = r * ( sd(y) / sd(x) )
#  Where:
#   sd(x) is the standard deviation of x
#   sd(y) is the standard deviation of y
#   r is the correlation between x and y
# So:
b <- cor(cars$speed, cars$dist) * ( sd(cars$dist) / sd(cars$speed) )

# We can plot the centroid (the coordinate of the x and y means):
points(mean(cars$speed), mean(cars$dist), pch=3, cex=2)
# The regression line (the line with slope b) will pass through this centroid, so once we've calculated the slope,
# we can determine the y-intercept just by following the line back to x=0.
# Knowing the centroid means we have a known point on our regression line. We can use these known values to rearrange the straight-line equation and solve for A:
# y = bx + A
# A = y - bx

# More generally:
# A = mean(y) - b * mean(x)
A <- mean(cars$dist) - ( b * mean(cars$speed) )

# We can then use the slope and intercept to plot the regression line through our data (with a line width of 4 to overwrite the previous lines):
abline(a=A, b=b, lwd=4, col='red')




# INTERPRETING THE OUTPUT OF A LINEAR MODEL
# Look at a summary of the model object
summary(mod)
##################################################################
# Call:
# lm(formula = cars$dist ~ cars$speed)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -29.069  -9.525  -2.272   9.215  43.201 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -17.5791     6.7584  -2.601   0.0123 *  
# cars$speed    3.9324     0.4155   9.464 1.49e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.38 on 48 degrees of freedom
# Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 
# F-statistic: 89.57 on 1 and 48 DF,  p-value: 1.49e-12
##################################################################

# Let's look at the important lines

##################################################################
# (Intercept) -17.5791     6.7584  -2.601   0.0123 *  
##################################################################
# This just tells us that the intercept is different from zero.  
# Normally this isn't particularly interesting because the null hypothesis,
#	 that the int = 0, is often a silly null.
# Usually we are more interested in whether there is a relationship between our variables
#	this is captured in the slope.

##################################################################
# "# cars$speed    3.9324     0.4155   9.464 1.49e-12 ***"
##################################################################
# This line gives us information about speed as a predictor variable
# The '***' tells us that speed has a statistically significant effect on stopping distance.
# that is, the slope is significantly different from zero (i.e., there is a relationship)
# In this case, our p-value is < 0.001.

##################################################################
# "Residual standard error: 15.38 on 48 degrees of freedom"
##################################################################
# This is the standard error of the estimates. The smaller the error, the more accurate are the predicted values.
# It's built on 48 degrees of freedom, which is nrow(cars) - 2, as we used 2 parameters (slope and intercept) to calculate it.
sqrt( sum(residuals(mod)^2) / (nrow(cars) - 2) )
# -2 because we're calculating from a sample, not the population, and we've already used two parameters (slope and intercept)

##################################################################
# "Multiple R-squared:  0.6511,	Adjusted R-squared:  0.6438 "
##################################################################
# r^2 is a measure of goodness of fit, and is the proportion of the response variable variation that is explained by the model.
# In our simple linear regression, it is:
rsq <- cor(cars$speed, cars$dist)^2 
# r^2 will always increase as you add predictor variables in a multiple regression, but it doesn't mean that your model is getting better.
# Adding more terms might simply start 'explaining' random noise in your data.
# The adjusted r-squared value accounts for the number of terms in the model.
# The adjusted value can be used to compare goodness-of-fit between two models with differing numbers of terms.
# It can be negative, and will always be less than or equal to the standard r^2.
p <- 1 # we have one predictor: speed
rsq.adj <- 1 - (1 - rsq) * ( (nrow(cars) - 1) / (nrow(cars) - p - 1) )


# Multiple regression
# Car speed is obviously not the only factor that will have an effect on stopping distance.
# We can append two other car-based factors to our dataframe.
# A list of somewhat realistic masses that we expect to have an effect on dist:
mass <- c(40, 80, 40, 90, 90, 40, 110, 200, 300, 50, 80, 60, 70, 80, 90, 80, 110, 110, 150, 70, 90, 200, 300, 40, 60, 120, 80, 90, 80, 90, 110, 80, 110, 150, 200, 80, 100, 150, 70, 90, 95, 100, 110, 110, 90, 110, 150, 155, 180, 90)

# These are 50 random ages between 18 and 90, generated with sample(18:90, 50).
# We expect that these will have no effect on stopping distance, as they are random numbers.
# (Note that in the real world, driver age probably does have an effect on stopping distance.)
driverAge <- c(56, 85, 83, 32, 49, 78, 38, 29, 48, 69, 74, 41, 58, 62, 82, 87, 45, 50, 77, 26, 34, 66, 86, 65, 90, 27, 60, 46, 72, 28, 64, 19, 63, 59, 57, 54, 25, 51, 18, 61, 36, 67, 35, 31, 39, 81, 76, 23, 30, 68)

# cbind binds these two extra columns to our existing cars dataframe, and creates a new 'cars2' object
cars2 <- cbind(cars, mass, driverAge)

# Now we can generate some models with differing numbers of terms
mod1 <- lm(dist ~ speed, data=cars2)
mod2 <- lm(dist ~ speed + mass, data=cars2)
mod3 <- lm(dist ~ speed + mass + driverAge, data=cars2)

# Our R-squared value will keep increasing as we add in new predictor variables, but our adjusted R-squared value
# should not increase (or will increase only slightly) when we add in driverAge, because it doesn't add much to the predictive power of our model.
summary(mod1) # Multiple R-squared:  0.6511,  Adjusted R-squared:  0.6438
summary(mod2) # Multiple R-squared:  0.8475,  Adjusted R-squared:  0.841
summary(mod3) # Multiple R-squared:  0.8508,  Adjusted R-squared:  0.841

# The adjusted r-squared of mod3 is the same as that of mod2, which tells us that adding in driverAge did not give us any additional explanatory power.

# Here's part of summary(mod3):
##################################################################
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -39.61776    7.06669  -5.606 1.12e-06 ***
#   speed         3.65025    0.28863  12.647  < 2e-16 ***
#   mass          0.20664    0.02652   7.792 6.00e-10 ***
#   driverAge     0.07378    0.07292   1.012    0.317    
##################################################################
# This tells us that both speed and mass have a statistically significant effect on stopping distance,
# but that driverAge (with a p-value of 0.317) does not (which is expected, given that it's a random number).

# Also note that the slope for speed changes when we include mass in the model.

# Up till now we've been visualising (graphing) a straight-line relationship with only one predictor, but what do we do with more than one predictor?
# With two predictor variables (plus the response variable, for a total of three variables), we can imagine a tilted plane that predicts our data, 
#	but by the time we get to four total variables it's best to give up visualising
#	and just accept that the model provides a prediction for each datapoint based on that point's predictor variable values.
#	The model fit is still minimising the sum of squared deviations between data and prediction.




# Interactions
# Adding in a (multiplicative) interaction term is the same as adding in a new additive predictor
# variable that itself is made up of the product of the two terms in the interaction

# Create a new column in the cars2 data frame that is the product of speed and mass (i.e., speed multiplied by mass):
cars2$speedMass <- cars2$speed * cars2$mass

# Create a model object with an interaction between speed and mass (denoted by 'speed*mass'):
mod2 <- lm(dist ~ speed + mass + speed*mass, data=cars2)

# Create a model object that has the speedMass product in it. Note that we add it in with +, not *:
mod3 <- lm(dist ~ speed + mass + speedMass , data=cars2)

# Look at the summaries to see that the two models are equal
summary(mod2)
summary(mod3)

