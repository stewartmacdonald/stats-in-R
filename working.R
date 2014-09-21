# THE FOLLOWING IS A WORK IN PROGRESS
#PCA
pca <- princomp(cars2, cor=TRUE)
pca
summary(pca)
pca$loadings
pca$scores
biplot(pca)

skinkSVL		<- c( 20,   20,   23,   24,   26,   27,   29,  30,   32,  35)
skinkMass		<- c(1.0, 1.01, 1.04, 1.09, 1.10, 1.12, 1.15, 1.2, 1.22, 1.3)
skinkSex		<- c('m',  'f',  'f',  'm',  'f',  'm',  'm', 'f',  'm', 'f')
skinkHeadWidth	<- c(3.0,  2.8,  2.9,  3.2,  3.0,  3.3,  3.4, 3.2,  3.5, 3.5)
skinks <- data.frame(ID=1:10, sex=skinkSex, SVL=skinkSVL, mass=skinkMass, headWidth=skinkHeadWidth)
# Note that sex has been converted to a factor with two levels: 'f' and 'm'.

# we know that longer lizards are likely to have wider heads, so we can create a new measurement that is headWidth as a proportion of SVL:
skinks$headWidthProp <- skinks$headWidth / skinks$SVL

# Generate scatterplots of each pair-wise combination of variables.
# This entire plot is known as a draftsman's plot.
# Note that sex is still plotted, but might not mean much as it's a factor.
# However, we can see that two groups of sex values are slightly different.
#  The sex represented by 2 on the x-axis seems to have a slightly higher headWidthProp.
# You can also see that anything involving headWidth or headWidthProp seems to have two parallel lines in.

# We can investigate this further with a boxplot:
boxplot(headWidthProp ~ sex, data=skinks)
# This tells us that males typically have wider heads.

fit <- aov(headWidthProp ~ sex, data=skinks)
summary(fit)

print(model.tables(fit,"means"),digits=3) 

pairs(skinks)

skink.lm <- lm(headWidth ~ SVL + mass + sex, data=skinks)

