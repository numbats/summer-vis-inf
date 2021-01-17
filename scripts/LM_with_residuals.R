require(ggplot2)
require(nullabor)

# Simple linear regression model
plot(mpg ~ wt, data = mtcars)
l1 <- lm(mpg ~ wt, data = mtcars)
summary(l1)
par(mfrow = c(2,2))
plot(l1)
resd <- resid(l1)
plot(mtcars$wt, resd)
plot(mtcars$mpg, resd)
