# import library
library(ggplot2)

# read the data
data <- read.csv("houses.csv")

# head of the data
head(data)

# statistics of the data
summary(data)

# plot the data
ggplot(data, aes(x = lotsize, y = price)) + 
  geom_point() 

# liner model between lotsize and price
lm1<- lm(data$lotsize ~ data$price)
lm1

# the residuals
res1 <- residuals(lm1)
data$res1 <- res1

# add number of bedrooms as a predictor
lm2<- lm(data$price ~ data$lotsize + data$bedrooms+ data$bathrms)
lm2 
# the residuals
res2 <- residuals(lm2)
data$res2 <- res2

# plot the first model as a line
ggplot(data, aes(x = lotsize, y = price)) + 
  geom_point() + stat_smooth(method = "lm", color = "red")


# plot the first cofficient of the second model
ggplot(data, aes(x = lotsize, y = price)) + 
  geom_point() + geom_abline(slope=lm2$coefficients[2], color = "red")

# plot the second cofficient of the second model
ggplot(data, aes(x = bedrooms, y = price)) + 
  geom_point() + geom_abline(slope=lm2$coefficients[3], color = "red")

# plot the third cofficient of the second model
ggplot(data, aes(x = bathrms, y = price)) + 
  geom_point() + geom_abline(slope=lm2$coefficients[4], color = "red")

# convert driveway column as a factor
data$driveway <- factor(data$driveway)

# add driveway as a predictor
lm3<- lm(data$price ~ data$lotsize + data$bedrooms+ data$bathrms + data$driveway)
lm3
# the residuals
res3 <- residuals(lm3)
data$res3 <- res3

# convert number of bathrooms as a factor
data$bathrms <- factor(data$bathrms)

# re-fit model 2
lm2_new<- lm(data$price ~ data$lotsize + data$bedrooms+ data$bathrms + data$driveway)
lm2_new

# the residuals
res2_new <- residuals(lm2_new)
data$res2_new <- res2_new

# the scatter plot of residuals
ggplot(data, aes(x = price, y = res1)) + 
  geom_point()

ggplot(data, aes(x = price, y = res2)) + 
  geom_point()

ggplot(data, aes(x = price, y = res3)) + 
  geom_point()