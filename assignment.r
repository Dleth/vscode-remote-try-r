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

# botplot of the bedrooms and price
ggplot(data, aes(x = as.factor(bedrooms), y = price)) + 
    geom_boxplot()

# botplot of the bathrooms and price
ggplot(data, aes(x = as.factor(bedrooms), y = price)) + 
    geom_boxplot()

# boxplot of stories and price
ggplot(data, aes(x = as.factor(stories), y = price)) + 
    geom_boxplot()

# boxplot of driveway and price
ggplot(data, aes(x = as.factor(driveway), y = price)) + 
    geom_boxplot()

# boxplot of recroom and price
ggplot(data, aes(x = as.factor(recroom), y = price)) + 
    geom_boxplot()

# boxplot of gashw and price
ggplot(data, aes(x = as.factor(gashw), y = price)) + 
    geom_boxplot()

# boxplot of airco and price
ggplot(data, aes(x = as.factor(airco), y = price)) + 
    geom_boxplot()

# boxplot of garagepl and price
ggplot(data, aes(x = as.factor(garagepl), y = price)) + 
    geom_boxplot()

# boxplot of prefarea and price
ggplot(data, aes(x = as.factor(prefarea), y = price)) + 
    geom_boxplot()

# liner model between lotsize and price
lm1<- lm(data$lotsize ~ data$price)
lm1

# the residuals
res1 <- residuals(lm1)
data$res1 <- res1

# add number of bedrooms and bathrooms as a predictor
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
lm3<- lm(data$price ~ data$lotsize + data$bedrooms+ data$bathrms)
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

# the scatter plot of residuals, mean and standard deviation of the residuals
mean(res1)
sd(res1)
ggplot(data, aes(x = price, y = res1)) + 
  geom_point()

mean(res2)
sd(res2)
ggplot(data, aes(x = price, y = res2)) + 
  geom_point()

mean(res3)
sd(res3)
ggplot(data, aes(x = price, y = res3)) + 
  geom_point()

mean(res2_new)
sd(res2_new)
ggplot(data, aes(x = price, y = res2_new)) + 
  geom_point()


# convert columns to factors
data$bedrooms <- factor(data$bedrooms)
data$driveway <- factor(data$driveway)
data$stories <- factor(data$stories)
data$recroom <- factor(data$recroom)
data$fullbase <- factor(data$fullbase)
data$gashw <- factor(data$gashw)
data$airco <- factor(data$airco)
data$garagepl <- factor(data$garagepl)
data$prefarea <- factor(data$prefarea)

# liner model between al the columns and price
lm4<- lm(data$price ~ data$lotsize + data$bedrooms+ data$bathrms + data$driveway + data$stories + data$recroom + data$fullbase + data$gashw + data$airco + data$garagepl + data$prefarea)

# the residuals
res4 <- residuals(lm4)
data$res4 <- res4

# stepwise regression
lm4_step<- step(lm4)

# the residuals
res4_step <- residuals(lm4_step)
data$res4_step <- res4_step

# step with interaction
lm4_step_intera<- step(lm4_step, scope = ~.*.)
res4_step_intera <- residuals(lm4_step_intera)
data$res4_step_intera <- res4_step_intera

# plot the residuals 
mean(res4)
sd(res4)
ggplot(data, aes(x = price, y = res4)) + 
  geom_point()

mean(res4_step)
sd(res4_step)
ggplot(data, aes(x = price, y = res4_step)) + 
  geom_point()

mean(res4_step_intera)
sd(res4_step_intera)
ggplot(data, aes(x = price, y = res4_step_intera)) + 
  geom_point()