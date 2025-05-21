### TASK 1 ###
df = read.table('C:/Users/aleks/Desktop/eLearning_Folder/Advanced_Data_Analysis_using_R/Telikh_Ergasia/companies.txt', header=TRUE)

reg1 = lm( Sales ~ Market_Value, data=df )
summary(reg1)

# Normality Tests for Residuals
plot(reg1, which = 1:4)
qqnorm(reg1$residuals)
qqline(reg1$residuals, col = "steelblue", lwd = 2)


install.packages("nortest")
library(nortest)
lillie.test(reg1$residuals)
shapiro.test(reg1$residuals)

# Homoscedasticity tests for residuals

plot( fitted(reg1), rstandard(reg1) )

#install.packages("car")
#library(car)
#leveneTest(reg1$residuals)

# Runs test
install.packages('randtests')
library(randtests)
runs.test(reg1$residuals)

############ logarithm transform data ###############


reg2 = lm( log(Sales) ~ log(Market_Value), data=df )
summary(reg2)

# Normality Tests for Residuals
plot(reg2, which = 1:4)
qqnorm(reg2$residuals)
qqline(reg2$residuals, col = "steelblue", lwd = 2)


install.packages("nortest")
library(nortest)
lillie.test(reg2$residuals)
shapiro.test(reg2$residuals)

# Homoscedasticity tests for residuals

plot( fitted(reg2), rstandard(reg2) )

# Runs test
install.packages('randtests')
library(randtests)
runs.test(reg2$residuals)


# Diagnostics

par(mfrow=c(3,2))
plot(reg2, which=1:4)

##### TASK 2 #############################

# a)
AssetsTr = log(df$Assets)
SalesTr = log(df$Sales)
Market_ValueTr = log(df$Market_Value)
ProfitTr = sign(df$Profit)*log(abs(df$Profit))

# b)
reg3 = lm( log(Sales) ~ AssetsTr + Market_ValueTr + ProfitTr, data=df )
summary(reg3)

#d)
final_model = step(reg3, direction='both')
final_model



##### TASK 4 #############################

# (a)
df = read.table('C:/Users/aleks/Desktop/eLearning_Folder/Advanced_Data_Analysis_using_R/Telikh_Ergasia/companies.txt', header=TRUE)
attach(df)

# change of variable
for (i in 1:79) {
  if (Profits[i] > 0) {
    df$Profitable[i] <- 1
  }
  else {
    df$Profitable[i] <- 0
  }
}
attach(df)


# Model selection
model3 <- glm(Profitable ~ Assets + Sales + Market_Value + Employees + Sector
              ,family = binomial)
final_model3 = step(model3, direction='both')
summary(final_model3)

# (c)
null = glm(Profitable ~ 1, family = "binomial")
anova(null, final_model3, test="Chisq")

# (d)
newdata = data.frame(Sector = 'A', Assets = mean(Assets), Market_Value = mean(Market_Value)
                     , Sales= mean(Sales), Employees = mean(Employees))
predict(final_model3, newdata, type="response")
val1 = predict(final_model3, newdata, type="response")
p = exp(val1)/(1+exp(val1))
print(p)


# TASK 3

# a)

df_4 = read.table('C:/Users/aleks/Desktop/eLearning_Folder/Advanced_Data_Analysis_using_R/Telikh_Ergasia/companies.txt', header=TRUE)
attach(df_4)
model4 <- glm(Market_Value ~ Assets + Sales + Profits + Employees + Sector, 
              data = df_4,family = poisson(link=log))
final_model4 = step(model4, direction='both')

# b)
summary(final_model4)

# c)

null_4 <- glm(Market_Value~1, data = df_4, family=poisson)
anova (final_model4, null_4, test = "Chisq")

# d)

newdata4 = data.frame(Sector = 'A', Assets = mean(Assets), 
                      Sales= mean(Sales), Profits = mean(Profits), Employees = mean(Employees))
exp(predict(final_model4, newdata = newdata4))


###### TASK 5 #########

# (a)

df = read.table('C:/Users/aleks/Desktop/eLearning_Folder/Advanced_Data_Analysis_using_R/Telikh_Ergasia/companies.txt', header=TRUE)
attach(df)

# change of variable
for (i in 1:79) {
  if (Profits[i] > 0) {
    df$Profitable[i] <- 1
  }
  else {
    df$Profitable[i] <- 0
  }
}
attach(df)

library(MASS)

model5 <- lda(Sector ~ Assets + Sales + 
                Market_Value + Profits +Employees, data = df)
model52 <- predict(model5, data = df)
t <- table(model52$class, df$Sector)
t
sum(diag(t))/sum(t) 

# c) 
library(class)
df5 <- df[ , ! names(df) %in% c("Company_Name", "Sector")]
model5c <- knn(train = df5, test = df5, cl = Profitable , k=4)
t2 <- table(model5c, Profitable)
t2
sum(diag(t2))/sum(t2) 

# d)
install.packages("tree")
library("tree")
fit1 <- tree(as.factor(Profitable) ~ Assets + Sales + 
               Market_Value +Employees, data = df5)
summary(fit1)
plot(fit1, lwd=2, col="blue")
text(fit1, cex = 1.3)

 
