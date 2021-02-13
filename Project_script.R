data <- read.csv("C:/Users/jocel/OneDrive/2020_3_Fall/MIS 382N - Marketing Analysis/Project/rd4dd_census_clean.csv")
data_abrg <- data
#data$store_age_binary <- if_else(data$OP_time < 4, 0, 1)

#data$OP_time <- NULL

#linear model to test and see which coefs are significant
lm <- lm(store_age_binary ~ ., data = data)
summary(lm)

#correlation matrix
res <- cor(data_abrg)
round(res, 2)

# multinomial log reg
library(nnet)
library(MASS)

train <- read.csv("C:/Users/jocel/OneDrive/2020_3_Fall/MIS 382N - Marketing Analysis/Project/x_train_alex.csv")
test <- read.csv("C:/Users/jocel/OneDrive/2020_3_Fall/MIS 382N - Marketing Analysis/Project/x_test_alex.csv")

# binary logit
#logitMod <- glm(store_age_binary ~  ., data=train, family=binomial(link="logit"))
#summary(logitMod)
#PseudoR2(logitMod, which = "all")

#predicted <- plogis(predict(logitMod, test))  # predicted scores
# or
#predicted <- predict(logitMod, testData, type="response") 

lin_reg <- lm(OP_time ~ ., data = train)
summary(lin_reg)

#ordered logistic regression
m <- polr(as.factor(OP_time) ~ as.factor(District) + Latitude + Longitude +
            as.factor(Ultra_food) + Per_ultra + as.factor(Ready_food) + as.factor(CERE) +
            as.factor(WHITE) + as.factor(VITAMINV) + as.factor(VITAMINRT) + as.factor(DARKV) +
            as.factor(OTHERV) + as.factor(VITAMINF) + as.factor(OTHERF) + as.factor(ORGANM) +
            as.factor(FLESHM) + as.factor(FISH) + as.factor(INSECTSV) + as.factor(EGGS) +
            as.factor(MILK) + as.factor(NUTS) + as.factor(LEGUM) + as.factor(OILF) + as.factor(SWEETS) +
            as.factor(SAB) + as.factor(OUT_CER) + as.factor(Shop_type), data = train, Hess = TRUE)

#m <- polr(as.factor(OP_time) ~ ., data = train, Hess = TRUE)


m_3 <- polr(as.factor(OP_time) ~ as.factor(District) + Latitude + Longitude +
            Per_ultra + as.factor(CERE) + as.factor(WHITE) + as.factor(VITAMINV) + as.factor(VITAMINRT) + as.factor(DARKV) +
            as.factor(OTHERV) + as.factor(VITAMINF) + as.factor(OTHERF) + as.factor(ORGANM) +
            as.factor(FISH) + as.factor(INSECTSV) + 
            as.factor(MILK) + as.factor(LEGUM) + as.factor(OILF) + as.factor(SWEETS) +
            as.factor(SAB) + as.factor(OUT_CER) + as.factor(Shop_type) + SWEETS:SAB + OTHERV:OILF + VITAMINV:OTHERV + FISH:LEGUM + WHITE:VITAMINV, data = train, Hess = TRUE)

#OTHERV:OILF + VITAMINV:OTHERV + Shop_type:LEGUM + FISH:LEGUM + WHITE:VITAMINV + District:INSECTSV + District:Shop_type
#+ as.factor(SWEETS):as.factor(SAB) +
 # as.factor(OTHERV):as.factor(OILF) + as.factor(VITAMINV):as.factor(OTHERV) +
  #as.factor(Shop_type):as.factor(LEGUM) + as.factor(FISH):as.factor(LEGUM
summary(m_3)

library(DescTools)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, direction = 'both')
PseudoR2(m2, which = "all")


# five features most positive
m <- polr(as.factor(OP_time) ~ as.factor(OILF) + as.factor(Shop_type) + as.factor(CERE) + 
            as.factor(SAB) + as.factor(NUTS), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, ~.^2)
summary(m2)

predictions <- predict(m2, test)
addmargins(table(predicted = predictions,actual = test$OP_time))
# 67.6% train accuracy
# 72.3% test accuracy


# five from RFE
m <- polr(as.factor(OP_time) ~ as.factor(OTHERF) + as.factor(ORGANM) + as.factor(INSECTSV) + 
            as.factor(MILK) + as.factor(OILF), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, ~.^2)
summary(m2)

predictions <- predict(m, train)
addmargins(table(predicted = predictions,actual = train$OP_time))
# 66.9% train 
# 68.75% test

# ten from RFE
m <- polr(as.factor(OP_time) ~ as.factor(Shop_type) + as.factor(Ready_food) + as.factor(DARKV) + 
            as.factor(OTHERF) + as.factor(ORGANM) + as.factor(INSECTSV) + as.factor(MILK) +
            as.factor(NUTS) + as.factor(OILF) + as.factor(SAB), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, ~.^2)

summary(m2)

predictions <- predict(m2, test)

addmargins(table(predicted = predictions,actual = test$OP_time))
# 67.4% train 
# 74.1% test


# ten from RFE
m <- polr(as.factor(OP_time) ~ as.factor(Shop_type) + as.factor(Ultra_food) + as.factor(Ready_food) + as.factor(DARKV) + 
            + as.factor(VITAMINF) + as.factor(OTHERF) + as.factor(ORGANM) +
            as.factor(OILF) + as.factor(SAB) + as.factor(OUT_CER), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, ~.^2)

summary(m2)

predictions <- predict(m2, test)

addmargins(table(predicted = predictions,actual = test$OP_time))
# 67.6% train 
# 72.3% test

# with STEP_AIC, went up to 74.1%


# RFE 15
m <- polr(as.factor(OP_time) ~ as.factor(District) + as.factor(Shop_type) + as.factor(Ready_food) + 
            as.factor(CERE) + as.factor(WHITE) + as.factor(DARKV) + as.factor(OTHERF) + 
            as.factor(ORGANM) + as.factor(INSECTSV) + as.factor(MILK) + as.factor(NUTS) + as.factor(LEGUM) +
            as.factor(OILF) + as.factor(SAB) + as.factor(OUT_CER), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m)

summary(m2)

predictions <- predict(m2, test)

addmargins(table(predicted = predictions,actual = test$OP_time))

# test - 74.1% accuracy



# RFE 15
m <- polr(as.factor(OP_time) ~ ., data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, ~.^2)

summary(m2)

predictions <- predict(m2, test)

addmargins(table(predicted = predictions,actual = test$OP_time))

# test - 74.1% accuracy




# RFE 20
m <- polr(as.factor(OP_time) ~ as.factor(District) + as.factor(Shop_type) + as.factor(Ultra_food) +
            as.factor(Ready_food) + 
            as.factor(CERE) + as.factor(WHITE) + as.factor(VITAMINV) + as.factor(DARKV) + as.factor(VITAMINF) +
            as.factor(OTHERF) + 
            as.factor(ORGANM) + as.factor(INSECTSV) + as.factor(MILK) + as.factor(NUTS) + as.factor(LEGUM) +
            as.factor(OILF) + as.factor(SWEETS) + as.factor(SAB) + as.factor(OUT_CER), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, direction = "both")

summary(m2)

predictions <- predict(m, test)

addmargins(table(predicted = predictions,actual = test$OP_time))

# test - 74.1% accuracy




# RFE 20
m <- polr(as.factor(OP_time) ~ as.factor(Shop_type) + as.factor(OUT_CER), data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

m2 <- stepAIC(m, direction = "both")

summary(m2)

predictions <- predict(m, test)

addmargins(table(predicted = predictions,actual = test$OP_time))

# test - 74.1% accuracy




library(ggplot2)

# stacked bar chart
ggplot(train, 
       aes(x = as.factor(OP_time), 
           fill = as.factor(OILF))) + 
  geom_bar(position = "fill") +
  labs(title = 'Certification presence by operating time',y = "Proportion", x = "Operating time", fill = "Certification")

temp <- train[,2:28]
train.pca <- prcomp(temp)
summary(train.pca)

train.pca$x[,1:2]

new_train = data.frame(train$OP_time)
new_train$PC1 = train.pca$x[,1]
new_train$PC2 = train.pca$x[,2]
new_train$PC3 = train.pca$x[,3]
new_train$PC4 = train.pca$x[,4]
new_train$PC5 = train.pca$x[,5]
new_train$PC6 = train.pca$x[,6]
new_train$PC7 = train.pca$x[,7]
new_train$PC8 = train.pca$x[,8]
new_train$PC9 = train.pca$x[,9]
new_train$PC10 = train.pca$x[,10]
new_train$PC11 = train.pca$x[,11]
new_train$PC12 = train.pca$x[,12]
new_train$PC13 = train.pca$x[,13]
new_train$PC14 = train.pca$x[,14]
new_train$PC15 = train.pca$x[,15]
new_train$PC16 = train.pca$x[,16]
new_train$PC17 = train.pca$x[,17]
new_train$PC18 = train.pca$x[,18]

test.pca = predict(train.pca, test)



new_train$train.OP_time = factor(new_train$train.OP_time, levels = c(1,2,3,4), ordered = FALSE)

m <- polr(train.OP_time ~ PC1+ PC2 + PC3 + PC4, data = new_train, Hess = TRUE)

summary(m)
library(DescTools)
PseudoR2(m, which = "all")

#m2 <- stepAIC(m, ~.^2, direction = 'both')

predictions <- predict(m, test.pca)
addmargins(table(predicted = predictions,actual = test$OP_time))
# 75% test accuracy



#Multinomial - unordered - 75% test accuracy
train$OP_time = factor(train$OP_time, levels = c(1,2,3,4), ordered = FALSE)
library(nnet)
model <- multinom(OP_time ~ as.factor(Shop_type) + as.factor(OTHERF) + 
                    as.factor(ORGANM) + as.factor(INSECTSV) + 
                    as.factor(MILK) + as.factor(OILF), data = train)
summary(model)

predictions <- predict(model, test)
addmargins(table(predicted = predictions,actual = test$OP_time))
#75% test accuracy

# BEST - 75% testing accuracy
train$OP_time = factor(train$OP_time, levels = c(1,2,3,4), ordered = TRUE)

m <- polr(OP_time ~ as.factor(Shop_type)  + as.factor(OTHERF) + 
            as.factor(ORGANM) + as.factor(INSECTSV) + 
            as.factor(MILK) + as.factor(OILF), data = train, Hess = TRUE)
options(scipen = 999)
summary(m)
exp(cbind(OR = coef(m)))
library(DescTools)
PseudoR2(m, which = "all")

#m2 <- stepAIC(m, ~.^2, direction = 'both')

predictions <- predict(m, test)
addmargins(table(predicted = predictions,actual = test$OP_time))
# 75% test accuracy



train$OP_time = factor(train$OP_time, levels = c(1,2,3,4), ordered = TRUE)
library(MASS)
m <- polr(OP_time ~ as.factor(District) + as.factor(Shop_type) + Ultra_food + Ready_food + 
            CERE + WHITE + VITAMINV + VITAMINRT + DARKV + VITAMINF + 
            OTHERF + ORGANM + INSECTSV + MILK + NUTS + LEGUM + OILF +
            SWEETS + SAB + OUT_CER, data = train, Hess = TRUE)

summary(m)
PseudoR2(m, which = "all")

#m2 <- stepAIC(m, ~.^2, direction = 'both')

predictions <- predict(m, test)
addmargins(table(predicted = predictions,actual = test$OP_time))
