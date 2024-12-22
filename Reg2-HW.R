#İnstall Neccesary Packages
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)
install.packages("lattice")
library(lattice)
install.packages("caret")
library(caret)
install.packages("corrplot")
library(corrplot)
install.packages("naniar")
library(naniar)
install.packages("psych")
library(psych)
install.packages("e1071")
library(e1071)
install.packages("randomForest")
library(randomForest)
rfNews()
library(MASS)
library(stats)
install.packages("psych")
library(psych)

#Understanding Data

dataset <- read_excel("/Users/metinvs/Downloads/ENG.xlsx")
dataset$MATH

# RSN = (PV1MPRE+...+PV10MPRE)/10   - Score of math reasoning
# UNDR = (PV1MPIN+...+PV10MPIN)/10  - Score of interpreting applying and evaluating of math
# MUNDR = (PV1MPFS+...+PV10MPFS)/10 - Score of math formulation
# EMPL = (PV1MPEM+...+PV10MPEM)/10  - Score of employing math concepts
# CRT = (PV1MCUD+...+PV10MCUD)/10   - Score of uncertainty and data
# SPC = (PV1MCSS+...+PV10MCSS)/10   - Score of space and shape understanding
# MATH = (PV1MATH+...+PV10MATH)/10  - Score of math

dplyr::describe(dataset)
describe(dataset)
summary(dataset$MATH)
summary(dataset$SCIE)
summary(dataset$READ)
range(dataset$MATH)
par(mfrow=c(1,3))
boxplot(dataset$MATH, col = "lightblue", main = "MATH")
boxplot(dataset$SCIE, col = "lightyellow", main = "SCIE")
boxplot(dataset$READ, col = "lightgreen", main = "READ")

par(mfrow=c(1,1))
describe(dataset)
describe(dataset$MATH)
describe(dataset$SCIE)
describe(dataset$READ)
describe(dataset$CRT)

correlation_matrix <- cor(dataset[c("MATH", "READ","SCIE","CRT",
                                    "SPC","EMPL","ST268Q04JA","ST059Q01TA")], use = "complete.obs") 
corrplot(correlation_matrix, method = "circle", type = "upper", 
         title = "Corelation Matrix between math and science", 
         cl.pos = "b", tl.col = "black")

correlations <- cor(dataset[, sapply(dataset, is.numeric)])
# Korelasyon matrisini çizin
corrplot(correlations, method="circle")

#Get How Many Unique SchoolID
schoolid <- str(dataset$CNTSCHID)
schoolid <- as.character(dataset$CNTSCHID)
schoolid
dataset_nondup <- dataset[!duplicated(dataset$CNTSCHID), ]
school_counts <- dataset_nondup %>% 
  count(CNTSCHID, sort = TRUE)
print(school_counts)

#Select Two Student Each School
set.seed(123)  
random_students <- dataset %>%
  group_by(CNTSCHID) %>%
  sample_n(size = 2, replace = FALSE) %>%
  ungroup()

#Data Preparing
dataset$AGE[dataset$AGE == 0] <- NA
dataset$ESCS[dataset$ESCS == 0] <- NA
dataset$HISEI[dataset$HISEI == 0] <- NA
dataset$STRATUM[dataset$STRATUM == 0] <- NA
dataset$PAREDINT[dataset$PAREDINT == 0] <- NA

# Check missing values
missing_values <- sapply(dataset, function(x) sum(is.na(x)))
print(missing_values)
# Copy for keep original dataset
dataset_filled <- dataset  
# Fill data with mean
for (col in names(dataset_filled)) {
  if (is.numeric(dataset_filled[[col]])) {  
    mean_value <- mean(dataset_filled[[col]], na.rm = TRUE)  
    dataset_filled[[col]][is.na(dataset_filled[[col]])] <- mean_value 
  }
}

# Data Check 
missing_values_after <- sapply(dataset_filled, function(x) sum(is.na(x)))
print(missing_values_after)  # Tüm sütunlar için eksik değerlerin sıfır olduğunu doğrulayın

# Linear Model-1
# H0: Reading and science score effect math score.
# H1: Reading and science score does not effect math score.
set.seed(12345)
train_index <- createDataPartition(dataset$MATH, p = 0.8, list = FALSE)
train_set <- dataset[train_index, ]
test_set <- dataset[-train_index, ]

model <- lm(MATH ~ READ + SCIE, data = train_set)
train_predictions <- predict(model, train_set)
test_predictions <- predict(model, test_set)
train_rmse <- sqrt(mean((train_set$MATH - train_predictions)^2))
test_rmse <- sqrt(mean((test_set$MATH - test_predictions)^2))
cat(" Train RMSE:",mae,"\n","Test RMSE:",rmse)
summary(model)
par(mfrow = c(2, 2))
plot(model)

par(mfrow=c(1,2))
hist(predictions, col = 'lightblue',main ='Model Predictions')
hist(random_students$MATH, col = 'lightgreen',xlab = 'Math Score',main = 'Actual Values')
par(mfrow=c(1,1))

# Linear Model-2
set.seed(456456)
train_index2 <- createDataPartition(dataset$MATH, p = 0.8, list = FALSE)
train_set2 <- dataset[train_index2, ]
test_set2 <- dataset[-train_index2, ]
model2 <- lm(MATH ~ RSN + UNDR + EMPL + MUNDR, data = random_students)
train_predictions2 <- predict(model, train_set2)
test_predictions2 <- predict(model, test_set2)
train_rmse2 <- sqrt(mean((train_set2$MATH - train_predictions2)^2))
test_rmse2 <- sqrt(mean((test_set2$MATH - test_predictions2)^2))
cat("Train RMSE:", train_rmse2, "\n")
cat("Test RMSE:", test_rmse2, "\n")
summary(model2)
par(mfrow = c(2, 2))
plot(model)

#Random Forest
set.seed(789789)  
index <- createDataPartition(random_students$MATH, p = 0.9, list = FALSE) 
train_set_rf <- random_students[index, ]
test_set_rf <- random_students[-index, ]
rfmodel <- randomForest(MATH ~ READ + SCIE, data = train_set_rf, ntree = 100, proximity = TRUE) 
importance(rfmodel)
rf_predictions <- predict(rfmodel, test_set_rf)
actuals <- test_set_rf$MATH
mae <- mean(abs(rf_predictions - actuals))  
rmse <- sqrt(mean((rf_predictions - actuals)^2))

cat(" Random Forest MAE:",mae,"\n","Random Forest RMSE:",rmse)
print(rfmodel)
print(rf_model)
cat(" Random Forest MAE:",mae,"\n","Linear Regression MAE:", mae_lm, "\n", 
    "Random Forest RMSE:",rmse,"\n","Linear Regression RMSE:", rmse_lm, "\n")

# Cross-validation
train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(MATH ~ READ + SCIE, data = dataset, method = "lm", trControl = train_control)
rfmodel_cv <- train(MATH ~ READ + SCIE, data = dataset, method = "rf", trControl = train_control)
lm_results <- model_cv$results
rf_results <- rfmodel_cv$results

#Error Check
print("Linear Regression Results:")
print(lm_results)
print("Random Forest Results:")
print(rf_results)

#Performance Metrics
summary(model)$coefficients
variable_importance <- importance(rfmodel)
print(variable_importance)



