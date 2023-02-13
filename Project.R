data <- read.csv("F:/sem 9/DS/FINAL/diabetes.csv",header = TRUE,sep=',')

str(data)

data["SkinThickness"][data["SkinThickness"] == 0] <- NA
data["BloodPressure"][data["BloodPressure"] == 0] <- NA
data["Insulin"][data["Insulin"] == 0] <- NA

summary(data)

library(zoo)

data_miss_fixed <- na.aggregate(data)

normalize = function(x){
  
  return((x-min(x))/(max(x)-min(x)))
}

data_normal <- as.data.frame(lapply(data_miss_fixed[,1:9], normalize))


data_normal["Outcome"][data_normal["Outcome"] == 0] <- "Negative"
data_normal["Outcome"][data_normal["Outcome"] == 1] <- "Positive"

library(caTools)
library(class)

split <- sample.split(data_normal, SplitRatio = 0.7)
train_cl <- subset(data_normal, split == "TRUE")
test_cl <- subset(data_normal, split == "FALSE")

train_scale <- train_cl[, 1:8]
test_scale <- test_cl[, 1:8]

library(class)

knn_model <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Outcome,
                      k = 22)
knn_model

cm <- table(test_cl$Outcome, knn_model)
cm

accu <- 100 * sum(test_cl$Outcome == knn_model)/NROW(test_cl$Outcome)


