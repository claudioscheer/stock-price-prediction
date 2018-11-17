data <- read.csv("datasets/dataset-ibm.csv", header = TRUE)
head(data)

data$Date <- as.numeric(as.Date(data$Date, "%Y-%m-%d"))

train <- sample(nrow(data), 0.8 * nrow(data), replace = FALSE)
train_set <- data[train,]
validation_set <- data[-train,]

regression_model <-
  lm(Close ~ Date * Open * High * Low, data = train_set)
save(regression_model, file = "src/linear-regression-model.Rdata")

summary(regression_model)

prediction_y <-
  predict(regression_model, validation_set, interval = "prediction")
output <- cbind(validation_set, prediction_y)

library(ggplot2)

ggplot(output, aes(x = Date * Open * High * Low, y = Close)) +
  geom_point(col = "blue", size = 0.5) +
  stat_smooth(method = "lm", col = "red")
