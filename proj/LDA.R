library(MASS)
library(ggplot2)
library(neuralnet)
# install.packages("neuralnet")

heart <- read.csv("heart.csv")

# podział zbioru na treningowy i testowy
set.seed(123)
sampled <- sample(2, nrow(heart), replace = TRUE, prob = c(0.75, 0.25))
train <- heart[sampled == 1,]
test <- heart[sampled == 2,]

# skalowanie
train[,2:45] = scale(train[,2:45])
test[,2:45] = scale(test[,2:45])


start_time <- Sys.time()
lnr <- lda(OVERALL_DIAGNOSIS~., train)
end_time <- Sys.time()
end_time - start_time

lnr
# histogramy funkcji dyskryminacji dla poszczególnych klas
class = train$OVERALL_DIAGNOSIS
pred <- predict(lnr, train)
LD1 = pred$x
ldahist(data = LD1, class)
dev.off()

# funkcje gęstości wektora zmiennych dyskryminujących dla obu klas na jednym wykresie
df = data.frame(LD1, class = as.factor(class))
ggplot(data = df)+geom_density(aes(LD1, fill = class), alpha = 0.1)
dev.off()

# macierz pomyłek dla danych treningowych
p.train <- predict(lnr, train)$class
table_train <- table(predicted = p.train, Actual = train$OVERALL_DIAGNOSIS)
table_train
sum(diag(table_train)/sum(table_train))

# macierz pomyłek dla danych testowych
p.test <- predict(lnr, test)$class
table_test <- table(predicted = p.test, Actual = test$OVERALL_DIAGNOSIS)
table_test
sum(diag(table_test)/sum(table_test))


