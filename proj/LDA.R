heart <- read.csv("heart.csv")
# head(heart, 1)
# str(heart)
# summary(heart)

# podział zbioru na treningowy i testowy
set.seed(123)
sampled <- sample(2, nrow(heart), replace = TRUE, prob = c(0.75, 0.25))
train <- heart[sampled == 1,]
test <- heart[sampled == 2,]

# skalowanie
train[,2:45] = scale(train[,2:45])
test[,2:45] = scale(test[,2:45])

library(MASS)
lnr <- lda(OVERALL_DIAGNOSIS~., train)

# lnr$prior
# lnr$counts

class = train$OVERALL_DIAGNOSIS
pred <- predict(lnr, train)
ldahist(data = pred$x[,1], class)
dev.off()

# g = train$OVERALL_DIAGNOSIS
# data = pred$x[,1]

library(ggplot2)

LD1 = pred$x
df = data.frame(LD1, class = as.factor(class))
ggplot(data = df)+geom_density(aes(LD1, fill = class), alpha = 0.1)

