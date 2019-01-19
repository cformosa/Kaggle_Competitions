library(caret)

#Exploratory Analysis
iris
str(iris)

iris[iris$Species == 'setosa',]
plot(Species ~ Sepal.Width, data = subset(iris, Species == 'setosa'))

featurePlot(x = iris[, 1:4],
            y = iris$Species,
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

featurePlot(x = iris[, 1:4],
            y = iris$Species,
            plot = "density",
            ## Add a key at the top
            auto.key = list(columns = 3))

library(ggplot2)
g <- ggplot(iris, aes(Sepal.Width, Species))
p <- g + geom_point(aes(color = Species, size = Sepal.Width))
print(p)

g1 <- ggplot(iris, aes(Sepal.Width, Sepal.Length))
p1 <- g1 + geom_text(aes(label = Species), size = 2)
p1

library(dplyr)
iris %>%
  filter(Sepal.Length > 7)

iris %>%
  group_by(Species) %>%
  summarise(avg = mean(Sepal.Width)) %>%
  arrange(avg)

#ANALYSIS
library(caret)

train_idx <- createDataPartition(iris$Species, p = .8, list = FALSE) #list = FALSE is important
train <- iris[train_idx,]
test <- iris[-train_idx,]

cvControl <- trainControl(method = 'cv', number = 5)

##Random Forest
rfGrid <- expand.grid(mtry = 1:10)
rfTune <- train(Species ~ ., data = iris, method = 'rf', trControl = cvControl, tuneGrid = rfGrid)
predicts <- predict(rfTune, test, type = 'raw')
mean(predicts == test$Species)
plot(rfTune)

##Linear SVM
svmTune <- train(Species ~., data = iris, method = 'svmLinear', trContorl = cvControl, tuneLength = 10)
mean(predict(svmTune, train) == train$Species)
mean(predict(svmTune, test) == test$Species)
svmTune$bestTune

#PCA and Clustering
#uses pca results to plot and can then use them to predict!
iris_pca <- prcomp(iris[,1:4], center = TRUE, scale = TRUE)
plot(iris_pca, type = 'l')
summary(iris_pca)
names(iris_pca)
iris_pca$rotation[,1:2]
x <- as.data.frame(iris_pca$x[,1:2])
x$y <- iris$Species
x
ggplot(x, aes(PC1, PC2)) + geom_point(aes(color = y))

wc <- NULL
for(i in 1:4){
  clusts <- kmeans(iris[1:4], centers = i, nstart = 15)
  wc[i] <- clusts$withinss
}
plot(wc, type = 'l')
