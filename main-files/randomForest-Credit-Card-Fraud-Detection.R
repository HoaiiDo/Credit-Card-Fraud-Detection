library(randomForest)
library(yardstick)
#bagging using random forest technique
#Bagging
set.seed(1234)
memory.limit(size = 15000)
credit_rdf <- randomForest(as.factor(Class) ~ ., data = train_data, ntree = 300, mtry = 6, importance = TRUE)
credit_rdf
#variable importance
importance <- data.frame(credit_rdf$importance)


#plot the variable importance 
Imp1 <- ggplot(importance, aes(x=reorder(rownames(importance),MeanDecreaseGini), y=MeanDecreaseGini)) +
  geom_bar(stat="identity", fill="tomato", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (MeanDecreaseGini)", x="Variable", y="Variable importance (MeanDecreaseGini)")
Imp2 <- ggplot(importance, aes(x=reorder(rownames(importance),MeanDecreaseAccuracy), y=MeanDecreaseAccuracy)) +
  geom_bar(stat="identity", fill="lightblue", colour="black") +
  coord_flip() + theme_bw(base_size = 8) +
  labs(title="Prediction using RandomForest with 100 trees", subtitle="Variable importance (MeanDecreaseAccuracy)", x="Variable", y="Variable importance (MeanDecreaseAccuracy)")
gt <- arrangeGrob(Imp1, Imp2, ncol=2)
as_ggplot(gt)


#prediction
rf.pred <- as.factor(predict(credit_rdf, newdata = up_test))
#confusion matrix
conf <- confusionMatrix(rf.pred, up_test$Class, positive = "1")
conf
#area under the curve(AUC)
roc.curve(up_test$Class, rf.pred, plotit = TRUE)

