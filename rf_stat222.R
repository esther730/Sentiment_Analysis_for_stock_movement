library(randomForest)
library(mccr)
data = read.csv('C:/Users/Esther/Desktop/stat222/all_stock_sentiment.csv')
data = data[-which(data$sp_sign==0),]
data$weekday = as.factor(data$weekday)
data$sp_sign = as.factor(data$sp_sign)
data$day_belong = as.factor(data$day_belong)
#data$HIV_sign_diff = as.factor(data$HIV_sign_diff)
View(traindata)
#data = data[-1,]

#testset = data[which((data$Month==11) & (data$Year ==2013)),]
traindata = data[1:nrow(data) * 0.9,] #1781
#traindata = data[-which((data$Month==11) & (data$Year ==2013)),]
#traindata = traindata[-which(traindata$sp_sign==0),]
testset = data[(nrow(data) * 0.9+1):nrow(data),] #178
nrow(traindata)
nrow(testset)
HIV_stock_rfmod <- randomForest(sp_sign ~ day_belong+
                                  weekday+
                                  HIV_PN_diff+
                              #HIV_sign_diff+                           
                              sum_HIV_Negative+
                              sum_HIV_Positive+
                              sum_HIV_Polarity+
                              sum_HIV_Subjectivity+
                              sum_HIV_Negative+
                              # mean_HIV_Negative+
                              # mean_HIV_Positive+
                              # mean_HIV_Polarity+
                              # mean_HIV_Subjectivity+
                              var_HIV_Negative+
                              var_HIV_Positive+
                              var_HIV_Polarity+
                              var_HIV_Subjectivity+
                              cnt,
                              
                              ntree = 100,
                              data = traindata,
                              family="binomial")
auc_train <- AUC(HIV_stock_rfmod$votes[,2], traindata$sp_sign)
train_data_predictions <- predict(HIV_stock_rfmod, traindata, predict.all=T)
#confus.matrix = table(train_data_predictions,traindata$sp_sign)
test_data_predictions <- predict(HIV_stock_rfmod, testset, predict.all=T)
test_data_predictions_prob <- apply(test_data_predictions$individual, 1, function(x){mean(as.numeric(x))})
auc_test <- AUC(test_data_predictions_prob, testset$sp_sign)
pred_test <- prediction(test_data_predictions_prob, as.factor(testset$sp_sign))
perf_test <- performance(pred_test,"tpr","fpr")
jpeg(paste0(getwd(),'/',"HIV","_stock_roc_plot.jpg")) 
plot(perf_test,col="black",lty=2, lwd=3)
legend(0.3,0.3,paste(c("AUC = "),auc_test,sep=""),border="white",cex=1.3,box.col = "white")
dev.off()
LM_stock_rfmod <- randomForest(sp_sign ~ day_belong+
                                 weekday+
                                 LM_PN_diff+                            
                                  sum_LM_Positive+
                                  sum_LM_Polarity+
                                  sum_LM_Subjectivity+
                                  sum_LM_Negative+
                                  # mean_LM_Negative+
                                  # mean_LM_Positive+
                                  # mean_LM_Polarity+
                                  # mean_LM_Subjectivity+
                                  var_LM_Negative+
                                  var_LM_Positive+
                                  var_LM_Polarity+
                                  var_LM_Subjectivity+
                                 #as.factor(traindata$Month)+
                                  cnt,
                                #as.factor(traindata$weekday),                          ,
                                ntree = 100,
                                data = traindata,
                                family="binomial")
auc_train <- AUC(LM_stock_rfmod$votes[,2], traindata$sp_sign)
train_data_predictions <- predict(LM_stock_rfmod, traindata, predict.all=T)
mccr(traindata$sp_sign,train_data_predictions)
test_data_predictions <- predict(LM_stock_rfmod, testset, predict.all=T)
test_data_predictions_prob <- apply(test_data_predictions$individual, 1, function(x){mean(as.numeric(x))})
auc_test <- AUC(test_data_predictions_prob, as.factor(testset$sp_sign))
pred_test <- prediction(test_data_predictions_prob, as.factor(testset$sp_sign))
confusionMatrix(test_data_predictions,testset$sp_sign)
perf_test <- performance(pred_test,"tpr","fpr")
perf_test <- performance(pred_test,"auc")
jpeg(paste0(getwd(),'/',"HIV","_stock_roc_plot.jpg")) 
plot(perf_test,col="black",lty=2, lwd=3)
legend(0.3,0.3,paste(c("AUC = "),auc_test,sep=""),border="white",cex=1.3,box.col = "white")
dev.off()

#SVM_LM_Model <- svm(sp_sign ~ day_belong+
SVM_HIV_Model <- svm(sp_sign ~ day_belong+
                       weekday+
                       # LM_PN_diff+                            
                       # sum_LM_Positive+
                       # sum_LM_Polarity+
                       # sum_LM_Subjectivity+
                       # sum_LM_Negative+
                       # mean_LM_Negative+
                       # mean_LM_Positive+
                       # mean_LM_Polarity+
                       # mean_LM_Subjectivity+
                       var_LM_Negative+
                       var_LM_Positive+
                       var_LM_Polarity+
                       var_LM_Subjectivity+
                       #as.factor(traindata$Month)+
                       cnt, data = traindata)

SVM_HIV_Model <- svm(sp_sign ~ day_belong+
                       weekday+
                       HIV_PN_diff+
                       sum_HIV_Positive+
                       sum_HIV_Polarity+
                       sum_HIV_Subjectivity+
                       sum_HIV_Negative+
                       # mean_HIV_Negative+
                       # mean_HIV_Positive+
                       # mean_HIV_Polarity+
                       # mean_HIV_Subjectivity+
                       var_HIV_Negative+
                       var_HIV_Positive+
                       var_HIV_Polarity+
                       var_HIV_Subjectivity+
                       cnt, data = traindata)

plot(SVM_HIV_Model,data=traindata,color.palette = topo.colors)

auc <- auc@y.values[[1]]

train_data_predictions <-predict(SVM_HIV_Model, traindata, decision.values=TRUE)
train_data_predictions_prob <- attr(train_data_predictions,"decision.values")
pred = prediction(train_data_predictions_prob, traindata$sp_sign)
auc.perf = performance(pred, measure = "auc")
test_data_predictions <-predict(SVM_HIV_Model, testset, decision.values=TRUE)
test_data_predictions_prob <- attr(test_data_predictions,"decision.values")
confus.matrix = table(train_data_predictions,traindata$sp_sign)
pred = prediction(test_data_predictions_prob, testset$sp_sign)
auc.perf = performance(pred, measure = "auc")
#library('sigr')
#calcAUC(as.numeric(train_data_predictions),traindata$sp_sign)
confus.matrix = table(test_data_predictions,testset$sp_sign)
#Matthews Correlation Coefficient(MCC)
tn = 411
tp = 704
fn = 270
fp = 396
tp = 902
tn = 194
fn = 623
fp = 72
(tp*tn -fp*fn)/((tp+fp)*(tp+fn)*(tn+fp)*(tn*fn))

mccr(testset$sp_sign,test_data_predictions)
sum(diag(confus.matrix))/sum(confus.matrix)
plot(SVM_HIV_Model, traindata, sum_HIV_Polarity~cnt,
     slice = list(cnt = 3 , mean_HIV_Polarity = 3),color.palette = terrain.colors)


test_data_predictions_prob <- apply(test_data_predictions$individual, 1, function(x){mean(as.numeric(x))})
auc_test <- AUC(test_data_predictions, testset$sp_sign)
pred_test <- prediction(test_data_predictions, testset$sp_sign)
perf_test <- performance(pred_test,"tpr","fpr")
jpeg(paste0(getwd(),'/',"HIV","_stock_roc_plot.jpg")) 
plot(perf_test,col="black",lty=2, lwd=3)
legend(0.3,0.3,paste(c("AUC = "),auc_test,sep=""),border="white",cex=1.3,box.col = "white")
dev.off()

prop.test(x, n, p = NULL,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95, correct = TRUE)

library(aTSA)
adf.test(data$sum_LM_Polarity)

# ref : https://stackoverflow.com/questions/10369109/finding-lag-at-which-cross-correlation-is-maximum-ccf
Find_Max_CCF<- function(a,b)
{
  d <- ccf(a, b, plot = FALSE)
  cor = d$acf[,,1]
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  res_max = res[which.max(res$cor),]
  return(res_max)
} 
Find_Max_CCF(data$sum_LM_Polarity, data$sp_open_to_close)
Find_Max_CCF(data$cnt, data$sp500_Volume)
Find_Max_CCF(data$cnt, data$sp_volatility)
ccf(data$cnt, data$sp_volatility)
Box.test (data$sp_open_to_close, lag = 1, type = "Ljung")
