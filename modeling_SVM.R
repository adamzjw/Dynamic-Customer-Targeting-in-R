load("data.RData")

#exploratory data analysis
summary(training)

library(e1071)

beginning = Sys.time()
models <- tune.svm(y~., data=training, scale=T, kernel="polynomial", type="C-classification", probability=T,                 degree=1:3, cost=2^(0:7) )
end = Sys.time()
trainingTime = end-beginning

summary(models)
fit <- models$best.model

save(fit, file="modeling_SVM.Rdata")

pred = predict(fit, cv, probability=T, na.action = na.pass)

cv_y = factor(cv$y, levels=c("no","yes"), labels=c("no","yes"))

library(pROC)
prob = attr(pred, "probabilities")
roc_fit = roc(cv_y, prob[,2], levels=c("no","yes"), plot=T)
print(roc_fit)

Fscore =  function(confusionMatrix){
  div <- function(x,y) ifelse(y==0,0,base:::"/"(x,y))
  precision = confusionMatrix$byClass["Pos Pred Value"]
  precision = ifelse(is.na(precision),0,precision)
  recall = confusionMatrix$byClass["Sensitivity"]
  recall = ifelse(is.na(recall),0,recall)
  result = 2 * div(precision * recall, precision + recall)
  names(result) = "Fscore"
  return(result)
}

findDecisionValue = function(prob, cv){
  best_Fscore = -Inf
  best_decision.value = NA
  for (decision.value in seq(0,1,0.05)){
    pred_label = rep("no", length(prob[,2]))
    pred_label[prob[,2]> decision.value] = "yes"
    require(caret)
    pred_label = factor(pred_label, levels=c("no","yes"))
    cm = confusionMatrix(pred_label, cv, positive="yes")
    fs = Fscore(cm)
    if (fs > best_Fscore){
      best_Fscore = fs
      best_decision.value = decision.value
    }
  }
  print(best_Fscore)
  return(best_decision.value)
}

d_value = findDecisionValue(prob,cv_y)
pred_label = rep("no", length(prob[,2]))
pred_label[prob[,2]> d_value] = "yes"
require(caret)
pred_label = factor(pred_label, levels=c("no","yes"))
cm = confusionMatrix(pred_label, cv_y, positive="yes")

Fscore = Fscore(cm)
print(cm)

save(cm, file="modeling_SVM.CM.Rdata")
save(trainingTime, d_value, Fscore, file="modeling_SVM.info.Rdata")