require(pROC)
load("data.RData")
cv_y = factor(cv$y, levels=c("no","yes"), labels=c("no","yes"))

#summary of ROC

#C50
filename = "modeling_C50.Rdata"
load(filename)
fit_C50 = fit
rm(fit)
pred_C50 = predict(fit_C50, newdata=cv, type="prob")
#ROC curve
roc_fit_C50 = roc(cv_y, pred_C50[,2], levels=c("no","yes"), plot=T)


#rpart
filename = "modeling_rpart.Rdata"
load(filename)
fit_rpart = fit
rm(fit)
pred_rpart = predict(fit_rpart, newdata=cv, type="prob")
#ROC curve
roc_fit_rpart = roc(cv_y, pred_rpart[,2], levels=c("no","yes"), plot=T)

#random forest
filename = "modeling_RF.Rdata"
load(filename)
fit_RF = fit
rm(fit)
pred_RF = predict(fit_RF, newdata=cv, type="prob")
#ROC curve
roc_fit_RF = roc(cv_y, pred_RF[,2], levels=c("no","yes"), plot=T)

#Neural Network
filename = "modeling_NN.Rdata"
load(filename)
fit_NN = fit
rm(fit)
pred_NN = predict(fit_NN, newdata=cv, type="prob")
#ROC curve
roc_fit_NN = roc(cv_y, pred_NN[,2], levels=c("no","yes"), plot=T)

#Naive Bayes
filename = "modeling_NaiveBayes.Rdata"
load(filename)
fit_NB = fit
rm(fit)
require(e1071)
pred_NB = predict(fit_NB, newdata=cv, type="raw")
#ROC curve
roc_fit_NB = roc(cv_y, pred_NB[,2], levels=c("no","yes"), plot=T)


#SVM
# filename = "modeling_SVM.Rdata"
# load(filename)
# fit_SVM = fit
# rm(fit)
# require(e1071)
# pred_SVM = predict(fit_SVM, newdata=cv, probability=T)
# #ROC curve
# pred_SVM = attr(pred_SVM, "probabilities")
# roc_fit_SVM = roc(cv_y, pred_SVM[,2], levels=c("no","yes"), plot=T)


#ROC_score
ROC_score = data.frame(cbind(sensitivities = roc_fit_NB$sensitivities,
                             specificities = roc_fit_NB$specificities,
                             classifier = rep("NB", length(roc_fit_NB$sensitivitie))))


ROC_score = rbind(ROC_score, data.frame(cbind(sensitivities = roc_fit_rpart$sensitivities,
                             specificities = roc_fit_rpart$specificities,
                             classifier = rep("Rpart", length(roc_fit_rpart$sensitivitie)))))

ROC_score = rbind(ROC_score, data.frame(cbind(sensitivities = roc_fit_C50$sensitivities,
                                              specificities = roc_fit_C50$specificities,
                                      classifier = rep("C50", length(roc_fit_C50$sensitivitie)))))

ROC_score = rbind(ROC_score, data.frame(cbind(sensitivities = roc_fit_RF$sensitivities,
                                              specificities = roc_fit_RF$specificities,
                                              classifier = rep("RF", length(roc_fit_RF$sensitivitie)))))
                                      

ROC_score = rbind(ROC_score, data.frame(cbind(sensitivities = roc_fit_NN$sensitivities,
                                              specificities = roc_fit_NN$specificities,
                                              classifier = rep("NN", length(roc_fit_NN$sensitivitie)))))
                                        

ROC_score = rbind(ROC_score, data.frame(cbind(sensitivities = roc_fit_SVM$sensitivities,
                                              specificities = roc_fit_SVM$specificities,
                                              classifier = rep("SVM", length(roc_fit_SVM$sensitivitie)))))

ROC_score$sensitivities <- as.numeric(as.character(ROC_score$sensitivities))
ROC_score$specificities <- as.numeric(as.character(ROC_score$specificities))

require(ggplot2)
require(scales)

cbPalette <- c("#abdda4", "#2b83ba", "#2b83ba", "#2b83ba", "#d7191c", "#fdae61")

p = ggplot (data = ROC_score, aes(x=specificities, y=sensitivities, gourp=classifier, colour=classifier)) + geom_line(aes(linetype=classifier),size=1) + scale_y_continuous(lim=c(0,1), labels=percent) + scale_x_reverse(lim=c(1,0), labels=percent) + scale_colour_manual(values=cbPalette) + ggtitle("ROC curves for different classifiers") + theme_set(theme_gray(base_size = 22))+ guides(colour = guide_legend(override.aes = list(size=1)))+ xlab("Specificities") + ylab("Sensitivities")

p = p + geom_line(aes(x, y), data.frame(x=c(0,1),y=c(1,0),classifier=c(NA,NA)), color="black") + coord_fixed(1)

p

#summary of best spliting point based on Fscore
readCM = function(modelname){
  filename = paste0("modeling_",modelname,".CM.Rdata")
  load(filename)
  return(cm)
}
CM_C50 = readCM("C50")
CM_rpart = readCM("rpart")
CM_RF = readCM("RF")
CM_NaiveBayes = readCM("NaiveBayes")
CM_NN = readCM("NN")
CM_SVM = readCM("SVM")

plot.bestpoint = function(plot, cm, class){
  require(ggplot2)
  plot = plot + geom_point(aes(x, y), data.frame(x=cm$byClass["Specificity"],y=cm$byClass["Sensitivity"], classifier=class, colour=class), pch= 17, size=5) + geom_point(aes(x, y), data.frame(x=cm$byClass["Specificity"],y=cm$byClass["Sensitivity"], classifier=class), colour="white", pch= 17, size=3)
  return(plot)
}

p = plot.bestpoint(p,CM_NN,"NN")
p = plot.bestpoint(p,CM_RF,"RF")
#p = plot.bestpoint(p,CM_SVM,"SVM")
p = plot.bestpoint(p,CM_rpart,"Rpart")
p = plot.bestpoint(p,CM_NaiveBayes,"NB")
p = plot.bestpoint(p,CM_C50,"C50")

p

#summary of best spliting point based on Fscore
costFunction = function(cm, tn, tp, fn, fp) {
  costMatrix = matrix(c(tn,fp,fn,tp), nrow=2)
  return(sum(sum(cm$table * costMatrix)))
}

findDecisionValue2 = function(prob, cv){
  best_cost = Inf
  best_decision.value = NA
  best_CM = NA
  for (decision.value in seq(0,1,0.01)){
    pred_label = rep("no", length(prob[,2]))
    pred_label[prob[,2] >= decision.value] = "yes"
    require(caret)
    pred_label = factor(pred_label, levels=c("no","yes"))
    cm = confusionMatrix(pred_label, cv, positive="yes")
    cost = costFunction(cm, tn=0, tp=-1, fn=5, fp=1)
    if (cost < best_cost){
      best_cost = cost
      best_decision.value = decision.value
      best_CM = cm
    }
  }
  return(list(best_decision.value, best_CM))
}

plot.bestpoint2 = function(plot, cm, class){
  require(ggplot2)
  plot = plot + geom_point(aes(x, y), data.frame(x=cm$byClass["Specificity"],y=cm$byClass["Sensitivity"], classifier=class, colour=class), pch= 18, size=6.5) + geom_point(aes(x, y), data.frame(x=cm$byClass["Specificity"],y=cm$byClass["Sensitivity"], classifier=class), colour="white", pch= 18, size=4)
  return(plot)
}

p = plot.bestpoint2(p, findDecisionValue2(pred_NB, cv_y)[[2]],"NB")
p = plot.bestpoint2(p, findDecisionValue2(pred_rpart, cv_y)[[2]],"Rpart")
p = plot.bestpoint2(p, findDecisionValue2(pred_C50, cv_y)[[2]],"C50")
p = plot.bestpoint2(p, findDecisionValue2(pred_RF, cv_y)[[2]],"RF")
p = plot.bestpoint2(p, findDecisionValue2(pred_NN, cv_y)[[2]],"NN")
#plot.bestpoint2(p, findDecisionValue2(pred_SVM, cv_y)[[2]],"SVM")

p

#summary of lift charts
lift_curve = function(pred, cv_y, case = "yes") {
  lift_x = c()
  lift_y = c()
  trueTotal = summary(cv_y)[case]
  for (threshold in seq(1,0,-0.01)) {
    x = sum(pred[,2] >= threshold)/length(cv_y)
    y = sum(cv_y[pred[,2] >= threshold] == case)/trueTotal
    lift_x = c(lift_x, x)
    lift_y = c(lift_y, y)
  }
  return(data.frame(cbind(lift_x,lift_y), row.names = NULL))
}

Lift_score = cbind(lift_curve(pred_NB, cv_y),classifier=rep("NB",101))
Lift_score = rbind(Lift_score, cbind(lift_curve(pred_rpart, cv_y),classifier=rep("Rpart",101)))
Lift_score = rbind(Lift_score, cbind(lift_curve(pred_RF, cv_y),classifier=rep("RF",101)))
Lift_score = rbind(Lift_score, cbind(lift_curve(pred_C50, cv_y),classifier=rep("C50",101)))
Lift_score = rbind(Lift_score, cbind(lift_curve(pred_NN, cv_y),classifier=rep("NN",101)))
#Lift_score = rbind(Lift_score, cbind(lift_curve(pred_SVM, cv_y),classifier=rep("SVM",101)))

Lift_score$classifier = factor(Lift_score$classifier)

cbPalette <- c("#abdda4", "#2b83ba", "#2b83ba", "#2b83ba", "#d7191c", "#fdae61")

l = ggplot (data = Lift_score, aes(x=lift_x, y=lift_y, gourp=classifier, colour=classifier)) + geom_line(aes(linetype=classifier),size=1) + scale_y_continuous(lim=c(0,1), labels=percent) + scale_x_continuous(lim=c(0,1), labels=percent) + scale_colour_manual(values=cbPalette) + ggtitle("LIFT charts for different classifiers") + theme_set(theme_gray(base_size = 22))+ guides(colour = guide_legend(override.aes = list(size=1))) + xlab("Specificities") + ylab("Sensitivities") + xlab("% Customers Contacted") + ylab("% Positive Responses")

l = l + geom_line(aes(x, y), data.frame(x=c(0,1),y=c(0,1),classifier=c(NA,NA)), color="black") + coord_fixed(1)

l

#NN performance on test set
pred_NN_testset = predict(fit_NN, newdata=test, type="prob")
test_y = factor(test$y, levels=c("no","yes"), labels=c("no","yes"))
roc_fit_NN_testset = roc(test_y, pred_NN_testset[,2], levels=c("no","yes"), plot=T)
decisionPoint = findDecisionValue2(pred_NN_testset, test_y)

#plot
ROC_score_testset = data.frame(cbind(sensitivities = roc_fit_NN_testset$sensitivities,
                             specificities = roc_fit_NN_testset$specificities))

ROC_score_testset$sensitivities <- as.numeric(as.character(ROC_score_testset$sensitivities))
ROC_score_testset$specificities <- as.numeric(as.character(ROC_score_testset$specificities))

f = ggplot(data = ROC_score_testset, aes(x=specificities, y=sensitivities)) + geom_line(size=1, color="#d7191c") + scale_y_continuous(lim=c(0,1), labels=percent) + scale_x_reverse(lim=c(1,0), labels=percent) + ggtitle("ROC curves for NN on testset") + theme_set(theme_gray(base_size = 21))+ xlab("Specificities") + ylab("Sensitivities")

f = f + geom_line(aes(x, y), data.frame(x=c(0,1),y=c(1,0)), color="black", linetype=2) + coord_fixed(1)

f = f + geom_point(aes(x, y), data.frame(x=decisionPoint[[2]]$byClass["Specificity"],y=decisionPoint[[2]]$byClass["Sensitivity"]), pch= 18, size=6.5, color="#d7191c") + geom_point(aes(x, y), data.frame(x=decisionPoint[[2]]$byClass["Specificity"],y=decisionPoint[[2]]$byClass["Sensitivity"]), color="white", pch= 18, size=4)

f
