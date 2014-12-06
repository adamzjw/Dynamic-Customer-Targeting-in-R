require(e1071)

simulation = function(formula, data, init, each, iter){
  rownames(data) = 1:nrow(data)
  candidate=c()
  new_candidate=c()
  random_index = sample(1:dim(data)[1])
  candidate = c(candidate, random_index[1:init])
  
  result = list(candidate)
  trainingTime = rep(NA, iter)
  for (i in 1:iter) {
    beginning = Sys.time()
    fit1 = naiveBayes(formula, data[candidate,])
    fit2 = naiveBayes(formula, data[candidate[(length(candidate)-each):length(candidate)],])
    end = Sys.time()
    trainingTime[i] = end-beginning
    
    pred1 = predict(fit1, newdata=data[-candidate,], type="raw")
    rownames(pred1) = rownames(data)[-candidate]
    pred2 = predict(fit2, newdata=data[-candidate,], type="raw")
    rownames(pred2) = rownames(data)[-candidate]
      
    pred1_score = pred1[pred1[,2] > 0,]
    pred2_score = pred2[pred2[,2] > 0,]
    #pred1_sorted = pred1_score[order(pred1_score[,2], decreasing = T),]
    pred_sorted = pred1[,2] + pred2[,2]
    pred_sorted = pred_sorted[order(pred_sorted, decreasing = T)]
    print(head(pred_sorted))
    #new_candidate = unique(c(as.numeric(rownames(pred1_sorted)),as.numeric(rownames(pred2_sorted))))
    new_candidate = unique(c(as.numeric(names(pred_sorted))))
    print(head(new_candidate))
    print(length(new_candidate))
    if (length(new_candidate)>each){
      result[[i+1]] = new_candidate[1:each]
    } else {
      print("warning")
      result[[i+1]] = c(new_candidate,sample(x=(1:dim(data)[1])[-candidate],size=(each-length(new_candidate))))
    }
    candidate = c(candidate, result[[i+1]])
  }
  return(result)
}
numTrials = function(formula, data, n,init, each){
  iter = floor((nrow(data)-init)/each)
  result = list()
  for (i in 1:n){
    result[[i]] = simulation(formula, data, init, each, iter)
  }
  return(result)
}
summary.TrialResult =function(TrailResults, testY, caseLabel ="yes", controlLabel = "no"){
  ncolumns = length(TrailResults[[1]])
  countMatrix_case = matrix(data = NA, ncol=ncolumns)
  countMatrix_total = matrix(data = NA, ncol=ncolumns)
  #redundant when same # of cases are enter in each round
  
  for (reselt in TrailResults){
    count_case = c()
    count_total = c()
    
    for (iter_index in reselt){
      count_case = c(count_case, table(testY[iter_index])[caseLabel])
      count_total = c(count_total, table(testY[iter_index])[caseLabel]+table(testY[iter_index])[controlLabel])
    }
    
    countMatrix_case = rbind(countMatrix_case, matrix(count_case, ncol=ncolumns))
    countMatrix_total = rbind(countMatrix_total, matrix(count_total, ncol=ncolumns))
  }
  if (nrow(countMatrix_case)>2){
    countMean_case = apply(countMatrix_case[-1,],2,mean)
    countSD_case = apply(countMatrix_case[-1,],2,sd)
    countMean_total = apply(countMatrix_total[-1,],2,mean)
    countSD_total = apply(countMatrix_total[-1,],2,sd)
  } else {
    countMean_case = countMatrix_case[-1,]
    countSD_case = sd(countMatrix_case[-1,])
    countMean_total = countMatrix_total[-1,]
    countSD_total = sd(countMatrix_total[-1,])
  }

  
  return(list(countMean_case=c(0,countMean_case), c(0,countSD_case=countSD_case),
              countMean_total=c(0,countMean_total), c(0,countSD_total=countSD_total)))
} 
plot.StreamingModel = function(count_case, count_total, testY, titleText=NULL, caseLabel ="yes"){
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  count_total[1] = 1  #for zero division
  plot(cumsum(count_total)/length(testY),cumsum(count_case)/cumsum(count_total),type="l", col="blue", ylab = "% Rate", xlab = "% Customers Contacted", main=titleText, xlim = c(0,1), ylim = c(0,1), asp=1)
  lines(cumsum(count_total)/length(testY),count_case/count_total,col="blue",lty=2)
  lines(cumsum(count_total)/length(testY),
        (cumsum(count_case)/length(testY))/(table(testY)[caseLabel]/length(testY)),col="red")
  lines(cumsum(count_total)/length(testY),(cumsum(count_total)/length(testY)), lty=3)
  legend("right", legend =c("Response Rate So Far","Response Rate In Each Round","% Positive Responses", "Baseline") ,col=c("blue","blue","red", "black"), lty=c(1,2,1,3), cex=0.52, inset=c(-.42,0), box.lty=0)
  par(mar=c(5, 4, 4, 2) + 0.1, xpd=FALSE)
}
pivot = function(countMean_case, countMean_total){
  x = cumsum(countMean_total)/sum(countMean_total)
  y = cumsum(countMean_case)/sum(countMean_total)
  #plot(x,y)
  est_responsesRate = diff(y[1:2])/diff(x[1:2])
  slope = diff(y[2:length(y)])/diff(x[2:length(y)])
  est_slope = (est_responsesRate-y[2:(length(y)-1)])/(1-x[2:(length(x)-1)])
  diff_slope = slope-est_slope
  #plot(diff_slope); abline(h=0, lty=2)
  for (i in 1:length(diff_slope)){
    if (diff_slope[i] < 0.01) {
      return(i+2)     #for i-th slope
    }
  }
  return(length(diff_slope)+1)
}

#Telemarketing
set.seed(1124)
bank_imputed = read.csv("bank-additional-full_imputed.csv", row.name="X")
bank_imputed = bank_imputed[sample(1:dim(bank_imputed)[1],5000),]

TrailResults = numTrials(y ~ ., bank_imputed, n = 30, init=500, each=300)
bank_imputed_y = bank_imputed$y
save(TrailResults, bank_imputed_y,file="TrailResults.RData")
summary = summary.TrialResult(TrailResults, bank_imputed_y)
plot.StreamingModel(summary$countMean_case, summary$countMean_total, bank_imputed_y, "Dynamic Customer Selection\n in real-world simulation")

pivot_point = pivot(summary$countMean_case, summary$countMean_total)
abline(v=sum(summary$countMean_total[1:pivot_point])/sum(summary$countMean_total), lty=3, col="purple")

#other simulation with small data set
#bank_imputed = bank_imputed[sample(1:dim(bank_imputed)[1],5000),]

#Unpredictable Dataset
simdata_label = bank_imputed$y
useless_predictor_generator = function(label){
  ncolumns = sample(c(2:6),1)
  nrows = length(label)
  distr_index = sample(1:2,1)
  if (distr_index == 1) {
    result = rnorm(nrows, mean=0, sd=runif(1, min=1,max=3))
  } else {
    result = runif(nrows, min=0, max=ncolumns)
  }
  result = cut(result, breaks=ncolumns)
  return(result)
}

simdata = data.frame(simdata_label)
for (i in 1:20) {
  temp = useless_predictor_generator(simdata_label)
  nameOfPredictor = paste0(0,i)
  simdata = cbind(simdata, temp)
  colnames(simdata)[i+1] = nameOfPredictor
}

simResults = numTrials(simdata_label ~ ., simdata,
                       n = 30, init=300, each=300)
sim_y = simdata$simdata_label
save(simResults, sim_y, file="simResults.RData")

simSummary = summary.TrialResult(simResults, sim_y)
plot.StreamingModel(simSummary$countMean_case, simSummary$countMean_total, sim_y, "Dynamic Customer Selection\n in unpredictable simulation")

pivot_point = pivot(simSummary$countMean_case, simSummary$countMean_total)
abline(v=sum(simSummary$countMean_total[1:pivot_point])/sum(simSummary$countMean_total), lty=3, col="purple")


#Mixed with unpredictable predictors

combindeData=cbind(simdata[,colnames(simdata) != "simdata_label"], bank_imputed)


combinedResults = numTrials(y ~ ., combindeData, n = 30, init=300, each=300)
combined_y = combindeData$y
save(combinedResults, combined_y, file="combindedResults.RData")

comSummary = summary.TrialResult(combinedResults,combined_y)
plot.StreamingModel(comSummary$countMean_case, comSummary$countMean_total, combined_y, "Dynamic Customer Selection\n in noisy simulation")

pivot_point = pivot(comSummary$countMean_case, comSummary$countMean_total)
abline(v=sum(comSummary$countMean_total[1:pivot_point])/sum(comSummary$countMean_total), lty=3, col="purple")

#influence of # of each

Result = list()
each = c(100,300,500,1000)
for (i in 1:length(each)) {
  #Result[[i]] = numTrials(y ~ ., bank_imputed, n = 5, init=each[i], each=each[i])
  #bank_imputed_y = bank_imputed$y
  Summary = summary.TrialResult(Result[[i]], bank_imputed_y)
  plot.StreamingModel(Summary$countMean_case, Summary$countMean_total, bank_imputed_y, paste0(each[i]," Customers in Each Iteration"))
}
#save(Result, bank_imputed_y, file="resultList_each.RData")

#influence of # of init

Result2 = list()
init = c(100,300,500,1000)
for (i in 1:length(init)) {
  #Result2[[i]] = numTrials(y ~ ., bank_imputed, n = 5, init=init[i], each=100)
  #bank_imputed_y = bank_imputed$y
  Summary = summary.TrialResult(Result2[[i]], bank_imputed_y)
  plot.StreamingModel(Summary$countMean_case, Summary$countMean_total,bank_imputed_y, paste0(init[i]," Customers in First Initial Iteration"))
}
#save(Result2, bank_imputed_y, file="resultList_init.RData")
