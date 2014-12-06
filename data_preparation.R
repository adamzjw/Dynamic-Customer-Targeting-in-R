#load data
fname = "bank-additional-full.csv"
bank <- read.csv(fname, sep=";", na.strings="unknown")
bank=bank[,colnames(bank)!="default"]
bank=bank[,colnames(bank)!="duration"]

#data shape
str(bank)

#missing value $ imputation
summary(bank)

#library(Amelia)
#missmap(bank, main="Missings Map", col=c("yellow", "black"), legend=FALSE)

#excluding y
y = bank$y

require(missForest)          #cannot deal with incremental customer
bank_imputed = missForest(bank)
bank_imputed = bank_imputed$ximp

bank_imputed = cbind(bank_imputed, y)

fname_new = paste0(strsplit(fname, split = ".", fixed=T)[[1]][1],"_imputed.",strsplit(fname, split = ".", fixed=T)[[1]][2])
write.csv(bank_imputed, file=fname_new)

#partition
n = dim(bank_imputed)[1]
set.seed(1118)
m = sample(1:n)
train_index = m[1:ceiling(0.6*n)]
cv_index =  m[(ceiling(0.6*n)+1):ceiling(0.8*n)]
test_index =  m[(ceiling(0.8*n)+1):n]

#creating sets
training = bank_imputed[train_index,]
cv = bank_imputed[cv_index,]
test = bank_imputed[test_index,]

#exploratory data analysis
summary(training)

#save_data
save(training, cv, test, file="data.RData")
