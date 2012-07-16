# install randomForest algorithm. You only need to do this once per computer.
install.packages("randomForest")

# load randomForest package in memory. You only need to do this once per unsaved session.
library(randomForest)

# accuracy calculation function
getBinAccuracy<-function(prediction, testset){
  
  
  TP<-0 # true positive predictions
  TN<-0 # true negative predictions
  FP<-0 # false positive predictions
  FN<-0 # false negative predictions
  P<-0 # all actual positives
  N<-0 # all actual negatives
  FPr<-0 # False Positive rate: FP/N (the percent of non-payers we guess to be payers)
  FNr<-0 # False Negtiave rate: FN/P (the percent of payers we guess to be non-payers)
  
  
  for(i in 1:length(prediction)){
    
    pred<-prediction[i]
    truth<-testset[i]
    
    if((pred=="1")&(truth=="1")){
      
      TP=TP+1
      P=P+1
    }
    
    if((pred==0)&(truth==0)){
      
      TN=TN+1
      N=N+1
    }
    
    if((pred==1)&(truth==0)){
      
          
      N=N+1
      FP=FP+1
      
    }
    
    if((pred==0)&(truth==1)){
      
      P=P+1
      FN=FN+1
    }
  }
  
  if(FP==0){
    FPr=0
  }else{
    FPr=FP/N
  }
  
  if(FN==0){
    FNr=0
  }else{
    FNr=FN/P
  }
  
  return(c((TP+TN)/(P+N),TP, TN, FP, FPr, FN, FNr, P, N)) # the function c() simply forms a list
}



# load full SMOTE processed training set from csv
trainSM<-read.csv("trainSMOTE.csv")


# remove extra column
colnames(trainSM) # list column names: it has an extra column called "X"
trainSM<-trainSM[,-1] # everything except the first column
colnames(trainSM) # the extra column is gone


# separate response variables from feature variables in training dataset
# trF=training features
# trR= training responses

trF<-trainSM[,-1] 
trR<-as.factor(trainSM[,1]) #the randomForest implementation prefers factor datatypes.

# Load saved test set. This function call puts the object "test" in memory so it can be used here.
# Using .rda files is an alternative to using csv's and can be used to save any kinds of R objects
# including predictors.

load(file="test.rda")
colnames(test) # no extra column!

# separate test features from test responses
tsF<-test[,2:length(test[1,])]
tsR<-as.factor(test[,1])


# Create the predictor. Type "?randomForest" for help
fit<-randomForest(trR~real_sum+real_max+real_min+genNum+refNum+act2Count+act3Count+act4Count+act6Count+act7Count+act27Count+maxlevel+last, data=data.frame(trF), importance=TRUE)


# This alternate method works but produces a different less descriptive fit$importance variable (!?)
fit<-randomForest(trR~., data=trF)


# apply the predictor to the test feature set (this is also what we would use on a live prediction)
# Type "?predict.randomForest" for help
guess<-predict(fit, data.frame(tsF))

# get accuracy of prediction
acc<-getBinAccuracy(guess, tsR)
format(acc, scientific=F) # Remove scientific formating. When the result isn't assigned it is displayed.

# get variable importance information
imp<-fit$importance

# code to write this data with importance value in decreasing order and save it as csv
payImp<-imp[,2] # only column 2
nopayImp<-imp[,1] # only column 1
names<-row.names(imp) # list row names

payImp<-data.frame(names, payImp) # data frame can hold different data types, a matrix must have only 1 type
payImp<-payImp[order(payImp[,2], decreasing=T),] # select the rows in the order of the order function
write.csv(payImp, file="payImp.csv") # write csv

nopayImp<-data.frame(names, nopayImp)
nopayImp<-nopayImp[order(nopayImp[,2], decreasing=T),]
write.csv(nopayImp, file="nopayImp.csv")

