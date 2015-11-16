snopes.model.preprocess<-function(train.data,train.label,model){
  
  ## NOTE: Train.data is a full matrix instead of a sparse representation
  ## Datasets
  ## train.data<-read.csv("C:/Users/nemecys/Desktop/Study/CSCE 689/Project/Model building/train.data.sparse.csv")
  ## train.label<-read.csv("C:/Users/nemecys/Desktop/Study/CSCE 689/Project/Model building/label_training.csv")
  
  ## Loading libraries
  print("Loading Libraries")
  library(tm)
  library(RTextTools)

  ## Converting train.data to a Term document matrix type
  print("Converting to TDM format")
  Documents<-nrow(train.data)
  train.data[is.na(train.data)]<-0
  colnames(train.data)<-seq(1,ncol(train.data),1)
  train.data <- as.DocumentTermMatrix(train.data,weighting=weightTf)
  
  ## Create a training dataset container and classification model
  print("Creating containers")
  train.container <- create_container(train.data,train.label[,1],trainSize=1:Documents, virgin=FALSE)
  
  print("Training the model")
  model <- train_model(train.container,model,l1_regularizer = 0, l2_regularizer = 1)
  test.container<-create_container(train.data,labels=rep(0,Documents),testSize=1:Documents, virgin=FALSE)
  
  ## Dummy model accuracy
  print("Dummy accuracy")
  print(64.44083)
  
  ## Assessing results on training dataset
  print("Training accuracy")
  results <- classify_model(test.container, model)
  compare<-cbind(train.label,results[,1])
  compare[,2]<-as.integer(as.character(compare[,2]))
  compare[,3]<-abs(compare[,1]-compare[,2])
  names(compare)<-c("Original","Predicted","Decision")
  train.accuracy<-(nrow(compare[compare[,3]==0,])*100/nrow(compare))
  
  ## Output Accuracy
  train.accuracy
  
}
  