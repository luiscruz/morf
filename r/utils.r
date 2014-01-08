library(SnowballC) # Stemming
library(stringr) #TODO

#-------------- Preprocess Corpus --------------#

utils.preprocessDocumentCollection <- function(documentCollection){
	documentCollection <-tm_map(documentCollection, PlainTextDocument)
	#-----------Remove punctuation-------
	#documentCollection <-tm_map(documentCollection, removePunctuation)
	#documentCollection <-tm_map(documentCollection, removeNumbers)
	#Alternative to put a space in the place of punctuation (it also removes weird characters)
	documentCollection <-tm_map(documentCollection, str_replace_all, "[^[:alpha:]]",' ')
	#----------------------------------------
	documentCollection <-tm_map(documentCollection, tolower)
	documentCollection <-tm_map(documentCollection, removeWords, stopwords("english"))
	documentCollection <-tm_map(documentCollection, stripWhitespace)
	documentCollection <-tm_map(documentCollection, stemDocument, "english")
	return(documentCollection)
}

utils.escapeColnames <- function(dataset){
	sapply(colnames(dataset), function(name) paste0(name,".attr"), USE.NAMES = FALSE)
}

#-------------- Modeling methods --------------#

utils.generateFormula <- function(labelClass, attributes){
	decision_tree.formula.attributes <- paste(attributes, collapse = '+')
decision_tree.formula <- as.formula(paste(labelClass,"~", decision_tree.formula.attributes, collapse=''))
}

utils.applyModel <- function(model, test, realValues = NULL){
	prediction <- predict(model, test, type="class")
	if(!is.null(realValues)){
		confusionMatrix <- table(realValues, prediction)
		utils.confusionMatrixEvaluation(confusionMatrix)
	}
	return(prediction)
}

#-------------- Feature selection --------------#

utils.find.info.terms <- function(dtm.train, min.info){
		
		# the las column is the label attribute
		index.of.label.attribute <- ncol(dtm.train)
		number.of.attributes <- ncol(dtm.train)-1
		number.of.documents <- nrow(dtm.train)
		number.of.info.terms <- 0
		info.term.indexes <- vector()
#		attribute.names <- colnames(dtm.train)
		
		default.info<-info(table(dtm.train[,index.of.label.attribute]))
		
		#process all attributes
		for(attribute in 1: number.of.attributes){
			if (sum(dtm.train[,attribute])>0){
				number.of.different.values <- length(table(dtm.train[,attribute]))
				class.cardinality.for.attribute <- table(dtm.train[,attribute], dtm.train[,index.of.label.attribute])
				attribute.info <- 0
				for(attribute.value.index in 1: number.of.different.values){
					attribute.value.weight <- sum(class.cardinality.for.attribute[attribute.value.index,])/number.of.documents
					attribute.info <- attribute.info+attribute.value.weight*info(class.cardinality.for.attribute[attribute.value.index,])
				}
				info.gain <- default.info - attribute.info 
				
				#if info gain of attribute is above the treshold it will be used to create the model
				if(info.gain > min.info){
					info.term.indexes <- append(info.term.indexes, attribute)
				}
			}
		}
		return(info.term.indexes)
}

info <- function(x){
	inf <- 0
	sumx <- sum(x)
	for (i in x) {
		pi <- i/sumx
		infi <- (pi)*log2(pi)
		if (is.na(infi)){
			infi <- 0
		}
		inf <-inf - infi
	}
	return(inf)
}

#-------------- Confusion Matrix Evaluation methods --------------#

# Function to create the Confusion Matrix
utils.calcConfusionValues <- function(confusionMatrix){
	list(tp=confusionMatrix[1,1],
		 fp=confusionMatrix[2,1],
		 fn=confusionMatrix[1,2],
		 tn=confusionMatrix[2,2]
	);
}

# Function to calculate the model Precision
utils.precision <- function(confusionMatrix){
	values = utils.calcConfusionValues(confusionMatrix)
	values$tp/(values$tp + values$fp)
}
# Function to calculate the model Recall
utils.recall <- function(confusionMatrix){
	values = utils.calcConfusionValues(confusionMatrix)
	values$tp/(values$tp + values$fn)
}
# Function to calculate the model ErrorRate
utils.errorRate <- function(confusionMatrix){
	values = utils.calcConfusionValues(confusionMatrix)
	(values$fp + values$fn)/(sum(confusionMatrix))
}
# Function to calculate the model F1 score
utils.f1score <- function(confusionMatrix){
  rcall<- utils.recall(confusionMatrix)
  prc<- utils.precision(confusionMatrix)
  
  f1= (2*prc*rcall) /(prc+rcall)
}
# Function to print the model confusionMatrix details
utils.confusionMatrixEvaluation <- function(confusionMatrix){
	print(confusionMatrix)
	errorRate = utils.errorRate(confusionMatrix)
	recall= utils.recall(confusionMatrix)
	precision = utils.precision(confusionMatrix)
	f1score= utils.f1score(confusionMatrix)
	print(sprintf("Error Rate = %f", errorRate))
	print(sprintf("Recall = %f", recall))
	print(sprintf("Precision = %f", precision))
	print(sprintf("F1 Score = %f", f1score))
}

#-------------- Cross Validation Evaluation methods --------------#

# Function to create the blocks, K = number of block
utils.buildblocks<-function(dtm, k){
n <- nrow(dtm)
size <- n%/%k
set.seed(5)
alea <- runif(n)
rang <- rank(alea)
bloc <- (rang-1)%/%size + 1
bloc <- as.factor(bloc)
#print(summary(bloc))
}

#Decision tree model cross validation
utils.dt_cv <- function(clas, dtm, K) {
  bloc<-utils.buildblocks(dtm, K)
  all.err <- numeric(0)
  all.recall<- numeric(0)
  all.precision<- numeric(0)
  all.f1 <- numeric(0)
for (k in 1:K) {
  dt <- rpart(clas, dtm[bloc!=k,], method = "class")
  pred <- predict(dt, dtm[bloc==k,], type="class")
  conf.mx.dt <- table(dtm$label.attribute.attr[bloc==k], pred)
  
  all.err <- rbind(all.err,utils.errorRate(conf.mx.dt))
  all.recall<- rbind(all.recall,utils.recall(conf.mx.dt))
  all.precision<- rbind(all.precision,utils.precision(conf.mx.dt))
  all.f1 <- rbind(all.f1,utils.f1score(conf.mx.dt))
  }
  print(sprintf("Error Rate = %f",mean(all.err)))
  print(sprintf("Recall =%f", mean(all.recall)))
  print(sprintf("Precision =%f",mean(all.precision)))
  print(sprintf("F1 Score = %f",mean(all.f1)))
}

#Neural Network (NN) model cross validation
utils.nnet_cv <- function(clas, dtm, K) {
  bloc<-utils.buildblocks(dtm, K)
  all.err <- numeric(0)
  all.recall<- numeric(0)
  all.precision<- numeric(0)
  all.f1 <- numeric(0)
  for (k in 1:K) {
    model.nnet.cv <- nnet(clas, dtm[bloc!=k,], size=2, rang=0.1,decay=5e-4, maxit=200)
    pred <- predict(model.nnet.cv, dtm[bloc==k,], type="class")
    conf.mx.nnet <- table(dtm$label.attribute.attr[bloc==k], pred)
    
    all.err <- rbind(all.err,utils.errorRate(conf.mx.nnet))
    all.recall<- rbind(all.recall,utils.recall(conf.mx.nnet))
    all.precision<- rbind(all.precision,utils.precision(conf.mx.nnet))
    all.f1 <- rbind(all.f1,utils.f1score(conf.mx.nnet))
  }
  print(sprintf("Error Rate = %f",mean(all.err)))
  print(sprintf("Recall =%f", mean(all.recall)))
  print(sprintf("Precision =%f",mean(all.precision)))
  print(sprintf("F1 Score = %f",mean(all.f1)))
}

#K Nearest Neighbors (k-NN) model cross validation
utils.knn_cv <- function(clas, dtm, K) {
  bloc<-utils.buildblocks(dtm, K)
  all.err <- numeric(0)
  all.recall<- numeric(0)
  all.precision<- numeric(0)
  all.f1 <- numeric(0)
  for (k in 1:K) {
        
    model.knn.cv<-knn(dtm[bloc!=k, relevant_attributes_indexes], dtm[bloc==k, relevant_attributes_indexes], dtm[bloc!=k,last.col], k=1)
    conf.mx.knn <- table(dtm$label.attribute.attr[bloc==k], model.knn.cv)
    
    all.err <- rbind(all.err,utils.errorRate(conf.mx.knn))
    all.recall<- rbind(all.recall,utils.recall(conf.mx.knn))
    all.precision<- rbind(all.precision,utils.precision(conf.mx.knn))
    all.f1 <- rbind(all.f1,utils.f1score(conf.mx.knn))
  }
  print(sprintf("Error Rate = %f",mean(all.err)))
  print(sprintf("Recall =%f", mean(all.recall)))
  print(sprintf("Precision =%f",mean(all.precision)))
  print(sprintf("F1 Score = %f",mean(all.f1)))
}

# SVM model cross validation
utils.svm_cv <- function(clas, dtm, K) {
  bloc<-utils.buildblocks(dtm, K)
  all.err <- numeric(0)
  all.recall<- numeric(0)
  all.precision<- numeric(0)
  all.f1 <- numeric(0)
  for (k in 1:K) {
    model.svm.cv <- svm(clas, dtm[bloc!=k,], cost=100)
    pred <- predict(model.svm.cv, dtm[bloc==k,], type="class")
    conf.mx.svm <- table(dtm$label.attribute.attr[bloc==k], pred)
    
    all.err <- rbind(all.err,utils.errorRate(conf.mx.svm))
    all.recall<- rbind(all.recall,utils.recall(conf.mx.svm))
    all.precision<- rbind(all.precision,utils.precision(conf.mx.svm))
    all.f1 <- rbind(all.f1,utils.f1score(conf.mx.svm))
  }
  print(sprintf("Error Rate = %f",mean(all.err)))
  print(sprintf("Recall =%f", mean(all.recall)))
  print(sprintf("Precision =%f",mean(all.precision)))
  print(sprintf("F1 Score = %f",mean(all.f1)))
}

#Naive Bayes (NB) model cross validation
utils.NB_cv <- function(clas, dtm, K) {
  bloc<-utils.buildblocks(dtm, K)
  all.err <- numeric(0)
  all.recall<- numeric(0)
  all.precision<- numeric(0)
  all.f1 <- numeric(0)
  for (k in 1:K) {
    model.nb.cv <- naiveBayes(clas, dtm[bloc!=k,])
    pred <- predict(model.nb.cv, dtm[bloc==k,], type="class")
    conf.mx.nb <- table(dtm$label.attribute.attr[bloc==k], pred)
    
    all.err <- rbind(all.err,utils.errorRate(conf.mx.nb))
    all.recall<- rbind(all.recall,utils.recall(conf.mx.nb))
    all.precision<- rbind(all.precision,utils.precision(conf.mx.nb))
    all.f1 <- rbind(all.f1,utils.f1score(conf.mx.nb))
  }
  print(sprintf("Error Rate = %f",mean(all.err)))
  print(sprintf("Recall =%f", mean(all.recall)))
  print(sprintf("Precision =%f",mean(all.precision)))
  print(sprintf("F1 Score = %f",mean(all.f1)))
}