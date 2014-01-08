library(tm)
library(e1071)
source("utils.r")

preprocessText <- function(text){
	text <- PlainTextDocument(text)
	text <-str_replace_all(text, "[^[:alpha:]]",' ')
	#----------------------------------------
	text <-tolower(text)
	text <-removeWords(text, stopwords("english"))
	text <-stripWhitespace(text)
	text <-stemDocument(text, "english")
	return(text)
}


apply_model <- function(text){
	
	if(!exists("relevant_attributes")){
		load("relevant_attributes.rda")
	}
	if(!exists("model")){
		load("model_svm.rda")
	}
	
	relevant_attributes_unescaped <- sapply(relevant_attributes, function(x) substr(x, 1, nchar(x)-5), USE.NAMES = FALSE)
	#preprocess text
	corpus <- Corpus(VectorSource(text))
	corpus.p <- utils.preprocessDocumentCollection(corpus)
	dtm.mx <- DocumentTermMatrix(corpus.p, control = list(dictionary=Dictionary(relevant_attributes_unescaped)))
	colnames(dtm.mx) <- utils.escapeColnames(dtm.mx)
	dtm <- as.data.frame(as.matrix(dtm.mx))
	prediction <- predict(model, dtm, type="class")
	return(prediction)
}

# con.eval("setwd('"+Rails.root.to_s+"/r'); source('apply_model.r'); apply_model('Hello my love')").to_ruby