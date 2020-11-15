#importing packages
library(dplyr)
library(tm)
library(SnowballC)

# reading companies csv file - beware of encoding
# in session tab menu, you can set working directory to reviews folder 
# please change csv file name if the file is written in wrong syntax
# syntax : your_name_eng + _review.csv
setwd("C:/Users/user/Desktop/glassdoor/reviews/south")
csv1 = read.csv("kihun_review.csv", encoding = "UTF-8")

#write your name in English in between the quotation marks
csv1_name = "kihun"


################################
# --text preprocessing start-- #
################################

preprocessor = function(csv, name){
  
  #pros & cons columns 
  pros = csv$pros
  cons = csv$cons

  #translate text vector data to corpus
  text_corpus_pros <- iconv(pros, "ASCII", "UTF-8", sub="byte")
  text_corpus_pros = Corpus(VectorSource(text_corpus_pros))
  
  text_corpus_cons <- iconv(cons, "ASCII", "UTF-8", sub="byte")
  text_corpus_cons = Corpus(VectorSource(text_corpus_cons))
  
  #all text to lowercase
  #removing numbers, puncuations, stopwords
  corpus_pros = tm_map(text_corpus_pros, content_transformer(tolower))
  corpus_pros = tm_map(corpus_pros, removeNumbers)
  corpus_pros = tm_map(corpus_pros, removePunctuation)  
  corpus_pros = tm_map(corpus_pros, removeWords, stopwords())


  corpus_cons = tm_map(text_corpus_cons, content_transformer(tolower))
  corpus_cons = tm_map(corpus_cons, removeNumbers)
  corpus_cons = tm_map(corpus_cons, removePunctuation)
  corpus_cons = tm_map(corpus_cons, removeWords, stopwords())

  
  #stemming by stemDocument from snowballC
  corpus_pros = tm_map(corpus_pros, stemDocument)
  corpus_cons = tm_map(corpus_cons, stemDocument)
  
  #trimming unnecessary spaces
  corpus_pros = tm_map(corpus_pros, stripWhitespace)
  corpus_cons = tm_map(corpus_cons, stripWhitespace)
  
  #matrixing stems : by tdm? | tf-idf?
  #LDA? - algorithm - stat inference
  tdm_pros = TermDocumentMatrix(corpus_pros)
  tdm_cons = TermDocumentMatrix(corpus_cons)
  
  #removing sparsity
  tdm_pros = removeSparseTerms(tdm_pros, sparse = 0.99)
  mat_pros = as.matrix(tdm_pros)
  
  tdm_cons = removeSparseTerms(tdm_cons, sparse = 0.99)
  mat_cons = as.matrix(tdm_cons)
  
  
  ########################
  # --csv saving start-- #
  ########################
  
  #naming
  name_pros = paste(name, "mat_pros.csv", sep = "_")
  name_cons = paste(name, "mat_cons.csv", sep = "_")
  
  #save new csv file for preprocessed text data
  write.csv(mat_pros, name_pros)
  write.csv(mat_cons, name_cons)
  
}


#run preprocessor for each companies
preprocessor(csv1, csv1_name)


# total 2 csv files will be generated for pros and cons
# send stopwords with quotation marks and comma
# ex) "happy", "great", "ametour"
