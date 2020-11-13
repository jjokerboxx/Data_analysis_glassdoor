#importing packages
library(dplyr)
library(tm)
library(SnowballC)


#reading companies csv file - beware of encoding
#please change csv file name if the file is written in wrong syntax
# syntax : corpshortname + _review.csv
csv1 = read.csv("corp_review.csv", encoding = "UTF-8")

#write corpshortname in between the quotation marks
csv1_name = "name"

#same
csv2 = read.csv("corp_review.csv", encoding = "UTF-8")
csv2_name = "name"
csv3 = read.csv("corp_review.csv", encoding = "UTF-8")
csv3_name = "name"
csv4 = read.csv("corp_review.csv", encoding = "UTF-8")
csv4_name = "name"
csv5 = read.csv("corp_review.csv", encoding = "UTF-8")
csv5_name = "name"
csv6 = read.csv("corp_review.csv", encoding = "UTF-8")
csv6_name = "name"


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
  tdm_pros = removeSparseTerms(tdm_pros, sparse = 0.98)
  mat_pros = as.matrix(tdm_pros)
  
  tdm_cons = removeSparseTerms(tdm_cons, sparse = 0.98)
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
preprocessor(csv2, csv2_name)
preprocessor(csv3, csv3_name)
preprocessor(csv4, csv4_name)
preprocessor(csv5, csv5_name)
preprocessor(csv6, csv6_name)

#total 12csv files will be generated each 6 files for pros and cons
#in session tab menu, you can set working directory to reviews folder 