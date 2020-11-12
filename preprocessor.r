#importing packages
library(dplyr)
library(tm)
library(SnowballC)

#reading companies csv file - beware of encoding
csv1 = read.csv("all_review.csv", encoding = "UTF-8")
csv1_name = "all"


################################
# --text preprocessing start-- #
################################

preprocessor = function(csv, name){
  
  #drop NA and trimming records to 400
  if (nrow(csv) > 400){
    csv = csv[1:400,]; nrow(csv)
  }
  
  #pros & cons columns 
  pros = csv$pros
  cons = csv$cons
  
  #translate text vector data to corpus
  text_corpus_pros = Corpus(VectorSource(pros))
  text_corpus_cons = Corpus(VectorSource(cons))
  
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
  mat_pros = as.matrix(tdm_pros); mat_pros
  
  tdm_cons = removeSparseTerms(tdm_cons, sparse = 0.98)
  mat_cons = as.matrix(tdm_cons); mat_cons
  
  #excel column merging
  
  
  #naming
  name_pros = paste(name, "mat_pros.csv", sep = "_")
  name_cons = paste(name, "mat_cons.csv", sep = "_")
  
  #save new csv file for preprocessed text data
  write.csv(mat_pros, name_pros)
  write.csv(mat_cons, name_cons)
  
  
}


preprocessor(csv1, csv1_name)

