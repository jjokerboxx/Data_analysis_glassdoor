#importing packages
library(dplyr)
library(tm)
library(SnowballC)


#reading companies csv file - beware of encoding
csv = read.csv("all_review.csv", encoding = "UTF-8")


#pros & cons columns 
pros = csv$pros
cons = csv$cons


#translate text vector data to corpus


#all text to lowercase
#removing numbers, puncuations, stopwords
text_corpus_pros <- iconv(pros, "ASCII", "UTF-8", sub="byte")
text_corpus_pros = Corpus(VectorSource(text_corpus_pros))
corpus_pros = tm_map(text_corpus_pros, content_transformer(tolower))
corpus_pros = tm_map(corpus_pros, removeNumbers)
corpus_pros = tm_map(corpus_pros, removePunctuation)  
corpus_pros = tm_map(corpus_pros, removeWords, stopwords())


text_corpus_cons <- iconv(cons, "ASCII", "UTF-8", sub="byte")
text_corpus_cons = Corpus(VectorSource(text_corpus_cons))
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