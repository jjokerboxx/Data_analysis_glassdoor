#importing packages
library(dplyr)
library(tm)
library(SnowballC)

#reading companies csv file - beware of encoding
csv = read.csv("southwest_review.csv", encoding = "UTF-8")



################################
# --text preprocessing start-- #
################################

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

# new branch 1.
#before preprocessed tokenized words
token_corpus_pros = Boost_tokenizer(text_corpus_pros); token_corpus_pros
token_corpus_cons = Boost_tokenizer(text_corpus_cons); token_corpus_cons

# new branch 2.
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

#check the results
inspect(corpus_pros)
inspect(corpus_cons)

#problem - any - company!!

#matrixing stems : by tdm? | tf-idf?
#LDA? - algorithm - stat inference
tdm_pros = TermDocumentMatrix(corpus_pros)
tdm_cons = TermDocumentMatrix(corpus_cons)

#removing sparsity
tdm_pros = removeSparseTerms(tdm_pros, sparse = 0.98)
mat_pros = as.matrix(tdm_pros); mat_pros

tdm_cons = removeSparseTerms(tdm_cons, sparse = 0.98)
mat_cons = as.matrix(tdm_cons); mat_cons



########################
# --clustering start-- #
########################

#Hierarchical clustering - standardization by scale func
distance = dist(scale(mat_pros))

#Hierarchical clustering - using "ward.D" method
hc = hclust(distance, method = "ward.D")

#check the results in plot
plot(hc)



###########################
# --visualization start-- #
###########################

#plotting
freq_pros = rowSums(mat_pros)
freq_cons = rowSums(mat_cons)

#showing data more than 50
freq_pros = subset(freq_pros, freq_pros>=50)
barplot(freq_pros, las = 2, col = rainbow(7))

freq_cons = subset(freq_cons, freq_cons>=50)
barplot(freq_cons, las = 2, col = rainbow(7))

#sorting
v_pros <- sort(rowSums(mat_pros),decreasing=TRUE) 
d_pros <- data.frame(word = names(v_pros),freq=v_pros)

v_cons <- sort(rowSums(mat_cons),decreasing=TRUE) 
d_cons <- data.frame(word = names(v_cons),freq=v_cons)

# top 10
head(d_pros, 10)
head(d_cons, 10)



#####################################
# --save and export results start-- #
#####################################

#save new csv file for preprocessed text data
write.csv(mat_pros, 'mat_pros.csv')
write.csv(corpus_pros, 'corpus_pros.csv')




