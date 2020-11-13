#importing packages
library(dplyr)
library(tm)
library(SnowballC)

#reading companies csv file - beware of encoding
csv = read.csv("all_review.csv", encoding = "UTF-8")

#pros & cons columns 
pros = csv$pros

#all text to lowercase
#removing numbers, puncuations, stopwords
text_corpus_pros <- iconv(pros, "ASCII", "UTF-8", sub="byte")
text_corpus_pros = Corpus(VectorSource(text_corpus_pros))
corpus_pros = tm_map(text_corpus_pros, content_transformer(tolower))
corpus_pros = tm_map(corpus_pros, removeNumbers)
corpus_pros = tm_map(corpus_pros, removePunctuation)  
corpus_pros = tm_map(corpus_pros, removeWords, stopwords())

#stemming by stemDocument from snowballC
corpus_pros = tm_map(corpus_pros, stemDocument)

#trimming unnecessary spaces
corpus_pros = tm_map(corpus_pros, stripWhitespace)


#matrixing stems : by tdm? | tf-idf?
#LDA? - algorithm - stat inference
tdm_pros = TermDocumentMatrix(corpus_pros)

#removing sparsity
tdm_pros = removeSparseTerms(tdm_pros, sparse = 0.98)
mat_pros = as.matrix(tdm_pros)


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
freq_pros = rowSums(mat_pros, na.rm=TRUE)

#showing data more than 50
freq_pros = subset(freq_pros, freq_pros>=50)
barplot(freq_pros, las = 2, col = rainbow(7))

#sorting
v_pros <- sort(rowSums(mat_pros),decreasing=TRUE) 
d_pros <- data.frame(word = names(v_pros),freq=v_pros)

# top 10
head(d_pros, 20)





