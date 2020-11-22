#importing packages
library(dplyr)
library(tm)
library(SnowballC)
library(cluster)
library(topicmodels)
library(lda)
library(arules)
library(chinese.misc)

#setting working directory where csv files are located
#setwd("C:/Users/user/Desktop/glassdoor/reviews")
setwd("/Users/igihun/Desktop/glassdoor/reviews")
#reading companies csv file - beware of encoding
csv = read.csv("all_review.csv", encoding = "UTF-8")

#extracting review data variables
pros = csv$pros
cons = csv$cons

#
#
#

##################################
#### -- Preprocessing start-- ####
##################################

#all text to lowercase
#removing numbers, puncuations, stopwords
text_corpus_pros <- iconv(pros, "ASCII", "UTF-8", sub="byte")
text_corpus_pros = Corpus(VectorSource(text_corpus_pros))
corpus_pros = tm_map(text_corpus_pros, content_transformer(tolower))
corpus_pros = tm_map(corpus_pros, removeNumbers)
corpus_pros = tm_map(corpus_pros, removePunctuation)  
corpus_pros = tm_map(corpus_pros, removeWords, stopwords("english"))

text_corpus_cons <- iconv(cons, "ASCII", "UTF-8", sub="byte")
text_corpus_cons = Corpus(VectorSource(text_corpus_cons))
corpus_cons = tm_map(text_corpus_cons, content_transformer(tolower))
corpus_cons = tm_map(corpus_cons, removeNumbers)
corpus_cons = tm_map(corpus_cons, removePunctuation)
corpus_cons = tm_map(corpus_cons, removeWords, stopwords("english"))

#stemming by stemDocument from snowballC
corpus_pros = tm_map(corpus_pros, stemDocument)
corpus_cons = tm_map(corpus_cons, stemDocument)

#manually adding stopwords
corpus_pros = tm_map(corpus_pros, removeWords, c("vipkid", "much", "just", "viasat", 
                                                 "cool", "your", "also", "never", "ever", 
                                                 "ive", "great", "decent", "amaz", "awesom", 
                                                 "will", "incred", "nice", "can", "dont", 
                                                 "better", "bad", "often", "way", "find", 
                                                 "llnl", "realli", "bit", "say", "hubspot", 
                                                 "krono", "still", "thing", "may", "sfg", "sammon", 
                                                 "half", "plenti", "given", "industry", "don't",
                                                 "nvidia", "put", "month", "cut", "TRUE", "need",
                                                 "wonder", "clear", "build", "done", "anderson",
                                                 "import", "pto", "within", "cant", "actual", "that",
                                                 "come", "esop", "even", "fantast", "look",
                                                 "corpor", "there", "everi", "rally", "mani",
                                                 "busi", "forward", "direct", "part", "use",
                                                 "anyth", "sinc", "howev", "none", "see", "number",
                                                 "keep", "around", "volunt", "last", "give", "yet", 
                                                 "work", "good", "compani", "lot", "best", "make",
                                                 "get", "like", "well", "smart", "excel", "day", "truli",
                                                 "alway", "perk", "hubspot", "esop", "friday", "just",
                                                 "long", "etc", "abl", "big", "one", "driven", "top", "meet",
                                                 "tri", "take", "first", "made", "start", "set", "week", "excit",
                                                 "real", "want", "easi", "someth", "larg", "year",
                                                 
                                                 "peopl", "benefit", "cultur", "employe"))
#need update!!!
corpus_cons = tm_map(corpus_cons, removeWords, c("sometim", "lack", "also", "depend", "make", 
                                                 "alway", "good", "take", "can", "none", 
                                                 "bain", "everyon", "truli", "much", "amaz", 
                                                 "awesome", "way", "provid", "year", "will", 
                                                 "around", "extrem", "fantast", "big", "alway", 
                                                 "can", "just", "docusign", "nice", "use", 
                                                 "north", "seem", "due", "still", "there", "hubspot",
                                                 "con", "may", "cant", "doesnt", "actual",
                                                 "across", "that", "bit", "con", "without",
                                                 "org", "nvidia", "sammon", "busi", "poor", "back",
                                                 "come", "even", "look", "corpor", "always",
                                                 "less", "part", "anyth", "sinc", "howev", "noth",
                                                 "see", "number", "keep", "within", "last", "certain",
                                                 "better", "yet", "best", "work", "good", "compani", "lot",
                                                 "get", "like", "well", "smart", "excel", "day", "truli",
                                                 "alway", "perk", "one", "long", "year"))


#trimming unnecessary spaces
corpus_pros = tm_map(corpus_pros, stripWhitespace)
corpus_cons = tm_map(corpus_cons, stripWhitespace)


#matrixing stems : TDM | DTM
#TDM for Clustering
tdm_pros = TermDocumentMatrix(corpus_pros)
tdm_cons = TermDocumentMatrix(corpus_cons)

#DTM for TF-IDF
dtm_pros = DocumentTermMatrix(corpus_pros)
dtm_cons = DocumentTermMatrix(corpus_cons)

#removing sparsity
tdm_pros = removeSparseTerms(tdm_pros, sparse = 0.99)
mat_pros = as.matrix(tdm_pros)
nrow(mat_pros)

tdm_cons = removeSparseTerms(tdm_cons, sparse = 0.99)
mat_cons = as.matrix(tdm_cons)

dtm_pros = removeSparseTerms(dtm_pros, spare = 0.99)
mat_dtm_pros = as.matrix(dtm_pros)

dtm_cons = removeSparseTerms(dtm_cons, spare = 0.99)
mat_dtm_cons = as.matrix(dtm_cons)

#TF-IDF
tf_idf_pros = DocumentTermMatrix(corpus_pros, control=list(weighting=weightTfIdf))
mat_tf_idf_pros = as.matrix(tf_idf_pros)
colnames(mat_tf_idf_pros)
inspect(tf_idf_pros[1:10,1:5])


tf_idf_cons = DocumentTermMatrix(corpus_cons, control=list(weighting=weightTfIdf))
mat_tf_idf_cons = as.matrix(tf_idf_cons)



#
#
#

#######################################
#### -- LDA topic modeling start-- ####
#######################################

# LDA code for building topics
dtm_pros_LDA = as.DocumentTermMatrix(tdm_pros)
dtm_pros = as.matrix(dtm_pros_LDA)

set.seed(1234)

ldaform = dtm2ldaformat(dtm_pros_LDA, omit_empty=T)

result.lda = lda.collapsed.gibbs.sampler(documents = ldaform$documents,
                                         K = 5,
                                         vocab = ldaform$vocab,
                                         num.iterations = 5000,  
                                         burnin = 1000, 
                                         alpha = 0.01,  
                                         eta = 0.01 )
# basic LDA attr info 
#attributes(result.lda)
#dim(result.lda$topics)    
#result.lda$topics
#dim(result.lda$document_sums)

# 5 topics
top.topic.words(result.lda$topics)
# sum of each topic
result.lda$topic_sums 

# examples of topic modeling 
result.lda$document_sums[,1] 


#
#
#

############################################
#### --Term association anlysis start-- ####
############################################

#findAssocs function to show certain association with terms
findAssocs(tdm_pros, "pay", 0.1)


pros_term = rownames(mat_pros)
assocs = findAssocs(tdm_pros, pros_term, 0.1)

test = findAssocs(tdm_pros, "allow", 0.1)
attr_assoc = test[[1]]; attr_assoc
elem = attr_assoc[8]
length(attr_assoc)
names(elem)
for (i in 1:length(attr_assoc)) {
  name = names(attr_assoc[i])
  print(name)
}

#new matrix for assoc-apriori test
new_mat_apriori = matrix(data = 0, nrow = nrow(mat_pros), ncol = nrow(mat_pros))
rownames(new_mat_apriori) = rownames(tdm_pros)
colnames(new_mat_apriori) = rownames(tdm_pros)

for (i in pros_term) {
  term_assoc = findAssocs(tdm_pros, i, 0.15)
  attrs_assoc = term_assoc[[1]]
  for (j in 1:length(attrs_assoc)) {
    name = names(attrs_assoc[j])
    #print(name)
    new_mat_apriori[i,name] = 1
  }
}
new_mat_apriori[1:10,1:10]
nrow(new_mat_apriori)

rules2 = apriori(new_mat_apriori, parameter = list(support = 0.01, confidence = 0.20, minlen = 2))
inspect(sort(rules2))

#Apriori algorithm to find term relation
#https://rfriend.tistory.com/193
mat_dtm_pros[mat_dtm_pros>1] = 1
rownames(mat_pros)
rules1 = apriori(mat_dtm_pros, parameter = list(support = 0.01, confidence = 0.20, minlen = 2))

inspect(sort(rules1))


# word_cor function to show correlation with terms
# https://rdrr.io/cran/chinese.misc/man/word_cor.html
# ex) word_cor(tdm_pros, wordvector, type = t, method = "pearson", min = 0.1, p = 0.4)
words = row.names(tdm_pros)
word_cor(tdm_pros, words, type = "tdm", method = "pearson", p = 0.1, min = 0.1)

#
#
#

#####################################
#### --Sentiment anlysis start-- ####
#####################################

# 
#
#


###########################################
#### -- Hierarchicalclustering start-- ####
###########################################


#Hierarchical clustering - standardization by scale func 
distance = dist(scale(mat_pros), method = "euclidean") # euclidean | minkowski

#Hierarchical clustering - using "ward.D" method
hc = hclust(distance, method = "ward.D"); plot(hc) # visualization #

#visualization of association
rect.hclust(hc, k = 5) # visualization of hclust with red box#

#check the cluster of documents in console 
hc_cut = cutree(hc, k = 5); hc_cut

#check the terms
hc$labels

#check the results in plot
plot(hc) # visualization of hclusering#

#
#
#

#####################################
#### --K-mean clustering start-- ####
#####################################

nrow(mat_pros) #177

#k-mean clustering
#mat_km = t(mat_pros)
set.seed(1234)
#mat_pros_stand = scale(mat_pros)
mat_km = kmeans(mat_pros, 8)

mat_km$size

#code from course material for elbow of k-mean clustering
wss <- 1:5
for(i in 1:5) {wss[i] <- sum(kmeans(mat_pros,i)$withinss)}
plot(1:5, wss[1:5], type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares") # visualization of elbow #
wss

rowSums(mat_pros) # frequencies of each term in row sums
# if we make DTM - and select some terms for cluster -> rowsums will show the frequency of the clusters
# later, regression can be applied

plot(mat_km$cluster) # visualization of k-mean cluster#

#
#
#

#############################################
#### --Further cluster | model analysis--####
#############################################

#from course material
#build manual cluster to further analysis | model
cluster1 <- dtm_pros[,c("remot", "sale", "stock", "discount")]; head(cluster1)


# code from course material #### On the Work! ##################################################################################################################################
# Sums #
C1_Sum <- rowSums(cluster1)
C2_Sum <- rowSums(cluster2)
C3_Sum <- rowSums(cluster3)

# Create a Score table #
Score <- matrix(data=0 , n_desc,3);
Score[,1] <- as.matrix(C1_Sum)
Score[,2] <- as.matrix(C2_Sum)
Score[,3] <- as.matrix(C3_Sum)

# Name the Columns/Clusters #
colnames(Score) <- c("Cluster1", "Cluster2", "Cluster3")
head(Score)

## Add a Score matrix to the original Data ##
csv_new <- cbind(csv, Score)
str(csv)

## Run a Regression ##

summary(csv_new[,c('Rank','Price','Screenshot','Size','StarCurrentVersion', 'RatingCurrentVersion', 'TopInAppPurchases','Cluster1','Cluster2','Cluster3')])

# Variable Transformation #
Sales <- -log(Apps_new$Rank)
Log_Rating_Num <- log(Apps_new$RatingCurrentVersion+1)

Apps_new <-cbind(Apps_new, Sales)
Apps_new <-cbind(Apps_new, Log_Rating_Num)


# Build a regression model #
Apps_Reg <- lm(Sales ~ Price + Screenshot + Size + StarCurrentVersion + Log_Rating_Num + TopInAppPurchases + Cluster1 + Cluster2 + Cluster3, data=Apps_new)
summary(Apps_Reg)
#####################################################################################################################################################################

#
#
#

#################################
#### --Visualization start-- ####
#################################

##################
#1. plotting pros#
##################
freq_pros = rowSums(mat_pros, na.rm=TRUE)

#showing data more than 50 frequency
freq_pros = subset(freq_pros, freq_pros>=50)
barplot(freq_pros, las = 2) # visualization #

#sorting
v_pros <- sort(rowSums(mat_pros),decreasing=TRUE) 
d_pros <- data.frame(word = names(v_pros),freq=v_pros)

# top 50 in console
head(d_pros, 20)

## --adding word cloud visualization-- ##


##################
#2. plotting cons#
##################
freq_cons = rowSums(mat_cons, na.rm=TRUE)

#showing data more than 50 frequency
freq_cons = subset(freq_cons, freq_cons>=50)
barplot(freq_cons, las = 2) # visualization #

#sorting
v_cons <- sort(rowSums(mat_cons),decreasing=TRUE) 
d_cons <- data.frame(word = names(v_cons),freq=v_cons)

# top 50 in console
head(d_cons, 50)

#saving top 50 terms of pros and cons
d_all = cbind(d_pros[1:50,], d_cons[1:50,])
rownames(d_all) <- 1:nrow(d_all)

write.csv(d_all, "TF_All_Reviews.csv")

## --adding word cloud visualization-- ##


#
#
#




