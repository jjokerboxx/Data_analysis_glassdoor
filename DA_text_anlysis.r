#importing packages
library(dplyr)
library(tm)
library(SnowballC)
library(cluster)
library(topicmodels)
library(lda)
library(arules)
library(chinese.misc)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(wordcloud)

#setting working directory where csv files are located
#setwd("C:/Users/user/Desktop/glassdoor/reviews")
setwd("/Users/igihun/Desktop/glassdoor/reviews")




###########################################################
#### -- Merging all companies csv files into 1 file -- ####
###########################################################

#review_dir = "C:/Users/user/Desktop/glassdoor/reviews"
review_dir = "/Users/igihun/Desktop/glassdoor/reviews"
review_list = list.files(review_dir)

#Please write your name in English in between quotation marks
name = "all"

#making empty data frame
data = data.frame()

#rbinding csv files
for (c in review_list) {
  print(c)
  blanck = read.csv(c, encoding = "UTF-8")
  c_name = gsub("_review.csv", "", c)
  blanck[,20] = c_name
  colnames(blanck)[20] = "cor_name"
  blanck = blanck[1:400,1:20] #400 rows and 19 columns + add new cor_name column
  data = rbind(data, blanck)
}

#saving enw csv file
csv_file_name = "review.csv"
fin_name = paste(name,csv_file_name,sep = "_")
write.csv(data, fin_name)





## -- CSV Variables -- ##

#reading Merged csv file - beware of encoding
#csv = read.csv("all_review.csv", encoding = "UTF-8")
csv = read.csv("/Users/igihun/Desktop/glassdoor/reviews/all_review.csv")

#extracting review data variables
pros = csv$pros
cons = csv$cons

#extracting numeric rating data variables
balance = csv$rating_balance
culture = csv$rating_culture
develop = csv$rating_career
benefit = csv$rating_comp
manage = csv$rating_mgmt





########################################
#### -- Data Preprocessing start -- ####
########################################

###################
## -- NUMERIC -- ##
###################
# Statistics summary 
summary(balance)
summary(culture)
summary(develop)
summary(benefit)
summary(manage)
as.numeric(balance)

summary(scale(balance))
summary(scale(culture))
summary(scale(develop))
summary(scale(benefit))
summary(scale(manage))


#replace NA or drop NA
for (i in csv$X) {
  if (is.na(csv$rating_culture[i]) & is.na(csv$rating_career[i]) & is.na(csv$rating_comp[i]) & is.na(csv$rating_mgmt[i])) {
    if (is.na(csv$rating_balance[i])) {
      csv = subset(csv, !(csv$X == i))
    }
    else {
      csv$rating_culture[i] = csv$rating_balance[i]
      csv$rating_career[i] = csv$rating_balance[i]
      csv$rating_comp[i] = csv$rating_balance[i]
      csv$rating_mgmt[i] = csv$rating_balance[i]
    }
    
  }
}

#Individual corporation summary
sum_bal = summarise(group_by(csv, cor_name), mean = mean(as.numeric(rating_balance), na.rm = T), SD = sd(as.numeric(rating_balance), na.rm = T))
sum_cul = summarise(group_by(csv, cor_name), mean = mean(as.numeric(rating_culture), na.rm = T), SD = sd(as.numeric(rating_culture), na.rm = T))
sum_car = summarise(group_by(csv, cor_name), mean = mean(as.numeric(rating_career), na.rm = T), SD = sd(as.numeric(rating_career), na.rm = T))
sum_ben = summarise(group_by(csv, cor_name), mean = mean(as.numeric(rating_comp), na.rm = T), SD = sd(as.numeric(rating_comp), na.rm = T))
sum_mgn = summarise(group_by(csv, cor_name), mean = mean(as.numeric(rating_mgmt), na.rm = T), SD = sd(as.numeric(rating_mgmt), na.rm = T))




################
## -- TEXT -- ##
################
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
# different stopwords list to verify different analysis

#Removing too prevalent words in pros that might dillute the results
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

#Removing stopwords exclude the sentiment words for Association Analysis - pros
corpus_pros = tm_map(corpus_pros, removeWords, c("vipkid", "much", "just", "viasat", 
                                                  "your", "also",  
                                                 "ive",  
                                                 "will", "incred", "can", "dont", 
                                                  "often", "way", "find", 
                                                 "llnl", "realli", "bit", "say", "hubspot", 
                                                 "krono", "still", "thing", "may", "sfg", "sammon", 
                                                 "half",  "given", "industry", "don't",
                                                 "nvidia", "put", "month", "cut", "TRUE", "need",
                                                  "clear", "build", "done", "anderson",
                                                 "import", "pto", "within", "cant", "actual", "that",
                                                 "come", "esop", "even", "fantast", "look",
                                                 "corpor", "there", "everi", "rally", "mani",
                                                 "busi", "forward", "direct", "part", "use",
                                                 "anyth", "sinc", "howev", "none", "see", "number",
                                                 "keep", "around", "volunt", "last", "give", "yet", 
                                                 "work", "good", "compani", "lot",  "make",
                                                 "get", "like", "well", "day", 
                                                 "alway", "perk", "hubspot", "esop", "friday", "just",
                                                 "long", "etc", "abl", "big", "one", "driven", "top", "meet",
                                                 "tri", "take", "first", "made", "start", "set", "week", "excit",
                                                 "real", "want", "easi", "someth", "larg", "year",
                                                 "work", "compani", "benefit", 
                                                 
                                                 "great",  "everi", "week", "new", "job", "know", "better", "feel", "best",
                                                 "truli", "everyon")) #"peopl", "cultur", "employe", "opportun",





## -- Cons -- ##
# In sentimaent analysis, we have to differ the stopword to analyze each documnets!!
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

#Removing stopwords exclude the sentiment words for Sentiment analysis - cons
corpus_cons = tm_map(corpus_cons, removeWords, c("sometim", "lack", "also", "depend", "make", 
                                                 "alway",  "take", "can", "none", 
                                                 "bain", "everyon", "truli", "much",  "way", "provid", "year", "will", 
                                                 "around", "extrem",  "big", "alway", 
                                                 "can", "just", "docusign",  "use", 
                                                 "north", "seem", "due", "still", "there", "hubspot",
                                                 "con", "may", "cant", "doesnt", "actual",
                                                 "across", "that", "bit", "con", "without",
                                                 "org", "nvidia", "sammon", "busi",  "back",
                                                 "come", "even", "look", "corpor", "always",
                                                 "less", "part", "anyth", "sinc", "howev", "noth",
                                                 "see", "number", "keep", "within", "last", "certain",
                                                 "yet", "best", "work",  "compani", "lot",
                                                 "get", "like", "well",  "day", "truli",
                                                 "alway", "perk", "one", "long", "year", 
                                                 "work", "compani", "benefit", "think", "thing", "far", "arent", "dont", "your"))


#trimming unnecessary spaces
corpus_pros = tm_map(corpus_pros, stripWhitespace)
corpus_cons = tm_map(corpus_cons, stripWhitespace)


#matrixing stems : TDM | DTM
#TDM for Clustering
tdm_pros = TermDocumentMatrix(corpus_pros)
tdm_cons = TermDocumentMatrix(corpus_cons)

#removing sparsity
tdm_pros = removeSparseTerms(tdm_pros, sparse = 0.99)
mat_pros = as.matrix(tdm_pros)
nrow(mat_pros)

tdm_cons = removeSparseTerms(tdm_cons, sparse = 0.99)
mat_cons = as.matrix(tdm_cons)


#DTM for TF-IDF
dtm_pros = DocumentTermMatrix(corpus_pros)
dtm_pros_tfidf = DocumentTermMatrix(corpus_pros, control = list(weighting=weightTfIdf))

dtm_cons = DocumentTermMatrix(corpus_cons)
dtm_cons_tfidf = DocumentTermMatrix(corpus_cons, control = list(weighting=weightTfIdf))

#removing sparsity
dtm_pros = removeSparseTerms(dtm_pros, sparse = 0.99)
mat_dtm_pros = as.matrix(dtm_pros)

dtm_cons = removeSparseTerms(dtm_cons, sparse = 0.99)
mat_dtm_cons = as.matrix(dtm_cons)

dtm_pros_tfidf = removeSparseTerms(dtm_pros_tfidf, sparse = 0.99)
mat_pros_tfidf = as.matrix(dtm_pros_tfidf)

dtm_cons_tfidf = removeSparseTerms(dtm_cons_tfidf, sparse = 0.99)
mat_cons_tfidf = as.matrix(dtm_cons_tfidf)








##############################################
#### -- Term Frequency Analysis start -- #####
##############################################


#1. plotting pros

freq_pros = rowSums(mat_pros, na.rm=TRUE)

#showing data more than 50 frequency
freq_pros = subset(freq_pros, freq_pros>=50)


barplot(freq_pros, las = 2) # visualization #
barplot(sort(freq_pros), las = 2)

#sorting
v_pros <- sort(rowSums(mat_pros),decreasing=TRUE) 
d_pros <- data.frame(word = names(v_pros),freq=v_pros)

# top 50 in console
head(d_pros, 20)

## --adding word cloud visualization-- ##



#2. plotting cons

freq_cons = rowSums(mat_cons, na.rm=TRUE)

#showing data more than 50 frequency
freq_cons = subset(freq_cons, freq_cons>=50)
barplot(freq_cons, las = 2) # visualization #
barplot(sort(freq_cons), las = 2)

#sorting
v_cons <- sort(rowSums(mat_cons),decreasing=TRUE) 
d_cons <- data.frame(word = names(v_cons),freq=v_cons)

# top 50 in console
head(d_cons, 50)

#saving top 50 terms of pros and cons
#d_all = cbind(d_pros[1:50,], d_cons[1:50,])
#rownames(d_all) <- 1:nrow(d_all)
#write.csv(d_all, "TF_All_Reviews.csv")

## --adding word cloud visualization-- ##







######################################################
#### -- Hierarchical Clustering Analysis start -- ####
######################################################


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






################################################
#### -- K-mean Clustering Analysis start -- ####
################################################

nrow(mat_pros)

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






##############################################
#### -- LDA Topic model Analysis start -- ####
##############################################

set.seed(1234)

# -- Pros LDA -- #

ldaform_pros = dtm2ldaformat(dtm_pros, omit_empty=T)

result.lda_pros = lda.collapsed.gibbs.sampler(documents = ldaform_pros$documents,
                                         K = 10,
                                         vocab = ldaform_pros$vocab,
                                         num.iterations = 10000,  
                                         burnin = 2000, # or 1000
                                         alpha = 0.02,  
                                         eta = 0.02 )
# basic LDA attr info 
#attributes(result.lda)
#dim(result.lda$topics)    
#result.lda$topics
#dim(result.lda$document_sums)

#all topics
result.lda_pros$topics
# 5 topics
top.topic.words(result.lda_pros$topics)
# sum of each topic
result.lda_pros$topic_sums 
# examples of topic modeling 
result.lda_pros$document_sums[,1293] 


# -- Cons LDA -- #

ldaform_cons = dtm2ldaformat(dtm_cons, omit_empty = T)

result.lda_cons = lda.collapsed.gibbs.sampler(documents = ldaform_cons$documents,
                                              K = 10,
                                              vocab = ldaform_cons$vocab,
                                              num.iterations = 10000,  
                                              burnin = 2000, # or 1000
                                              alpha = 0.02,  
                                              eta = 0.02 )

#all topics
result.lda_cons$topics
# 5 topics
top.topic.words(result.lda_cons$topics)
# sum of each topic
result.lda_cons$topic_sums 
# examples of topic modeling 
result.lda_cons$document_sums[,1293] 





###########################################################
#### -- Term Association | Correltion Anlysis start -- ####
###########################################################

#Association analysis of topic terms - as clean as possible!!! - new stopwords for this analysis
pros_term = rownames(mat_pros)
Assocs_pros = findAssocs(tdm_pros, pros_term, 0.1)
findAssocs(tdm_pros, terms = c("flexibl", "care", "balanc", "help",  "develop"), corlimit = 0.2) # frequent term?

findAssocs(tdm_pros, terms = c("balanc", "develop", "cultur"), corlimit = 0.2)

findAssocs(tdm_pros, "balanc", corlimit = 0.1)
#manual cluster!!!!!!!!
findAssocs(tdm_pros, terms = c("balanc",  "schedul", "flexibl", "worklif", "life", "hour", "shift", "time", "remot", "famili", "person"), corlimit = 0.11)
findAssocs(tdm_pros, terms = c("pay", "competit", "hour", "salari", "compens", "full", "time", "paid", "health", "insur", "plan", "stock", "option", "program"), corlimit = 0.11)
findAssocs(tdm_pros, terms = c("manag", "employe", "hire", "peopl", "team", "never", "senior", "encourag", "leadership", "leader", "execut"), corlimit = 0.11)
findAssocs(tdm_pros, terms = c("career", "develop", "growth", "profession", "person", "learn", "grow", "constant", "new", "train", "program"), corlimit = 0.11)
findAssocs(tdm_pros, terms = c("cultur", "environ", "friend", "collabor", "team", "support", "provid", "care"), corlimit = 0.11)

#Apriori algorithm to find term relation
mat_dtm_pros[mat_dtm_pros>1] = 1
rownames(mat_pros)
rules1_pros = apriori(mat_dtm_pros, parameter = list(support = 0.01, confidence = 0.20, minlen = 2))
inspect(sort(rules1_pros))


mat_dtm_cons[mat_dtm_cons>1] = 1
rownames(mat_cons)
rules1_cons = apriori(mat_dtm_cons, parameter = list(support = 0.01, confidence = 0.20, minlen = 2))
inspect(sort(rules1_cons))



#Assocs-Apriori test
mat_apriori_pros = matrix(data = 0, nrow = nrow(mat_pros), ncol = nrow(mat_pros))
rownames(mat_apriori_pros) = rownames(tdm_pros)
colnames(mat_apriori_pros) = rownames(tdm_pros)

for (i in pros_term) {
  term_assoc = findAssocs(tdm_pros, i, 0.15)
  attrs_assoc = term_assoc[[1]]
  for (j in 1:length(attrs_assoc)) {
    name = names(attrs_assoc[j])
    mat_apriori_pros[i,name] = 1
  }
}

rules2_pros = apriori(mat_apriori_pros, parameter = list(support = 0.01, confidence = 0.20, minlen = 2))
inspect(sort(rules2_pros))


## -- Cons -- ##
cons_term = rownames(mat_cons)
Assocs_cons = findAssocs(tdm_cons, cons_term, 0.1)
findAssocs(tdm_cons, terms = c("manag", "time", "hour","balanc"), corlimit = 0.1)


mat_apriori_cons = matrix(data = 0, nrow = nrow(mat_cons), ncol = nrow(mat_cons))
rownames(mat_apriori_cons) = rownames(tdm_cons)
colnames(mat_apriori_cons) = rownames(tdm_cons)

for (i in cons_term) {
  term_assoc = findAssocs(tdm_cons, i, 0.15)
  attrs_assoc = term_assoc[[1]]
  for (j in 1:length(attrs_assoc)) {
    name = names(attrs_assoc[j])
    mat_apriori_cons[i,name] = 1
  }
}

rules2_cons = apriori(mat_apriori_cons, parameter = list(support = 0.01, confidence = 0.20, minlen = 2))
inspect(sort(rules2_cons))





# word_cor function to show correlation with terms
words_pros = row.names(tdm_pros)
word_cor(tdm_pros, words_pros, type = "tdm", method = "pearson", p = 0.1, min = 0.1)

word_cor(tdm_pros, c("cultur", "valu", "balanc"), type = "tdm", method = "pearson", p = 0.1, min = 0.1)

words_cons = row.names(tdm_cons)
word_cor(tdm_cons, words_cons, type = "tdm", method = "pearson", p = 0.1, min = 0.1)







#####################################
#### --Sentiment anlysis start-- ####
#####################################

# 
#
# -- Pros --
pros_names = rownames(tdm_pros)
p<-as.character(pros_names)
#syuzhet 
syuzhet_vector_pros <- get_sentiment(p, method="syuzhet")
head(syuzhet_vector_pros)
plot(syuzhet_vector_pros)
summary(syuzhet_vector_pros)     

#bing
bing_vector_pros <- get_sentiment(p, method="bing")
head(bing_vector_pros)
summary(bing_vector_pros)

#affin
afinn_vector_pros <- get_sentiment(p, method="afinn")
head(afinn_vector_pros)
summary(afinn_vector_pros)

#emotionanalysis
c_pros <-get_nrc_sentiment(p)
head (c_pros,5)



# -- Cons --
#SENTIMENT 
cons_names = rownames(tdm_cons)
c<-as.character(cons_names)
#syuzhet 
syuzhet_vector_cons <- get_sentiment(c, method="syuzhet")
plot(syuzhet_vector_cons)
head(syuzhet_vector_cons)
summary(syuzhet_vector_cons)     

# bing
bing_vector_cons <- get_sentiment(c, method="bing")
head(bing_vector_cons)
summary(bing_vector_cons)

#affin
afinn_vector_pros <- get_sentiment(c, method="afinn")
head(afinn_vector_pros)
summary(afinn_vector_pros)

c_cons <-get_nrc_sentiment(c);
summary(c_cons)




#############################################
#### --Further cluster | model analysis--####
#############################################

#from course material
#build manual cluster to further analysis | model
#how to select words? -> findAssocs - with key words of each criteria
cluster1 <- dtm_pros[,c("manag", "employe", "hire", "peopl", "team", "never", "senior", "encourag", "leadership", "leader", "execut")]; head(cluster1)
cluster2 <- dtm_pros[,c("balanc",  "schedul", "flexibl", "worklif", "life", "hour", "time", "remot", "famili", "person")]; head(cluster2) # shift
cluster3 <- dtm_pros[,c("pay", "competit", "hour", "salari", "compens", "full", "time", "paid", "health", "insur", "plan", "stock", "option", "program")]; head(cluster3)
cluster4 <- dtm_pros[,c("cultur", "environ", "friend", "collabor", "team", "support", "provid", "care", "fun")]; head(cluster4)
cluster5 <- dtm_pros[,c("career", "develop", "growth", "profession", "person", "learn", "grow", "constant", "train", "program")]; head(cluster5) # new

clu1_tfidf = dtm_pros_tfidf[, c("manag", "employe", "hire", "peopl", "team", "never", "senior", "encourag", "leadership", "leader", "execut")]
clu2_tfidf <- dtm_pros_tfidf[,c("balanc",  "schedul", "flexibl", "worklif", "life", "hour", "time", "remot", "famili", "person")]; head(cluster2) # shift
clu3_tfidf <- dtm_pros_tfidf[,c("pay", "competit", "hour", "salari", "compens", "full", "time", "paid", "health", "insur", "plan", "stock", "option", "program")]; head(cluster3)
clu4_tfidf <- dtm_pros_tfidf[,c("cultur", "environ", "friend", "collabor", "team", "support", "provid", "care", "fun")]; head(cluster4)
clu5_tfidf <- dtm_pros_tfidf[,c("career", "develop", "growth", "profession", "person", "learn", "grow", "constant", "train", "program")]; head(cluster5) 



# code from course material #### On the Work! ##################################################################################################################################
# Sums #
C1_Sum <- colSums(as.matrix(cluster1)); C1_Sum
c1_sum = unname(C1_Sum); sum(c1_sum)
C2_Sum <- colSums(as.matrix(cluster2)); C2_Sum
c2_sum = unname(C2_Sum); sum(c2_sum)
C3_Sum <- colSums(as.matrix(cluster3)); C3_Sum
c3_sum = unname(C3_Sum); sum(c3_sum)
C4_Sum <- colSums(as.matrix(cluster4)); C4_Sum
c4_sum = unname(C4_Sum); sum(c4_sum)
C5_Sum <- colSums(as.matrix(cluster5)); C5_Sum
c5_sum = unname(C5_Sum); sum(c5_sum)


C1_Sum_tfidf = colSums(as.matrix(clu1_tfidf)) 
c1_sum_tfidf = unname(C1_Sum_tfidf); sum(c1_sum_tfidf)
C2_Sum_tfidf = colSums(as.matrix(clu2_tfidf)) 
c2_sum_tfidf = unname(C2_Sum_tfidf); sum(c2_sum_tfidf)
C3_Sum_tfidf = colSums(as.matrix(clu3_tfidf)) 
c3_sum_tfidf = unname(C3_Sum_tfidf); sum(c3_sum_tfidf)
C4_Sum_tfidf = colSums(as.matrix(clu4_tfidf)) 
c4_sum_tfidf = unname(C4_Sum_tfidf); sum(c4_sum_tfidf)
C5_Sum_tfidf = colSums(as.matrix(clu5_tfidf)) 
c5_sum_tfidf = unname(C5_Sum_tfidf); sum(c5_sum_tfidf)



# Create a Score table #
Score <- matrix(data=0 , n_desc,5);
Score[,1] <- as.matrix(C1_Sum)
Score[,2] <- as.matrix(C2_Sum)
Score[,3] <- as.matrix(C3_Sum)
Score[,4] <- as.matrix(C3_Sum)
Score[,5] <- as.matrix(C3_Sum)

# Name the Columns/Clusters #
colnames(Score) <- c("Management", "Balance", "Compensation", "Culture", "Development")
head(Score)

## Add a Score matrix to the original Data ##


## Run a Regression ##


# Variable Transformation #


# Build a regression model #

#####################################################################################################################################################################

#
#
#

#################################
#### --Visualization start-- ####
#################################






#
#
#




