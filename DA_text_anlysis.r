#importing packages
library(dplyr)
library(tm)
library(SnowballC)
library(cluster)
library(topicmodels)
library(lda)

setwd("C:/Users/user/Desktop/glassdoor/reviews")
#reading companies csv file - beware of encoding
csv = read.csv("all_review.csv", encoding = "UTF-8")

pros = csv$pros
cons = csv$cons

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

#manual stopwords
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
                                                 "real", "want", "easi", "someth", "larg"))
corpus_cons = tm_map(corpus_pros, removeWords, c("sometim", "lack", "also", "depend", "make", 
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
                                                 "alway", "perk", "one", "long"))


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


# LDA code for learning
dtm_pros_dtm = as.DocumentTermMatrix(tdm_pros)
dtm_pros = as.matrix(dtm_pros_dtm)

tdm_cons = removeSparseTerms(tdm_cons, sparse = 0.99)
mat_cons = as.matrix(tdm_cons)



set.seed(1234)

ldaform = dtm2ldaformat(dtm_pros_dtm, omit_empty=T)

result.lda = lda.collapsed.gibbs.sampler(documents = ldaform$documents,
                                         K = 8,
                                         vocab = ldaform$vocab,
                                         num.iterations = 5000,  
                                         burnin = 1000, 
                                         alpha = 0.01,  
                                         eta = 0.01 )

attributes(result.lda)
dim(result.lda$topics)    
result.lda$topics

top.topic.words(result.lda$topics)
result.lda$topic_sums 
dim(result.lda$document_sums)
result.lda$document_sums[,1] 

################################
# --Term association anlysis-- #
################################
findAssocs(tdm_pros, "schedul", 0.1)
findAssocs(tdm_pros, "balanc", 0.1)
findAssocs(tdm_pros, "life", 0.1)



#are there any asso-corr functions?



#####################################
# -- Hierarchicalclustering start-- #
#####################################


#Hierarchical clustering - standardization by scale func
distance = dist(scale(mat_pros), method = "minkowski")

#Hierarchical clustering - using "ward.D" method
hc = hclust(distance, method = "ward.D"); plot(hc)

#
rect.hclust(hc, k = 8)

hc_cut = cutree(hc, k = 8); hc_cut

hc$labels

#check the results in plot
plot(hc)


###############################
# --k-mean clustering start-- #
###############################

nrow(mat_pros) #265

#k-mean clustering
#mat_km = t(mat_pros)
set.seed(1234)
#mat_pros_stand = scale(mat_pros)
mat_km = kmeans(mat_pros, 8)

mat_km$size

wss <- 1:5
for(i in 1:5) {wss[i] <- sum(kmeans(mat_pros,i)$withinss)}
plot(1:5, wss[1:5], type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")
# type="b" creates a plot with lines between points # 
wss

rowSums(mat_pros) # frequencies of each term in row sums
# if we make DTM - and select some terms for cluster -> rowsums will show the frequency of the clusters
# later, regression can be applied

plot(mat_km$cluster)



###########################
# --visualization start-- #
###########################

###############
#plotting pros#
###############
freq_pros = rowSums(mat_pros, na.rm=TRUE)

#showing data more than 50
freq_pros = subset(freq_pros, freq_pros>=50)
barplot(freq_pros, las = 2, col = rainbow(7))

#sorting
v_pros <- sort(rowSums(mat_pros),decreasing=TRUE) 
d_pros <- data.frame(word = names(v_pros),freq=v_pros)

# top 50
head(d_pros, 50)


###############
#plotting cons#
###############
freq_cons = rowSums(mat_cons, na.rm=TRUE)

#showing data more than 50
freq_cons = subset(freq_cons, freq_cons>=50)
barplot(freq_cons, las = 2, col = rainbow(7))

#sorting
v_cons <- sort(rowSums(mat_cons),decreasing=TRUE) 
d_cons <- data.frame(word = names(v_cons),freq=v_cons)

# top 50
head(d_cons, 50)

#build manual cluster to further analysis | model
cluster1 <- dtm_pros[,c("remot", "sale", "stock", "discount")]; head(cluster1)

