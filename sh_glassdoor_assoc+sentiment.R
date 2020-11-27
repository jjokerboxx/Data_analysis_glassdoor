install.packages("tm") 
install.packages("SnowballC") 
install.packages("wordcloud") 
install.packages("RColorBrewer") 
install.packages("syuzhet")
install.packages("ggplot2")

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
#setwd("C:/Users/user/Desktop/glassdoor/reviews")
#csv = read.csv("all_review.csv", encoding = "UTF-8")
csv = read.csv("C:/Users/user/Desktop/glassdoor/reviews/all_review.csv")


##PROS
pros = csv$pros

corpus_pros <- Corpus(VectorSource(pros))
corpus_pros <- tm_map(corpus_pros, content_transformer(tolower))
corpus_pros <- tm_map(corpus_pros, removeNumbers)
corpus_pros <- tm_map(corpus_pros, removePunctuation)  
corpus_pros <- tm_map(corpus_pros, removeWords, stopwords("english"))
corpus_pros <- tm_map(corpus_pros, stemDocument)
corpus_pros <- tm_map(corpus_pros, removeWords, c("vipkid", "much", "just", "viasat", 
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
                                                 "real", "want", "easi", "someth", "larg",
                                                 "peopl", "benefit", "cultur", "employe", 
                                                 
                                                 "and", "the", "for", "are", "you", "with", "have", "the", "from", "their", ""))
corpus_pros <- tm_map(corpus_pros, stripWhitespace)
tdm_pros = TermDocumentMatrix(corpus_pros)
tdm_pros = removeSparseTerms(tdm_pros, sparse = 0.99)
mat_pros = as.matrix(tdm_pros)

dtm_v <- sort(rowSums(mat_pros),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
#  top 7 most frequent words
head(dtm_d, 7)

# visualization
barplot(dtm_d[1:7,]$freq, las = 2, names.arg = dtm_d[1:7,]$word,
        col ="lightpink", main ="Top 7 most frequent words",
        ylab = "Word frequencies")

set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

#ASSOCIATION: top 5 ?‹¨?–´?“¤?˜ association 
findAssocs(tdm_pros, terms = c("opportun","environ","manag","pay","team"), corlimit = 0.15)

#SENTIMENT (?•„?ž˜?˜ ?„¸ê°€ì§€ ë¶„ì„ë°©ë²• ì¡´ìž¬)
p<-as.character(pros)
#syuzhet 
syuzhet_vector_pros <- get_sentiment(p, method="syuzhet")
head(syuzhet_vector_pros)
summary(syuzhet_vector_pros)     

# bing
bing_vector_pros <- get_sentiment(p, method="bing")
head(bing_vector_pros)
summary(bing_vector_pros)

#affin
afinn_vector_pros <- get_sentiment(p, method="afinn")
head(afinn_vector_pros)
summary(afinn_vector_pros)


## ?„¸ê°€ì§€ ë¶„ì„ ëª¨ë‘median ê°’ì´ ?–‘?ˆ˜?´ë¯€ë¡? overall average sentiment across all the responses is positiveë¡? ?•´?„ê°€?Š¥

#emotionanalysis
c <-get_nrc_sentiment(p)
head (c,5)



##CONS

cons = review$cons
corpus_cons <- Corpus(VectorSource(cons))
corpus_cons <- tm_map(corpus_cons, content_transformer(tolower))
corpus_cons <- tm_map(corpus_cons, removeNumbers)
corpus_cons <- tm_map(corpus_cons, removePunctuation)  
corpus_cons <- tm_map(corpus_cons, removeWords, stopwords("english"))
corpus_cons <- tm_map(corpus_cons, stripWhitespace)
corpus_cons <- tm_map(corpus_cons, stemDocument)
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
                                                 "alway", "perk", "one", "long","think","job","dont"))

tdm_cons = TermDocumentMatrix(corpus_cons)
tdm_cons = removeSparseTerms(tdm_cons, sparse = 0.99)
mat_cons = as.matrix(tdm_cons)

dtm_v <- sort(rowSums(mat_cons),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

#  top 10 most frequent words
head(dtm_d, 10)

barplot(dtm_d[1:7,]$freq, las = 2, names.arg = dtm_d[1:7,]$word,
        col ="lightblue", main ="Top 7 most frequent words",
        ylab = "Word frequencies")

set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

#ASSOCIATION: top 5 ?‹¨?–´?“¤?˜ association 
findAssocs(tdm_cons, terms = c("manag","time","hour","peopl","employe"), corlimit = 0.2)

#SENTIMENT 
q<-as.character(cons)
#syuzhet 
syuzhet_vector_cons <- get_sentiment(q, method="syuzhet")
head(syuzhet_vector_cons)
summary(syuzhet_vector_cons)     

# bing
bing_vector_cons <- get_sentiment(q, method="bing")
head(bing_vector_cons)
summary(bing_vector_cons)

#affin
afinn_vector_pros <- get_sentiment(p, method="afinn")
head(afinn_vector_pros)
summary(afinn_vector_pros)

d <-get_nrc_sentiment(q)
head (d,5)
