# Bar Plot #

# Pros
freq_pros = subset(freq_pros, freq_pros>=300)
mat_freq_pros = as.matrix(freq_pros)
freq_pros_d = sort(rowSums(mat_freq_pros),decreasing=TRUE)

barplot(freq_pros_d, las = 2, col="blue")

# Cons
freq_cons = subset(freq_cons, freq_cons>=300)
mat_freq_cons = as.matrix(freq_cons)
freq_cons_d = sort(rowSums(mat_freq_cons),decreasing=TRUE)

barplot(freq_cons_d, las = 2, col="red")










# Word Cloud #
library(wordcloud)
library(RColorBrewer)

#Pros
corpus_pros <- Corpus(VectorSource(pros))
corpus_pros <- tm_map(corpus_pros, content_transformer(tolower))
corpus_pros <- tm_map(corpus_pros, removeNumbers)
corpus_pros <- tm_map(corpus_pros, removePunctuation)  
corpus_pros <- tm_map(corpus_pros, removeWords, stopwords("english"))
corpus_pros <- tm_map(corpus_pros, stripWhitespace)
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
                                                  "peopl", "benefit", "cultur", "employe"))

tdm_pros = TermDocumentMatrix(corpus_pros)
tdm_pros = removeSparseTerms(tdm_pros, sparse = 0.99)
mat_pros = as.matrix(tdm_pros)

dtm_v <- sort(rowSums(mat_pros),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)


pal <- brewer.pal(8, "Dark2")  #색상 코드
set.seed(1234)
wordcloud(words = dtm_d$word, #출력할 단어
          freq = dtm_d$freq, #언급된 빈도수
          max.words = 200, #표현 단어 수
          random.order = F, #고빈도 단어 중앙배치
          rot.per = 0.1, # 90도 회전해서 보여줄 단어 비율
          scale = c(4, 0.3), #단어 크기 범위
          colors = pal   #색
          )
# 'Opportun' is regarded as the most important factor when getting positive reviews
# 'manager', 'team', 'pay', 'environment' is also important factors to evaluate the job for employees


#Cons
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

set.seed(1234)
wordcloud(words = dtm_d$word,
          freq = dtm_d$freq,
          max.words = 200,
          random.order = F,
          rot.per = 0.1, # 90도 회전해서 보여줄 단어 비율
          scale = c(4, 0.3),
          colors = pal
)

# 'Manager' seems to be the biggest reason why a company gets a negative review
# Seeing that 'Time' is the second most frequently used word, overworking is a highly negative factor in evaluation





# LDA Visualization #
top.topic.words(result.lda_pros$topics)
mat_topic_words = as.matrix(top.topic.words(result.lda_pros$topics))
row1 = mat_topic_words[ ,1, drop=F]


topic_words_v <- sort(rowSums(mat_topic_words_n))
topic_words_d <- data.frame(word = names(mat_topic_words[ ,1,drop=F]),freq=mat_topic_words[ ,1,drop=F])


lda <- 
  
  
  terms(top.topic.words(result.lda_pros$topics), 5)