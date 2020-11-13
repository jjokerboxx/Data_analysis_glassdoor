#importing packages
library(dplyr)
library(tm)
library(SnowballC)

#reading companies csv file - beware of encoding
csv = read.csv("all_sparse_mat_pros.csv", encoding = "UTF-8")
csv = data.matrix(csv)
csv


#Hierarchical clustering - standardization by scale func
distance = dist(scale(csv))

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
