#csv merger
### --Before run the code-- ###
# 1. make new folder only for 6 compaies csv files
# 2. write folder location to 'review_dir'
# 3. make sure to change 'back-slash' or 'Korea Won sign' to normal slash in address
# 4. in session tab menu, you can set working directory to reviews folder
setwd("C:/Users/user/Desktop/glassdoor/reviews/south")

review_dir = "C:/Users/user/Desktop/glassdoor/reviews/south"
review_list = list.files(review_dir)

#Please write your name in English in between quotation marks
name = "kihun"

#making empty data frame
data = data.frame()

#rbinding csv files
for (c in review_list) {
  print(c)
  blanck = read.csv(c, encoding = "UTF-8")
  blanck = blanck[1:400,1:19]
  
  data = rbind(data, blanck)
  
}

#saving enw csv file
csv_file_name = "review.csv"
fin_name = paste(name,csv_file_name,sep = "_")
write.csv(data, fin_name)