#csv merger
review_dir = "C:/Users/user/Desktop/glassdoor/reviews"
review_list = list.files(review_dir)

data = data.frame()

for (c in review_list) {
  print(c)
  blanck = read.csv(c, encoding = "UTF-8")
  blanck = blanck[1:400,1:19]
  
  data = rbind(data, blanck)
  
}

write.csv(data, "all_review.csv")