setwd("C:/Users/me/Documents")
library(rvest)

datalist = list()
for (i in 1:23) {
       # create a url template 
        URL.base <- "https://www.trustpilot.com/review/www.sonetel.com?page="
        #create the dataframe with the dynamic values
        URL <- paste0(URL.base, i)
        #print(URL)

        webpage<-read_html(URL)
####################usernames####################

        #Using CSS selectors to scrap the usernames
        user_names_html<-html_nodes(webpage,'.user-review-name-link')

        #Convertingtotext
        user_names<-html_text(user_names_html)

        #Data-Preprocessing:removing'\n'and''
        user_names<-gsub("\n","",user_names)
        user_names<-gsub(" ","",user_names)
        user_names<-as.factor(user_names)

        #head(user_names)

####################review-body####################

        #Using CSS selectors to scrap the review-body
        review_body_html<-html_nodes(webpage,'.review-body')

        #Converting to text
        review_body<-html_text(review_body_html)

        #Data-Preprocessing:removing'\n'and ' '
        review_body<-gsub("\n","",review_body)
        review_body<-gsub("\r","",review_body)
        review_body<-gsub('\\s+',' ',review_body)
        review_body<-as.character(review_body)

        #head(review_body)

####################dataframing and adding to a list####################

        df<-data.frame(user = user_names, body = review_body)
        datalist[[i]] <- df
}

big_data <- do.call(rbind, datalist)
#save(big_data,file="sonotel.Rda") #saving my dataframe
#summary(big_data)
#str(big_data)
#########################Done with data extraction part#########################################################################


##############################Start of aspect detection#####################################
library(tidytext)
library(dplyr)
library(stringr)
library(tm)
datalist2 = list()
data <- as.character(big_data$body)

# Load the positive and negative lexicon data
positive_lexicon <- read.csv("positive-words.txt")
negative_lexicon <- read.csv("negative-words.txt")

corpus <- Corpus(VectorSource(big_data$body))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, stemDocument)

# counting words for each review
total_pos_count <- 0
total_neg_count <- 0
total_aspect_count <- 0
pos_count_vector <- c()
neg_count_vector <- c()


aspect_word = c("Customer","quality","sms","support","setup","customize","phoneservice","purchase","Voip","Troubleshotting","callrates","provider","Voicemail","callforwarding","soundquality")
aspect_words <- Corpus(VectorSource(aspect_word))
aspect_words <- tm_map(aspect_words, content_transformer(tolower))
aspect_words <- tm_map(aspect_words, removePunctuation)
aspect_words <- tm_map(aspect_words, removeWords, stopwords("english"))
aspect_words <- tm_map(aspect_words, stripWhitespace)
aspect_words <- tm_map(aspect_words, removeNumbers)
#aspect_words <- tm_map(aspect_words, stemDocument)


size <- length(corpus)

for(i in 1:size){
  corpus_words<- list(strsplit(corpus[[i]]$content, split = " "))

  #print(paste0("aspect words are"))
  aspect_words <- list(aspect_words)
  aspects <- intersect(unlist(corpus_words),unlist(aspect_words))
  #print(aspects)
  aspect_count <- length(intersect(unlist(corpus_words),unlist(aspect_words)))
  
  #print(paste0("positive words are"))
  #print(intersect(unlist(corpus_words), unlist(positive_lexicon))) ## positive words in current review
  positive <- intersect(unlist(corpus_words), unlist(positive_lexicon))
  pos_count <- length(intersect(unlist(corpus_words), unlist(positive_lexicon)))

  #print(paste0("negative words are"))
  #print(intersect(unlist(corpus_words), unlist(negative_lexicon))) ## negative words in current review
  negative <- intersect(unlist(corpus_words), unlist(negative_lexicon))
  neg_count <- length(intersect(unlist(corpus_words), unlist(negative_lexicon)))
  
  total_count_for_current_review <- pos_count + neg_count ## current positive and negative count
  pos_percentage <- (pos_count*100)/total_count_for_current_review
  neg_percentage <- (neg_count*100)/total_count_for_current_review
  #print(pos_percentage)                          ## current positive percentage
  #print(neg_percentage)                          ## current negtive percentage
  #print(paste0("______________________________________________________________"))

  total_pos_count <- total_pos_count + pos_count
  total_neg_count <- total_neg_count + neg_count
  total_aspect_count <- total_aspect_count + aspect_count
  pos_count_vector <- append(pos_count_vector, pos_count)
  neg_count_vector <- append(neg_count_vector, neg_count)


  for(j in 1:length(aspects)) {
    if( pos_count>=neg_count && length(aspects)>=1 ){
      z = (neg_percentage/100) * 2
      rate <- as.numeric(z)
      aspects <- c(aspects[j])
      rating <- data.frame(aspects, rate)
      datalist2 <-rbind(datalist2, rating)
    } 
      
    if(neg_count>pos_count && length(aspects)>=1 ){
      z = (neg_percentage/100) * 2
      rate <- as.numeric(z)
      aspects <- c(aspects[j])
      rating <- data.frame(aspects, rate)
      datalist2 <-rbind(datalist2, rating)
      
    }
  }
}
aspect_data <- datalist2
table(aspect_data)


########################done with aspect recognition##########################

#################################total sentiment#########################################

# Sentiment score of all reviews 
counts <- data.frame(pos_count_vector, neg_count_vector)
sentiment <- data.frame(c(1:size),(pos_count_vector - neg_count_vector) / (pos_count_vector + neg_count_vector))

total_count <- total_pos_count + total_neg_count
overall_positive_percentage <- (total_pos_count*100)/total_count
overall_negative_percentage <- (total_neg_count*100)/total_count

if(overall_positive_percentage > overall_negative_percentage) {
  cat(" The overall sentiment is positive with percentage")
  print(overall_positive_percentage)
}           
if(overall_positive_percentage < overall_negative_percentage) {
  cat(" The overall sentiment is negative with percentage")
  print(overall_negative_percentage)
}           

mean(aspect_data$rate[aspect_data$aspects == "provider"],na.rm=TRUE)