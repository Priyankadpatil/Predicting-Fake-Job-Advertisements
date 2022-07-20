#install.packages("tidyverse")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
library(tidyverse)
library(dslabs)
library(dplyr)
library(gridExtra)
library(grid)
library(tinytex)
library(caret)
library(lubridate)
library(data.table)
library(tidytext)
library(stopwords)
#library(qdap)
library(readr)
#library(rJava)
library(tm)
library(SnowballC)
library(wordcloud)
library("RColorBrewer")
library(randomForest)
library(tictoc)
library(e1071)
library("party")
library("RCurl")
setwd("C:\\Drive\\Siri\\MSDSBA\\DSB6000_Leadership_Strategy\\Project")
rm(list=ls())
JobPosting_Data <- read.csv("fake_job_postings.csv")
column_names <- colnames(JobPosting_Data)
unique_columns_count <-  JobPosting_Data %>% 
  summarise(n_title = n_distinct(title),
            n_location = n_distinct(location),
            n_department = n_distinct(department),
            n_salary_range = n_distinct(salary_range),
            n_employment_type = n_distinct(employment_type),
            n_required_experience = n_distinct(required_experience),
            n_required_education = n_distinct(required_education),
            n_industry = n_distinct(industry),
            n_function = n_distinct(function.),
            n_fraudulent = n_distinct(fraudulent))
# Identify genuine job postings
JobPosting_genuine <- JobPosting_Data %>% filter(fraudulent == 0)
# Convert the factor column into a char column
JobPosting_genuine$requirements <- as.character(JobPosting_genuine$requirements)
# COnvert that to a dataframe
requirements <- as.data.frame(JobPosting_genuine$requirements)
count <- seq(1, nrow(requirements), 1)
lower_requirements <- sapply(count, function(c){tolower(requirements[c, 1])})
#Replace all non-alphanumeric characters with space.
del_special_chars_requirements <- sapply(count, function(l){str_replace_all(lower_requirements[l], "[^[:alnum:]]", " ")})
# TO Delete stop words from the rest
# Convert first this into a Corpus object
del_special_chars_requirements <- VCorpus(VectorSource(del_special_chars_requirements))
# Remove the stop words
del_special_chars_requirements = tm_map(del_special_chars_requirements, removeWords, stopwords(kind = "en"))
# Remove punctuation
del_special_chars_requirements <- tm_map(del_special_chars_requirements, removePunctuation)
# White space cleanup
white_space_cleanup_requirements <- tm_map(del_special_chars_requirements, stripWhitespace)
#perform stemming - this should always be performed after text doc conversion
stemming_requirements <- tm_map(white_space_cleanup_requirements, stemDocument)
text_requirements <- tm_map(stemming_requirements, stemDocument,language = "english")
print(as.character(text_requirements[[1]]))
text_requirements[[1]]$content
#convert to document term matrix
docterm_corpus_requirements <- DocumentTermMatrix(text_requirements)
inspect(docterm_corpus_requirements)
#convert to term document matrix
docterm_corpus_requirements2 <- TermDocumentMatrix(text_requirements)
inspect(docterm_corpus_requirements2)
requirements_matrix <- as.matrix(docterm_corpus_requirements2)
v_genuine_req <- sort(rowSums(requirements_matrix),decreasing=TRUE)
d_genuine_req <- data.frame(word = names(v_genuine_req),freq=v_genuine_req)
#Generate Wordcloud
set.seed(2021)
wordcloud(words = d_genuine_req$word, freq = d_genuine_req$freq,scale=c(4,.2), min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(d_genuine_req[1:10,]$freq, las = 2, names.arg = d_genuine_req[1:10,]$word,
        col ="lightblue", main ="Most frequent words in genuine Job postings",
        ylab = "Word frequencies")

# This is for overall fake job Requirements column
JobPosting_fake <- JobPosting_Data %>% filter(fraudulent == 1)
# Convert the factor column into a char column
JobPosting_fake$requirements <- as.character(JobPosting_fake$requirements)
# COnvert that to a dataframe
requirements_fake <- as.data.frame(JobPosting_fake$requirements)
count_fake <- seq(1, nrow(requirements_fake), 1)
lower_requirements_fake <- sapply(count_fake, function(c1){tolower(requirements_fake[c1, 1])})
#Replace all non-alphanumeric characters with space.
del_special_chars_requirements_fake <- sapply(count_fake, function(l1){str_replace_all(lower_requirements_fake[l1], "[^[:alnum:]]", " ")})
# TO Delete stop words from the rest
# Convert first this into a Corpus object
del_special_chars_requirements_fake <- VCorpus(VectorSource(del_special_chars_requirements_fake))
# Remove the stop words
del_special_chars_requirements_fake = tm_map(del_special_chars_requirements_fake, removeWords, stopwords(kind = "en"))
# Remove punctuation
del_special_chars_requirements_fake <- tm_map(del_special_chars_requirements_fake, removePunctuation)
# White space cleanup
white_space_cleanup_requirements_fake <- tm_map(del_special_chars_requirements_fake, stripWhitespace)
#perform stemming - this should always be performed after text doc conversion
stemming_requirements_fake <- tm_map(white_space_cleanup_requirements_fake, stemDocument)
text_requirements_fake <- tm_map(stemming_requirements_fake, stemDocument,language = "english")
print(as.character(text_requirements_fake[[1]]))
text_requirements_fake[[1]]$content
#convert to document term matrix
docterm_corpus_requirements_fake <- DocumentTermMatrix(text_requirements_fake)
inspect(docterm_corpus_requirements_fake)
#convert to term document matrix
docterm_corpus_requirements_fake2 <- TermDocumentMatrix(text_requirements_fake)
inspect(docterm_corpus_requirements_fake2)
requirements_matrix_fake <- as.matrix(docterm_corpus_requirements_fake2)
v_fake_req <- sort(rowSums(requirements_matrix_fake),decreasing=TRUE)
d_fake_req <- data.frame(word = names(v_fake_req),freq=v_fake_req)
#generate Wordcloud
set.seed(2021)
wordcloud(words = d_fake_req$word, freq = d_fake_req$freq,scale=c(4,.2), min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(d_fake_req[1:10,]$freq, las = 2, names.arg = d_fake_req[1:10,]$word,
        col ="lightblue", main ="Most frequent words in Fraud Job postings",
        ylab = "Word frequencies")

# words that are unique to fraud job postings and not genuine job postings
dt_gen_req <- data.frame(lapply(d_genuine_req, as.character), stringsAsFactors=FALSE)
dt_fake_req <- data.frame(lapply(d_fake_req, as.character), stringsAsFactors=FALSE)
#To identify the rows that exist in d_fake but not in d
d_fake_notin_d_gen_req <-   dt_fake_req[!dt_fake_req$word %in% dt_gen_req$word, ]
# Remove NA's
d_fake_notin_d_gen_req <-   d_fake_notin_d_gen_req[complete.cases(d_fake_notin_d_gen_req), ]
# convert them back to factor and num so that I can plot them
d_fake_notin_d_gen_req$word <- as.factor(d_fake_notin_d_gen_req$word)
d_fake_notin_d_gen_req$freq <- as.numeric(d_fake_notin_d_gen_req$freq)
#Generate WordCloud
set.seed(2021)
wordcloud(words = d_fake_notin_d_gen_req$word, freq = d_fake_notin_d_gen_req$freq,scale=c(4,0.2), min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
barplot(d_fake_notin_d_gen_req[1:10,]$freq, las = 2, names.arg = d_fake_notin_d_gen_req[1:10,]$word,
        col ="lightblue", main ="Most frequent words unique to Fraud Job postings",
        ylab = "Word frequencies")

#DESCRIPTION
JobPosting_genuine$description <- as.character(JobPosting_genuine$description)
# COnvert that to a dataframe
description <- as.data.frame(JobPosting_genuine$description)
count <- seq(1, nrow(description), 1)
lower_description <- sapply(count, function(c){tolower(description[c, 1])})
#Replace all non-alphanumeric characters with space.
del_special_chars_description <- sapply(count, function(l){str_replace_all(lower_description[l], "[^[:alnum:]]", " ")})
# TO Delete stop words from the rest
# Convert first this into a Corpus object
del_special_chars_description <- VCorpus(VectorSource(del_special_chars_description))
# Remove the stop words
del_special_chars_description = tm_map(del_special_chars_description, removeWords, stopwords(kind = "en"))
# Remove punctuation
del_special_chars_description <- tm_map(del_special_chars_description, removePunctuation)
# White space cleanup
white_space_cleanup_description <- tm_map(del_special_chars_description, stripWhitespace)
#perform stemming - this should always be performed after text doc conversion
stemming_description <- tm_map(white_space_cleanup_description, stemDocument)
text_description <- tm_map(stemming_description, stemDocument,language = "english")
print(as.character(text_description[[1]]))
text_description[[1]]$content
#convert to document term matrix
docterm_corpus_description <- DocumentTermMatrix(text_description)
inspect(docterm_corpus_description)
#convert to term document matrix
docterm_corpus_description2 <- TermDocumentMatrix(text_description)
inspect(docterm_corpus_description2)
description_matrix <- as.matrix(docterm_corpus_description2)
v_genuine_Desc <- sort(rowSums(description_matrix),decreasing=TRUE)
d_genuine_Desc <- data.frame(word = names(v_genuine_Desc),freq=v_genuine_Desc)
#Generate Wordcloud
set.seed(2021)
wordcloud(words = d_genuine_Desc$word, freq = d_genuine_Desc$freq,scale=c(4,.15), min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(d_genuine_Desc[1:10,]$freq, las = 2, names.arg = d_genuine_Desc[1:10,]$word,
        col ="lightblue", main ="Most frequent words in genuine Job postings",
        ylab = "Word frequencies")

# This is for overall fake job description column
#JobPosting_fake <- JobPosting_Data %>% filter(fraudulent == 1)
# Convert the factor column into a char column
JobPosting_fake$description <- as.character(JobPosting_fake$description)
# COnvert that to a dataframe
description_fake <- as.data.frame(JobPosting_fake$description)
count_fake <- seq(1, nrow(description_fake), 1)
lower_description_fake <- sapply(count_fake, function(c1){tolower(description_fake[c1, 1])})
#Replace all non-alphanumeric characters with space.
del_special_chars_description_fake <- sapply(count_fake, function(l1){str_replace_all(lower_description_fake[l1], "[^[:alnum:]]", " ")})
# TO Delete stop words from the rest
# Convert first this into a Corpus object
del_special_chars_description_fake <- VCorpus(VectorSource(del_special_chars_description_fake))
# Remove the stop words
del_special_chars_description_fake = tm_map(del_special_chars_description_fake, removeWords, stopwords(kind = "en"))
# Remove punctuation
del_special_chars_description_fake <- tm_map(del_special_chars_description_fake, removePunctuation)
# White space cleanup
white_space_cleanup_description_fake <- tm_map(del_special_chars_description_fake, stripWhitespace)
#perform stemming - this should always be performed after text doc conversion
stemming_description_fake <- tm_map(white_space_cleanup_description_fake, stemDocument)
text_description_fake <- tm_map(stemming_description_fake, stemDocument,language = "english")
print(as.character(text_description_fake[[1]]))
text_description_fake[[1]]$content
#convert to document term matrix
docterm_corpus_description_fake <- DocumentTermMatrix(text_description_fake)
inspect(docterm_corpus_description_fake)
#convert to term document matrix
docterm_corpus_description_fake2 <- TermDocumentMatrix(text_description_fake)
inspect(docterm_corpus_description_fake2)
description_matrix_fake <- as.matrix(docterm_corpus_description_fake2)
v_fake_desc <- sort(rowSums(description_matrix_fake),decreasing=TRUE)
d_fake_desc <- data.frame(word = names(v_fake_desc),freq=v_fake_desc)
#generate Wordcloud
set.seed(2021)
wordcloud(words = d_fake_desc$word, freq = d_fake_desc$freq,scale=c(4,.2), min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(d_fake_desc[1:10,]$freq, las = 2, names.arg = d_fake_desc[1:10,]$word,
        col ="lightblue", main ="Most frequent words in Fraud Job postings",
        ylab = "Word frequencies")

# words that are unique to fraud job postings and not genuine job postings
dt_genuine_desc <- data.frame(lapply(d_genuine_Desc, as.character), stringsAsFactors=FALSE)
dt_fake_desc <- data.frame(lapply(d_fake_desc, as.character), stringsAsFactors=FALSE)
#To identify the rows that exist in d_fake but not in d
d_fake_notin_d_gen_desc <-   dt_fake_desc[!dt_fake_desc$word %in% dt_genuine_desc$word, ]
# Remove NA's
d_fake_notin_d_gen_desc <-   d_fake_notin_d_gen_desc[complete.cases(d_fake_notin_d_gen_desc), ]
# convert them back to factor and num so that I can plot them
d_fake_notin_d_gen_desc$word <- as.factor(d_fake_notin_d_gen_desc$word)
d_fake_notin_d_gen_desc$freq <- as.numeric(d_fake_notin_d_gen_desc$freq)
#Generate WordCloud
set.seed(2021)
wordcloud(words = d_fake_notin_d_gen_desc$word, freq = d_fake_notin_d_gen_desc$freq,scale=c(4,.2), min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
barplot(d_fake_notin_d_gen_desc[1:10,]$freq, las = 2, names.arg = d_fake_notin_d_gen_desc[1:10,]$word,
        col ="lightblue", main ="Most frequent words unique to Fraud Job postings",
        ylab = "Word frequencies")


##### Words in Fraud Requirements that are not in Genuine requirements and Genuine/Fraud Description and not in Description fake

d_fake_notin_d_gen_desc
d_fake_notin_d_gen_req

dt_fake_notin_d_gen_desc <- data.frame(lapply(d_fake_notin_d_gen_desc, as.character), stringsAsFactors=FALSE)
dt_fake_notin_d_gen_req <- data.frame(lapply(d_fake_notin_d_gen_req, as.character), stringsAsFactors=FALSE)
#To identify the rows that exist in d_fake but not in d
d_fake_notin_d_gen_desc_req <-   dt_fake_notin_d_gen_req[!dt_fake_notin_d_gen_req$word %in% dt_fake_notin_d_gen_desc$word, ]
# Remove NA's
d_fake_notin_d_gen_desc_req <-   d_fake_notin_d_gen_desc_req[complete.cases(d_fake_notin_d_gen_desc_req), ]
# convert them back to factor and num so that I can plot them
d_fake_notin_d_gen_desc_req$word <- as.factor(d_fake_notin_d_gen_desc_req$word)
d_fake_notin_d_gen_desc_req$freq <- as.numeric(d_fake_notin_d_gen_desc_req$freq)
#Generate WordCloud
set.seed(2021)
wordcloud(words = d_fake_notin_d_gen_desc_req$word, freq = d_fake_notin_d_gen_desc_req$freq,scale=c(2,.15), min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
barplot(d_fake_notin_d_gen_desc_req[1:10,]$freq, las = 2, names.arg = d_fake_notin_d_gen_desc_req[1:10,]$word,
        col ="lightblue", main ="Most frequent words unique to Fraud Job postings",
        ylab = "Word frequencies")

#remove requirements from the final vector
#as it can be considered a duplicate of the field names in the text
tj_new <- d_fake_notin_d_gen_desc_req[-1,] 

wordcloud(words = tj_new$word, freq = tj_new$freq,scale=c(2,.15), min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
barplot(tj_new[1:10,]$freq, las = 2, names.arg = tj_new[1:10,]$word,
        col ="lightblue", main ="Most frequent words unique to Fraud Job postings - requirements only",
        ylab = "Word frequencies")

############################################### Benefits ####################################################################

JobPosting_genuine$benefits <- as.character(JobPosting_genuine$benefits)
# COnvert that to a dataframe
benefits <- as.data.frame(JobPosting_genuine$benefits)
count <- seq(1, nrow(benefits), 1)
lower_benefits <- sapply(count, function(c){tolower(benefits[c, 1])})
#Replace all non-alphanumeric characters with space.
del_special_chars_benefits <- sapply(count, function(l){str_replace_all(lower_benefits[l], "[^[:alnum:]]", " ")})
# TO Delete stop words from the rest
# Convert first this into a Corpus object
del_special_chars_benefits <- VCorpus(VectorSource(del_special_chars_benefits))
# Remove the stop words
del_special_chars_benefits = tm_map(del_special_chars_benefits, removeWords, stopwords(kind = "en"))
# Remove punctuation
del_special_chars_benefits <- tm_map(del_special_chars_benefits, removePunctuation)
# White space cleanup
white_space_cleanup_benefits <- tm_map(del_special_chars_benefits, stripWhitespace)
#perform stemming - this should always be performed after text doc conversion
stemming_benefits <- tm_map(white_space_cleanup_benefits, stemDocument)
text_benefits <- tm_map(stemming_benefits, stemDocument,language = "english")
print(as.character(text_benefits[[1]]))
text_benefits[[1]]$content
#convert to document term matrix
docterm_corpus_benefits <- DocumentTermMatrix(text_benefits)
inspect(docterm_corpus_benefits)
#convert to term document matrix
docterm_corpus_benefits2 <- TermDocumentMatrix(text_benefits)
inspect(docterm_corpus_benefits2)
benefits_matrix <- as.matrix(docterm_corpus_benefits2)
v_genuine_benefits <- sort(rowSums(benefits_matrix),decreasing=TRUE)
d_genuine_benefits <- data.frame(word = names(v_genuine_benefits),freq=v_genuine_benefits)
#Generate Wordcloud
set.seed(2021)
wordcloud(words = d_genuine_benefits$word, freq = d_genuine_benefits$freq,scale=c(4,.15), min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(d_genuine_benefits[1:10,]$freq, las = 2, names.arg = d_genuine_benefits[1:10,]$word,
        col ="lightblue", main ="Most frequent words in genuine Job postings",
        ylab = "Word frequencies")

# This is for overall fake job benefits column
#JobPosting_fake <- JobPosting_Data %>% filter(fraudulent == 1)
# Convert the factor column into a char column
JobPosting_fake$benefits <- as.character(JobPosting_fake$benefits)
# COnvert that to a dataframe
benefits_fake <- as.data.frame(JobPosting_fake$benefits)
count_fake <- seq(1, nrow(benefits_fake), 1)
lower_benefits_fake <- sapply(count_fake, function(c1){tolower(benefits_fake[c1, 1])})
#Replace all non-alphanumeric characters with space.
del_special_chars_benefits_fake <- sapply(count_fake, function(l1){str_replace_all(lower_benefits_fake[l1], "[^[:alnum:]]", " ")})
# TO Delete stop words from the rest
# Convert first this into a Corpus object
del_special_chars_benefits_fake <- VCorpus(VectorSource(del_special_chars_benefits_fake))
# Remove the stop words
del_special_chars_benefits_fake = tm_map(del_special_chars_benefits_fake, removeWords, stopwords(kind = "en"))
# Remove punctuation
del_special_chars_benefits_fake <- tm_map(del_special_chars_benefits_fake, removePunctuation)
# White space cleanup
white_space_cleanup_benefits_fake <- tm_map(del_special_chars_benefits_fake, stripWhitespace)
#perform stemming - this should always be performed after text doc conversion
stemming_benefits_fake <- tm_map(white_space_cleanup_benefits_fake, stemDocument)
text_benefits_fake <- tm_map(stemming_benefits_fake, stemDocument,language = "english")
print(as.character(text_benefits_fake[[1]]))
text_benefits_fake[[1]]$content
#convert to document term matrix
docterm_corpus_benefits_fake <- DocumentTermMatrix(text_benefits_fake)
inspect(docterm_corpus_benefits_fake)
#convert to term document matrix
docterm_corpus_benefits_fake2 <- TermDocumentMatrix(text_benefits_fake)
inspect(docterm_corpus_benefits_fake2)
benefits_matrix_fake <- as.matrix(docterm_corpus_benefits_fake2)
v_fake_benefits <- sort(rowSums(benefits_matrix_fake),decreasing=TRUE)
d_fake_benefits <- data.frame(word = names(v_fake_benefits),freq=v_fake_benefits)
#generate Wordcloud
set.seed(2021)
wordcloud(words = d_fake_benefits$word, freq = d_fake_benefits$freq,scale=c(4,.2), min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(d_fake_benefits[1:10,]$freq, las = 2, names.arg = d_fake_benefits[1:10,]$word,
        col ="lightblue", main ="Most frequent words in Fraud Job postings",
        ylab = "Word frequencies")

# words that are unique to fraud job postings and not genuine job postings
dt_genuine_benefits <- data.frame(lapply(d_genuine_benefits, as.character), stringsAsFactors=FALSE)
dt_fake_benefits <- data.frame(lapply(d_fake_benefits, as.character), stringsAsFactors=FALSE)
#To identify the rows that exist in d_fake but not in d
d_fake_notin_d_gen_benefits <-   dt_fake_benefits[!dt_fake_benefits$word %in% dt_genuine_benefits$word, ]
# Remove NA's
d_fake_notin_d_gen_benefits <-   d_fake_notin_d_gen_benefits[complete.cases(d_fake_notin_d_gen_benefits), ]
# convert them back to factor and num so that I can plot them
d_fake_notin_d_gen_benefits$word <- as.factor(d_fake_notin_d_gen_benefits$word)
d_fake_notin_d_gen_benefits$freq <- as.numeric(d_fake_notin_d_gen_benefits$freq)
#Generate WordCloud
set.seed(2021)
wordcloud(words = d_fake_notin_d_gen_benefits$word, freq = d_fake_notin_d_gen_benefits$freq,scale=c(4,.2), min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
barplot(d_fake_notin_d_gen_benefits[1:10,]$freq, las = 2, names.arg = d_fake_notin_d_gen_benefits[1:10,]$word,
        col ="lightblue", main ="Most frequent words unique to Fraud Job postings",
        ylab = "Word frequencies")



##### Words in Fraud Benefits that are not in Genuine benefits and Genuine/Fraud requirements and Genuine/Fraud description 

d_fake_notin_d_gen_desc_req
d_fake_notin_d_gen_benefits

dt_fake_notin_d_gen_desc_req <- data.frame(lapply(d_fake_notin_d_gen_desc_req, as.character), stringsAsFactors=FALSE)
dt_fake_notin_d_gen_ben <- data.frame(lapply(d_fake_notin_d_gen_benefits, as.character), stringsAsFactors=FALSE)
#To identify the rows that exist in d_fake but not in d
d_fake_notin_d_gen_desc_req_ben <-   dt_fake_notin_d_gen_ben[!dt_fake_notin_d_gen_ben$word %in% dt_fake_notin_d_gen_desc_req$word, ]
# Remove NA's
d_fake_notin_d_gen_desc_req_ben <-   d_fake_notin_d_gen_desc_req_ben[complete.cases(d_fake_notin_d_gen_desc_req_ben), ]
# convert them back to factor and num so that I can plot them
d_fake_notin_d_gen_desc_req_ben$word <- as.factor(d_fake_notin_d_gen_desc_req_ben$word)
d_fake_notin_d_gen_desc_req_ben$freq <- as.numeric(d_fake_notin_d_gen_desc_req_ben$freq)
#Generate WordCloud
set.seed(2021)
wordcloud(words = d_fake_notin_d_gen_desc_req_ben$word, freq = d_fake_notin_d_gen_desc_req_ben$freq,scale=c(4,.15), min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
barplot(d_fake_notin_d_gen_desc_req_ben[1:10,]$freq, las = 2, names.arg = d_fake_notin_d_gen_desc_req_ben[1:10,]$word,
        col ="lightblue", main ="Most frequent words unique to Fraud Job postings",
        ylab = "Word frequencies")

##############################################################################
  
library(dplyr)
original.data <- JobPosting_Data
clean.data <- original.data

#Dropping columns Job_id, Salary range(created a new column New_salary),telecommuting,
#Industry

clean.data<-select(clean.data,-c(1,5,11,17))
str(clean.data)
names(clean.data)

#346 missing values in location
levels(clean.data$location)[levels(clean.data$location) == ""]<- "Not mentioned"
#clean.data$location

#11,553 missing values in department
levels(clean.data$department)[levels(clean.data$department) == ""]<- "Not mentioned"
#clean.data$department

#15,012 values missing in salary range
clean.data$New_Salary= as.numeric(clean.data$New_Salary)
levels(clean.data$New_Salary)[levels(clean.data$New_Salary) == ""]<- "Not mentioned"
#clean.data$New_Salary

#1709 missing values in company profile
levels(clean.data$company_profile)[levels(clean.data$company_profile) == ""]<- "Not mentioned"
#clean.data$company_profile

#2696 missing values in requirements
levels(clean.data$requirements)[levels(clean.data$requirements) == ""]<- "Not mentioned"

#7208 missing values in benefits
levels(clean.data$benefits)[levels(clean.data$benefits) == ""]<- "Not mentioned"

#3471 missing values in employment type
levels(clean.data$employment_type)[levels(clean.data$employment_type) == ""]<- "Not mentioned"

#7050 missing values in required experience
levels(clean.data$required_experience)[levels(clean.data$required_experience) == ""]<- "Not mentioned"

#8105 missing values in required education
levels(clean.data$required_education)[levels(clean.data$required_education) == ""]<- "Not mentioned"

#6455 missing values in function.
levels(clean.data$function.)[levels(clean.data$function.) == ""]<- "Not mentioned"

str(clean.data)
head(clean.data)


library(dplyr)
original.data <- data.frame(read.csv("fake_job_posting.csv",header = TRUE))
clean.data <- original.data
library(dplyr)
require(caTools)
#install.packages("corrplot")
library(corrplot)
library(randomForest)
library(tictoc)
library(e1071)
library("party")
library("RCurl")
library(rpart)
library(lme4)



#Dropping columns Job_id, Salary range(created a new column New_salary),telecommuting,
#Industry


clean.data<-select(clean.data,-c(1,5,11,17))
str(clean.data)
names(clean.data)

#346 missing values in location
levels(clean.data$location)[levels(clean.data$location) == ""]<- "Not mentioned"
#clean.data$location

#11,553 missing values in department
levels(clean.data$department)[levels(clean.data$department) == ""]<- "Not mentioned"
#clean.data$department

#15,012 values missing in salary range
clean.data$New_Salary= as.numeric(clean.data$New_Salary)
levels(clean.data$New_Salary)[levels(clean.data$New_Salary) == ""]<- "Not mentioned"
#clean.data$New_Salary

#1709 missing values in company profile
levels(clean.data$company_profile)[levels(clean.data$company_profile) == ""]<- "Not mentioned"
#clean.data$company_profile

#2696 missing values in requirements
levels(clean.data$requirements)[levels(clean.data$requirements) == ""]<- "Not mentioned"

#7208 missing values in benefits
levels(clean.data$benefits)[levels(clean.data$benefits) == ""]<- "Not mentioned"

#3471 missing values in employment type
levels(clean.data$employment_type)[levels(clean.data$employment_type) == ""]<- "Not mentioned"

#7050 missing values in required experience
levels(clean.data$required_experience)[levels(clean.data$required_experience) == ""]<- "Not mentioned"

#8105 missing values in required education
levels(clean.data$required_education)[levels(clean.data$required_education) == ""]<- "Not mentioned"

#6455 missing values in function.
levels(clean.data$function.)[levels(clean.data$function.) == ""]<- "Not mentioned"

#15037 missing values in new_salary
clean.data$New_Salary= as.numeric(clean.data$New_Salary)
clean.data$New_Salary[is.na(clean.data$New_Salary)] <- 0

str(clean.data)
head(clean.data)


# checking missing values
sapply(clean.data, function(x) sum(is.na(x))) # checking missing values

##cleaning 
sapply(clean.data,function(x) sum(is.na(x)))
clean.data$benefits[which(is.na(clean.data$benefits))]<- as.factor("3.0")
clean.data <- clean.data[!(clean.data$benefits%in%c(NA)),]
colSums(is.na(clean.data))

fakejob <- clean.data
#fakejob

#Converting to factor variables

sapply(fakejob, class)

fakejob$title = as.numeric(fakejob$title)
fakejob$location = as.numeric(fakejob$location)
fakejob$department = as.numeric(fakejob$department)
fakejob$company_profile = as.numeric(fakejob$company_profile)
fakejob$description = as.numeric(fakejob$description)
fakejob$requirements = as.numeric(fakejob$requirements)
fakejob$benefits = as.numeric(fakejob$benefits)
fakejob$employment_type = as.numeric(fakejob$employment_type)
fakejob$required_experience = as.numeric(fakejob$required_experience)
fakejob$required_education = as.numeric(fakejob$required_education)
fakejob$function. = as.numeric(fakejob$function.)

sapply(fakejob, class)

sapply(fakejob, function(x) sum(is.na(x))) # checking missing values


# CORRELATION MATRIX
corrplot(cor(fakejob), method="number")


# 80:20 data
head(clean.data)


sample = sample.split(clean.data,SplitRatio = 0.80)
fj.train <- subset(clean.data, sample == TRUE)
head(fj.train)

fj.test <- subset(clean.data, sample == FALSE)
sapply(fj.train, class)

dim(fj.train)
sapply(fj.train, class)

#fj.model <- glm(fraudulent ~  title + location + has_company_logo + employment_type
#               ,family=binomial(link='logit'),data=fj.train)

##title,requirement,location,has_company_logo,has_questions,salary_range,employement type

###running the model using Decsion Trees
val_actual<-fj.train$fraudulent
head(val_actual)
length(val_actual)

model_dt <- rpart(fraudulent ~ title+location+has_company_logo+employment_type, data = fj.train)

#model_dt <- rpart(fraudulent ~ has_company_logo+has_questions+employment_type +
#                    required_experience + required_education + function., data = fj.train)
summary(model_dt)
printcp(model_dt)
plotcp(model_dt)

#plotting the tree
#plot(model_dt)
#text(model_dt, pretty = 0)

#pruning the tree
model_dt_prune<- prune(model_dt, cp=0.01)

# make predictions
pred_dt <- predict(model_dt_prune, fj.train)
length(pred_dt)
table(pred_dt)
table(pred_dt, fj.train$fraudulent)

rmse_dt <- sqrt(mean((pred_dt - val_actual)^2))
rmse_dt

rsquare <- R2(pred_dt,val_actual)
rsquare
#Plot tree
library(rpart.plot)  
png("tree.png", width=1000, height = 800, antialias = "cleartype")

fake <- rpart(fraudulent ~ title+location+has_company_logo+employment_type, data = fj.train)
rpart.plot(fake, main = "Classification Tree")
dev.off()

fake <- rpart(fraudulent ~ has_company_logo+has_questions+employment_type + required_experience, data = fj.train)
rpart.plot(fake, main = "Classification Tree")
fake <- rpart(fraudulent ~ has_company_logo+has_questions+employment_type + required_experience +
                required_education, data = fj.train)
rpart.plot(fake, main = "Classification Tree")
fake <- rpart(fraudulent ~ has_company_logo+has_questions+employment_type +
                required_experience + required_education + function., data = fj.train)
rpart.plot(fake, main = "Classification Tree")