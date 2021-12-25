library(stringi)
library(stringr)
library(tm)
library(gsubfn)
library(RWeka)
library(ggplot2)

setwd("C:/Users/dyoon/OneDrive - Innovista Health/Me/Projects/R/R Coursera/Capstone/final/en_US")


file1 <- readRDS(file="./cleanfile_327259.rds")
file2 <- readRDS(file="./cleanfile_400002.rds")

blogs <- "./en_US.blogs.txt"
news <- "./en_US.news.txt"
twitter <- "./en_US.twitter.txt"

#analysis of files
blogs_file <- readLines(blogs)
news_file <- readLines(news)
twitter_file <- readLines(twitter)


#clean file 1
blogs_sample1 <- blogs_file[1:125000]
news_sample1 <- news_file[1:77259]
twitter_sample1 <- twitter_file[1:125000]
bnt_sample1 <- c(blogs_sample1, news_sample1, twitter_sample1)

Encoding(bnt_sample1) <- "UTF-8"
bnt_sample1 <- tolower(bnt_sample1)
bnt_sample1 <- str_replace_all(bnt_sample1, "[^[:alnum:]]", " ")
bnt_sample1 <- stripWhitespace(bnt_sample1)
bnt_sample1 <- removePunctuation(bnt_sample1)
bnt_sample1 <- removeNumbers(bnt_sample1)

length(bnt_sample1)

saveRDS(bnt_sample1, file = "cleanfile_1_to_327259.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)


#clean file 2
blogs_sample2 <- blogs_file[200000:400000]
twitter_sample2 <- twitter_file[200000:400000]
bnt_sample2 <- c(blogs_sample2, twitter_sample2)

Encoding(bnt_sample2) <- "UTF-8"
bnt_sample2 <- tolower(bnt_sample2)
bnt_sample2 <- str_replace_all(bnt_sample2, "[^[:alnum:]]", " ")
bnt_sample2 <- stripWhitespace(bnt_sample2)
bnt_sample2 <- removePunctuation(bnt_sample2)
bnt_sample2 <- removeNumbers(bnt_sample2)

length(bnt_sample2)

saveRDS(bnt_sample2, file = "cleanfile_400002.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)


#clean file 3
blogs_sample2 <- blogs_file[200000:400000]
twitter_sample2 <- twitter_file[200000:400000]
bnt_sample2 <- c(blogs_sample2, twitter_sample2)

Encoding(bnt_sample2) <- "UTF-8"
bnt_sample2 <- tolower(bnt_sample2)
bnt_sample2 <- str_replace_all(bnt_sample2, "[^[:alnum:]]", " ")
bnt_sample2 <- stripWhitespace(bnt_sample2)
bnt_sample2 <- removePunctuation(bnt_sample2)
bnt_sample2 <- removeNumbers(bnt_sample2)

length(bnt_sample2)

saveRDS(bnt_sample2, file = "cleanfile_400002.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
