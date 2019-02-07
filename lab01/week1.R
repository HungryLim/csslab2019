setwd("C:/Users/wooki/Documents/GitHub/csslab2019/lab01")
tweet_df <- read.csv("Political-media-DFE.csv", stringsAsFactors = FALSE)
tweet_df <- tweet_df[tweet_df$message %in% c("policy", "personal", "information", "attack", "mobilization"), ]

head(tweet_df)

tweet_df[1:3, c("message", "label", "text")]
sort(table(tweet_df$message), decreasing = TRUE)

#install.packages("quanteda")
library(quanteda)
set.seed(139871)


# "document feature" matrix
dtm <- quanteda::dfm(tweet_df$text, tolower = FALSE,remove_numbers=T,remove_symbols=T,remove_hyphens = T,remove_punct = TRUE,stem=T)

# so we can go about usual R business...
dtm_mat <- as.matrix(dtm)

#1 line
#tokens(tweet_df$text,remove_numbers=T,remove_symbols=T,remove_hyphens = T,remove_punct = TRUE)
dtm_mat[1, dtm_mat[1,] != 0]



# process
#processed<-tokens(tweet_df$text,remove_numbers=T,remove_symbols=T,remove_hyphens = T,remove_punct = TRUE, stem=T)
#processed[1]
#dtm[1]


dim(dtm)

set.seed(129374)
trainingset <- sample(1:nrow(tweet_df), nrow(tweet_df)/2)
testset <- c(1:nrow(tweet_df))[-trainingset]

# train model
mod1 <- quanteda::textmodel_nb(x = dtm[trainingset],
                               y = tweet_df$message[trainingset],
                               prior = "uniform")

# basic checks of predictive ability
preds <- predict(mod1, newdata = dtm[testset])
table(tweet_df$message[testset] == preds) / length(testset)

