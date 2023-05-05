library(sentimentr)
library(plyr)
library(dplyr)
library(emoji)
library(syuzhet)

emote1 <- c("This class is the greatest of all time") 
emote2 <- c("Been up studying all night, getting tired, need to sleep")
emote3 <- c("Why can't I figure this crap out, I'm starting to get angry")
emote4 <- c("Still awake, can't sleep, gotta figure this out, grrrrrr")
emote5 <- c("Yay I got it.... finaly!!  Super excited")
# Create vector of all emotes
 emotes <- c(emote1, emote2, emote3, emote4, emote5)

# Calculate the mean sentiment score of each emote and its result (positive, negative or neutral)
senr_sen_score <- c()
senr_sen_result <- c()
senr_sen_emoji <- c()

syuzhet_sen_score <- c()
syuzhet_sen_result <- c()
syuzhet_sen_emoji <- c()

for (i in emotes){
  # Calculate the sentiment
  senr_sen <- sentiment(i,n.before=0, n.after=0, amplifier.weight=0)
  syuzhet_sen <- get_nrc_sentiment(i, cl = NULL, language = "english", lowercase = TRUE)
  # Calculate the mean sentiment
  mean_sen <- mean(senr_sen$sentiment)
  
  # Determine if sentiment is positive or negative
  # Neutral set as within 0.05 of zero
  
  if (mean_sen < -0.05){
    senr_sen_result <- c(senr_sen_result, "negative")
    senr_sen_emoji <- c(senr_sen_emoji, emoji("sad"))
    
  } else if (mean_sen > 0.05) {
    senr_sen_result <- c(senr_sen_result, "positive")
    senr_sen_emoji <- c(senr_sen_emoji, emoji("smile"))
    
  } else {
    senr_sen_emoji <- c(senr_sen_emoji, emoji("neutral"))
    senr_sen_result <- c(senr_sen_result, "neutral")
  }
  
  # Add sen score to senr_sen_score
  senr_sen_score <- c(senr_sen_score, mean_sen)
  syuzhet_sen_score <- c(syuzhet_sen_score, syuzhet_sen)
  
}

# Display results in a dataframe
# sen_analysis <- data.frame(emotes, senr_sen_score, senr_sen_result)
sen_analysis <- data.frame(emotes, senr_sen_score, senr_sen_result, senr_sen_emoji)
# syuzhen_analysis <- data.frame(emotes, syuzhet_sen_score)
sen_analysis
syuzhen_analysis
senr_sen_result
senr_sen_emoji
senr_sen_score
mean_sen
senr_sen
syuzhet_sen

