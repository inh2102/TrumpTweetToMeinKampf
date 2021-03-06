  ### Just how similar is a given Trump tweet to a line from Hitler's Mein Kampf? Let's find out...
  
  ### This script uses four packages: tidyverse (notation), quanteda (text pre-processing and cosine similarity calculation), and rtweet (importing and processing Twitter API data). You'll need to un-comment the install.packages() calls if you haven't installed these packages.

#install.packages(tidyverse)
#install.packages(quanteda)
#install.packages(rtweet)

library(tidyverse,quietly=T); library(quanteda,quietly=T); library(rtweet,quietly=T)

# As of 7/26/20, Trump has produced 43,137 original tweets (excluding retweets). Let's read those in along with all 10,317 lines of Mein Kampf:

trump <- read_csv("all_trump_original_tweets_072620.csv",col_types = cols())
fileName <- 'MeinKampf.txt'
MeinKampf <- readChar(fileName, file.info(fileName)$size) %>% strsplit("\\.\\s|\\!\\s|\\?\\s")
MeinKampf <- tibble(data.frame(matrix(unlist(MeinKampf), nrow=10317, byrow=T),stringsAsFactors=FALSE))
df <- MeinKampf %>% rename(text = matrix.unlist.MeinKampf...nrow...10317..byrow...T.)

# Mein Kampf text pre-processing:

hitlercorp <- corpus(df$text)
hitlerdfm <- tokens(hitlercorp) %>% tokens_ngrams(n=1:3) %>% dfm(tolower=TRUE,remove_url=TRUE,stem=TRUE,remove_punct=TRUE,remove=c(stopwords("english")))

# Here we define the function:

select_trump_tweet <- function() {
  q1 <- readline(prompt="Which Trump tweet would you like to view? (A number from 1-43137) ")
  cat(plain_tweets(trump$text)[as.numeric(q1)])
  q2 <- readline(prompt="Would you like to use this tweet (Yes or No)? ")
  if (as.character(tolower(q2))=="yes") {
    trumpcorp <- corpus(plain_tweets(trump$text)[as.numeric(q1)]) 
    trumpdfm <- tokens(trumpcorp) %>% tokens_ngrams(n=1:10) %>% dfm(tolower=TRUE,remove_url=TRUE,stem=TRUE,remove_punct=TRUE,remove=c(stopwords("english"), "t.co", "https", "rt", "amp", "http", "t.c", "can", "~","RT","realdonaldtrump"))
    cat("\n\nTrump tweet selected!\n\n")
    cat("Searching lines from Mein Kampf...\n\n")
    trump_hitler <- as.data.frame(textstat_simil(hitlerdfm, trumpdfm, margin = "documents",method="cosine"))
    cat("Done! Top 5 matching results:\n\n")
    trump_hitler <- as_tibble(trump_hitler[order(-trump_hitler$cosine),])
    for (tweet in trump_hitler$document1[1:5]) {
      cat(paste(which(trump_hitler$document1==tweet),df$text[as.numeric(substr(tweet,5,nchar(tweet)))],"\n\n"))
    }
  }
  else {
    select_trump_tweet()
  }
}

### Before running the function, if you'd like to view 10 random Trump tweets, you can do so here:
  
sample_n(plain_tweets(trump),10)

## Time to turn a Trump tweet into Mein Kampf. Can you tell the difference?

select_trump_tweet()
