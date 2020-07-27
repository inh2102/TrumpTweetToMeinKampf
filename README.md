### Just how similar is a given Trump tweet to a line from Hitler’s Mein Kampf? Let’s use cosine similarity find out…


This script uses four packages: ```tidyverse``` (notation), ```quanteda``` (text
pre-processing and cosine similarity calculation), ```readr``` (reading in
data), and ```rtweet``` (importing and processing Twitter API data). You’ll
need to un-comment the install.packages() calls if you haven’t installed
these packages.

``` r
#install.packages(tidyverse)
#install.packages(quanteda)
#install.packages(readr)
#install.packages(rtweet)

library(tidyverse,quietly=T); library(quanteda,quietly=T); library(readr,quietly=T); library(rtweet,quietly=T)
```

As of 7/26/20, Trump has produced **43,137 original tweets** (excluding
retweets). Let’s read those in along with all 10,317 lines of Mein
Kampf:

``` r
trump <- read_csv("all_trump_original_tweets_072620.csv",col_types = cols())
fileName <- 'MeinKampf.txt'
MeinKampf <- readChar(fileName, file.info(fileName)$size) %>% strsplit("\\.\\s|\\!\\s|\\?\\s")
MeinKampf <- tibble(data.frame(matrix(unlist(MeinKampf), nrow=10317, byrow=T),stringsAsFactors=FALSE))
df <- MeinKampf %>% rename(text = matrix.unlist.MeinKampf...nrow...10317..byrow...T.)
```

*Mein Kampf text pre-processing:*

``` r
hitlercorp <- corpus(df$text)
hitlerdfm <- tokens(hitlercorp) %>% tokens_ngrams(n=1:3) %>% dfm(tolower=TRUE,remove_url=TRUE,stem=TRUE,remove_punct=TRUE,remove=c(stopwords("english")))
```

*Here we define the function:*

``` r
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
```

### Before running the function, if you’d like to view 10 random Trump tweets, you can do so here:

``` r
sample_n(plain_tweets(trump),10)
```

    ## # A tibble: 10 x 2
    ##      num text                                                                   
    ##    <dbl> <chr>                                                                  
    ##  1 35930 "You can't dispel this mood of positive energy. @Varneyco The Economy …
    ##  2 37549 "A very big Caravan of over 20,000 people started up through Mexico. I…
    ##  3 25700 "Leading in the Bloomberg Iowa poll. Also, my favorability numbers wen…
    ##  4 35632 "Happy Birthday Sean!//t.co/iQgY6RTWST"                                
    ##  5 25711 "Just announced that because of \"Trump\", advertising rates for debat…
    ##  6 21382 "30 million Americans are unemployed yet Obama has set up workshops ac…
    ##  7 11021 "\"@Antonio_Annesi: @realDonaldTrump No doubt you would fix our countr…
    ##  8 10378 "\"@theholst: @realDonaldTrump I was against fame entering the white h…
    ##  9 37510 "Everybody is asking why the Justice Department (and FBI) isn't lookin…
    ## 10 34846 "Happy to announce, I am nominating Alex Azar to be the next HHS Secre…

Time to turn a Trump tweet into Mein Kampf. Can you tell the difference?
------------------------------------------------------------------------

``` r
select_trump_tweet()
```
```
The FAKE NEWS media (failing @nytimes, @NBCNews, @ABC, @CBS, @CNN) is not my enemy, it is the enemy of the American People!
```
    ## Trump tweet selected!
    ## 
    ## Searching lines from Mein Kampf...
    ## 
    ## Done! Top 5 matching results:

```
    1 The result was that the enemies of the Republic ceased to oppose the Republic as such and helped to subjugate those who were also enemies of the Republic, though for quite different reasons 
    
    2 Only the enemies of the two countries, Germany and Russia, could have an active interest in such a war under these circumstances 
    
    3 It is the task of the propagandist to recruit the followers and it is the task of the organizer to select the members 
    ## 
    4 Here again it is the fault of the education given our young people 
    
    5 Those who effectively combat this mortal enemy of our people, who is at the same time the enemy of all Aryan peoples and all culture, can only expect to arouse opposition on the part of this race and become the object of its slanderous attacks
```
