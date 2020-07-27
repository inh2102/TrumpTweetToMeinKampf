# Trump Tweet to Mein Kampf

### Just how similar is a given Trump tweet to a line from Hitler’s Mein Kampf? Let’s find out…

This script uses four packages: ```diff - tidyverse``` (notation), ```diff - quanteda``` (text
pre-processing and cosine similarity calculation), ```diff - readr``` (reading in
data), and ```diff - rtweet``` (importing and processing Twitter API data). You’ll
need to un-comment the install.packages() calls if you haven’t installed
these packages.

``` r
#install.packages(tidyverse)
#install.packages(quanteda)
#install.packages(readr)
#install.packages(rtweet)

library(tidyverse); library(quanteda); library(readr); library(rtweet)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    ## Package version: 2.1.0

    ## Parallel computing: 2 of 16 threads used.

    ## See https://quanteda.io for tutorials and examples.

    ## 
    ## Attaching package: 'quanteda'

    ## The following object is masked from 'package:utils':
    ## 
    ##     View

    ## 
    ## Attaching package: 'rtweet'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     flatten

As of 7/26/20, Trump has produced 43,137 original tweets (excluding
retweets). Let’s read those in along with all 10,317 lines of Mein
Kampf:

``` r
trump <- read_csv("all_trump_original_tweets_072620.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   num = col_double(),
    ##   text = col_character()
    ## )

``` r
fileName <- 'MeinKampf.txt'
MeinKampf <- readChar(fileName, file.info(fileName)$size) %>% strsplit("\\.\\s|\\!\\s|\\?\\s")
MeinKampf <- tibble(data.frame(matrix(unlist(MeinKampf), nrow=10317, byrow=T),stringsAsFactors=FALSE))
df <- MeinKampf %>% rename(text = matrix.unlist.MeinKampf...nrow...10317..byrow...T.)
```

Mein Kampf text pre-processing:

``` r
hitlercorp <- corpus(df$text)
hitlerdfm <- tokens(hitlercorp) %>% tokens_ngrams(n=1:3) %>% dfm(tolower=TRUE,remove_url=TRUE,stem=TRUE,remove_punct=TRUE,remove=c(stopwords("english")))
```

Here we define the function:

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
    ##  1  6395 "\"@williamwatp: @realDonaldTrump great phone interview on espn! Ps fo…
    ##  2 27369 "Is Hillary really protecting women?//t.co/8ZtEIWNqz4"                 
    ##  3 27933 "I employ many people in Hawaii at my great hotel in Honolulu. I'll be…
    ##  4 22691 "Wow, little Mac Miller has almost 100 million views on his song, \"Do…
    ##  5  9569 "On Fifth Avenue, the iconic @TrumpTowerNY is one of NYC's most heavil…
    ##  6 39384 "Trade talks going on with numerous countries that, for many years, ha…
    ##  7 37501 "Good things are happening at China Trade Talk Meeting. Warmer feeling…
    ##  8 29576 "Thank you Hershey, Pennsylvania. Get out & VOTE on November 8th & we …
    ##  9 17921 "Someone just wrote that you predicted every single major event that's…
    ## 10 39577 "This President has done more for African Americans in this Country th…

Time to turn a Trump tweet into Mein Kampf. Can you tell the difference?
------------------------------------------------------------------------

``` r
select_trump_tweet()
```

    ## The FAKE NEWS media (failing @nytimes, @NBCNews, @ABC, @CBS, @CNN) is not my enemy, it is the enemy of the American People!
    ## 
    ## Trump tweet selected!
    ## 
    ## Searching lines from Mein Kampf...
    ## 
    ## Done! Top 5 matching results:
    ## 
    ## 1 The result was that the enemies of the Republic ceased to oppose the Republic as such and helped to subjugate those who were also enemies of the Republic, though for quite different reasons 
    ## 
    ## 2 Only the enemies of the two countries, Germany and Russia, could have an active interest in such a war under these circumstances 
    ## 
    ## 3 It is the task of the propagandist to recruit the followers and it is the task of the organizer to select the members 
    ## 
    ## 4 Here again it is the fault of the education given our young people 
    ## 
    ## 5 Those who effectively combat this mortal enemy of our people, who is at the same time the enemy of all Aryan peoples and all culture, can only expect to arouse opposition on the part of this race and become the object of its slanderous attacks
