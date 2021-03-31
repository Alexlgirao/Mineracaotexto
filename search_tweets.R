## Instalar a biblioteca rtweet
install.packages("rtweet")
## Carregar a biblioteca rtweet
library(rtweet)

## Definir uma Query (parâmetros de busca)
query <- "Plutão"
number_of_tweets <- 2000

## search for 2000 tweets using the rstats hashtag
tweets <- search_tweets(query,
                        language = "pt",
                        n = number_of_tweets,
                        include_rts = TRUE)
