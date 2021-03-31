library("rtweet")
library("wordcloud")
library("tidyverse")
library("tm")


query <- "petrobras"
number_of_tweets <- 5000
tweets <- search_tweets(query,
                        lang = "pt",
                        n = number_of_tweets,
                        include_rts = TRUE)

my_stopwords <- c(stopwords('pt'),
                  'petrobras',
                  'petrobrás',
                  'twitter')

tweets <-  tweets %>% select(text)
cleaned <- tweets %>%
           tolower() %>%
           removeWords(my_stopwords) %>%
           removePunctuation()

corpus <- Corpus(VectorSource(cleaned))
doc_matrix <- as.matrix(TermDocumentMatrix(corpus))

counts <- sort(rowSums(doc_matrix), decreasing = TRUE)
frame_counts <- data.frame(word = names(counts), freq = counts)

wordcloud(words = frame_counts$word,
          font = 2,
          freq = frame_counts$freq,
          min.freq = 3,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.1,
          colors = brewer.pal(10, "RdGy"))
