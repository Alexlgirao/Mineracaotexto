library(rvest)
library(stringr)
library(tidyverse)
library(tm)
library(wordcloud)

scrape_post_body <- function(site) {
    print(paste("Baixando ", site))
    source_html <- read_html(site)
    body <- source_html %>%
        html_nodes("article") %>%
        html_nodes("p") %>%
        html_text()
    bodies <- body[!is.na(body)]
    return(paste(bodies, collapse=""))
}

scrape_post_titles <- function(site) {
    print(paste("Baixando ", site))
    source_html <- read_html(site)

    titles <- source_html %>%
              html_nodes("h3") %>%
              html_nodes("a") %>%
              html_text()
    titles <- titles[!is.na(titles)]
    return(titles)
}




root <- "https://www.r-bloggers.com/"
all_pages <- c(root, paste0(root, "page/", 2:5))
all_titles <- lapply(all_pages, scrape_post_titles)
all_titles <- unlist(all_titles)

cleaned <- all_titles %>%
            tolower() %>%
            removeNumbers() %>%
            removeWords(stopwords("en")) %>%
            removePunctuation() %>%
            str_trim()

cleaned_corpus <- Corpus(VectorSource(cleaned))
cleaned_corpus <- tm_map(cleaned_corpus, stemDocument)

doc_object <- TermDocumentMatrix(cleaned_corpus)
doc_matrix <- as.matrix(doc_object)
counts <- sort(rowSums(doc_matrix),decreasing=TRUE)
counts <- counts[grepl("^[a-z]+$", names(counts))]
frame_counts <- data.frame(word = names(counts), freq = counts)

wordcloud(words = frame_counts$word, freq = frame_counts$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.2,
          colors=brewer.pal(8, "Dark2"))
