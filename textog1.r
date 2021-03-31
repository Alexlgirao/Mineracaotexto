library(rvest)
library(stringr)
library(tidyverse)
library(tm)
library(igraph)
library(wordcloud)
library(urltools)
library(spacyr)
library(gtools)
set.seed(1235)
#spacy_install()
#funcao para acessar os links do G1
scrape_post_links <- function(site) {
    # scrape HTML from input site
    source_html <- read_html(site)
    # grab the title attributes from link (anchor)
    # tags within H2 header tags
    links <- source_html %>%
        html_nodes("div.widget--info__text-container") %>%
        html_nodes("a") %>%
        html_attr("href")
    # filter out any titles that are NA (where no title was found)
    links <- links[!is.na(links)]
    # return vector of titles
    return(links)
}

root <- "https://g1.globo.com/busca/?q=AFOGAMENTO"
# get each webpage URL we need
all_pages <- c(root, paste0(root, "&page=", 1:20))
# use our function to scrape the title of each post
all_links <- lapply(all_pages, scrape_post_links)
# collapse the titles into a vector
all_links <- unlist(all_links)

extract_urls <- function(raw_url) {
    params <- urltools::param_get(raw_url)
    scraped_url <- params$u
    return (url_decode(scraped_url))
}
cleaned_links <- lapply(all_links, extract_urls)
# Not interested in Videos from globoplay app
cleaned_links <- Filter(function(x) !any(grepl("globoplay", x)),
                        cleaned_links)
cleaned_links <- Filter(function(x) !any(grepl("video", x)),
                        cleaned_links)

scrape_post_body <- function(site) {
    # Escape 404 Not Found Errors
    try(
        text <- site %>%
            read_html %>%
            html_nodes("article") %>%
            html_nodes("p.content-text__container") %>%
            html_text
    )
}
data <- lapply(cleaned_links, scrape_post_body)
data <- lapply(data,
               function(item) paste(unlist(item),
                                    collapse = ''))

# convert all titles to lowercase
cleaned <- tolower(data)
# remove any numbers from the titles
cleaned <- removeNumbers(cleaned)
# remove English stopwords
cleaned <- removeWords(cleaned, c(stopwords("pt"), "anos"))
# remove punctuation
cleaned <- removePunctuation(cleaned)
# remove spaces at the beginning and end of each title
#cleaned <- str_trim(cleaned)
cleaned <- str_squish(cleaned)
# convert vector of titles to a corpus
cleaned_corpus <- Corpus(VectorSource(cleaned))
# steam each word in each title
#cleaned_corpus <- tm_map(cleaned_corpus, stemDocument,"portuguese")
doc_object <- TermDocumentMatrix(cleaned_corpus)
doc_matrix <- as.matrix(doc_object)
# get counts of each word
counts <- sort(rowSums(doc_matrix),decreasing=TRUE)
# filter out any words that contain non-letters
#counts <- counts[grepl("^\\w+$", names(counts))]
#counts <- counts[grepl("^[a-z]+$", names(counts))]
# create data frame from word frequency info
frame_counts <- data.frame(word = names(counts), freq = counts)

wordcloud(words = frame_counts$word,
          freq = frame_counts$freq,
          min.freq = 4,
          max.words=200, random.order=FALSE,
          rot.per=0,
          colors=brewer.pal(8, "Reds"))
#spacy_download_langmodel('pt')
spacy_initialize(model="pt_core_news_sm")
entities <- spacy_extract_entity(unlist(data))
head(entities)

# group entities by document
filtered_entities <- subset(entities, entities["ent_type"] == "ORG" |
                                entities["ent_type"] == "LOC" )
# filtered_entities <- subset(entities, entities["ent_type"] == "LOC" )
edges <- filtered_entities %>%
    group_by(doc_id) %>%
    summarise(entities = paste(text, collapse = ","))
# remove duplicated for the same document
edges <- lapply(str_split(edges$entities, ","),
                function(t){unique(unlist(t))})
# Auxiliary functions for creating adjancnt
get_adjacent_list <- function(edge_list) {
    if(length(edge_list) > 2)
        adjacent_matrix <- combinations(length(edge_list),
                                        2, edge_list)
}
adjacent_matrix <- edges %>%
    lapply(get_adjacent_list) %>%
    reduce(rbind)

#Gerar arquivo pro graphy
df <- as_tibble(adjacent_matrix, colnames=c('source', 'target'))
weighted_edgelist <- df %>%
    group_by(V1, V2) %>%
    summarise(weight=n())
news_graph <- weighted_edgelist %>% graph_from_data_frame(directed=F)
write_graph(news_graph, 'afogamento2.graphml', 'graphml')
