library(topicmodels)
library(tidytext)
library(ggplot2)
library(tidyverse)
data("AssociatedPress")
AssociatedPress

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda


ap_topics <- tidy(ap_lda, matrix="beta")
ap_topics

ap_top_terms <- ap_topics %>%
                group_by(topic) %>%
                top_n(10, beta) %>% 
                ungroup %>%
                arrange(topic, -beta)

ap_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) + facet_wrap(~ topic, scales = "free") +
        coord_flip() +
        scale_x_reordered()

beta_spread <- ap_topics %>% 
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > .001 | topic2 > .001) %>%
    mutate(log_ratio = log2(topic2 / topic1))

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

tidy(AssociatedPress) %>%
    filter(document == 6) %>%
    arrange(desc(count))

