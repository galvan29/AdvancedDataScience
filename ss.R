devtools::install_github("fkeck/subtools")
library(tidytext)
library(tidyverse)
library(subtools)
library(ggplot2)
library(dplyr)
library(wordcloud)

dataF <- read_subtitles_season(dir = "C:/flutter_pro/AdvancedDataScience/ddd/")
ds_noTag <- clean_tags(dataF)
ds_noCap <- clean_captions(ds_noTag)
ds_noCap <- clean_patterns(ds_noCap, "gonna")

#cerchiamo le parole
ds_singleWord <- unnest_tokens(ds_noCap, word, Text_content) %>% rename(word = "word") %>% select(Season, Episode, word) %>% anti_join(stop_words)
ds_senteces <- ds_noCap %>% rename(Sentence = "Text_content") %>% select(Season, Episode, Sentence)

wordcloud_1 <- ds_singleWord %>% count(word) %>%
  with(wordcloud(words = word, freq = n, min.freq = 1, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")))


occorrences <- ds_singleWord %>% count(word) %>% arrange(-n)
occorrences
totOccur <- ds_singleWord %>% summarize(words = n())
totOccur

parole <- ds_singleWord %>% semi_join(head(occorrences, 10))

ggplot(head(occorrences, 10), aes(x = reorder(word, -n), y = n)) + geom_bar(stat = "identity")


bigrams <- unnest_tokens(ds_senteces, bigram, Sentence, token = "ngrams", n = 2)
bigrams <- bigrams %>% filter(!is.na(bigram))

bigrams %>% count(bigram, sort = TRUE)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts




