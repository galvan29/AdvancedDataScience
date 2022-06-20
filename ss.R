devtools::install_github("fkeck/subtools")
library(tidytext)
library(subtools)
library(ggplot2)
library(dplyr)

dataF <- read_subtitles_season(dir = "C:/flutter_pro/AdvancedDataScience/ddd/")
ds_noTag <- clean_tags(dataF)
ds_noCap <- clean_captions(ds_noTag)
ds_noCap <- clean_patterns(ds_noCap, "gonna")

#cerchiamo le parole
ds_singleWord <- unnest_tokens(ds_noCap, word, Text_content) %>% rename(word = "word") %>% select(Season, Episode, word) %>% anti_join(stop_words)
ds_senteces <- ds_noCap %>% rename(Sentece = "Text_content") %>% select(Season, Episode, Sentece)

wordcloud_1 <- ds_singleWord %>% count(word) %>%
  with(wordcloud(words = word, freq = n, min.freq = 1, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")))


occorrences <- ds_singleWord %>% count(word) %>% arrange(-n)
occorrences
totOccur <- ds_singleWord %>% summarize(words = n())
totOccur

parole <- ds_singleWord %>% semi_join(head(occorrences, 10))

ggplot(head(occorrences, 10), aes(x = reorder(word, -n), y = n)) + geom_bar(stat = "identity")
