devtools::install_github("fkeck/subtools")
library(tidytext)
library(tidyverse)
library(tidygraph)
library(subtools)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(igraph)
library(ggraph)
library(widyr)

dataF <- read_subtitles_season(dir = "C:/flutter_pro/AdvancedDataScience/ddd/")
ds_noTag <- clean_tags(dataF)
ds_noCap <- clean_captions(ds_noTag)
ds_noCap <- clean_patterns(ds_noCap, "gonna")

#cerchiamo le parole
ds_singleWord <- unnest_tokens(ds_noCap, word, Text_content) %>% rename(word = "word") %>% select(Season, Episode, word) %>% anti_join(stop_words)
ds_senteces <- ds_noCap %>% rename(Sentence = "Text_content") %>% select(Season, Episode, Sentence)

#wordcloud

wordcloud_1 <- ds_singleWord %>% count(word) %>%
  with(wordcloud(words = word, freq = n, min.freq = 1, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")))


occorrences <- ds_singleWord %>% count(word) %>% arrange(-n)
occorrences
totOccur <- ds_singleWord %>% summarize(words = n())
totOccur

parole <- ds_singleWord %>% semi_join(head(occorrences, 10))

ggplot(head(occorrences, 10), aes(x = reorder(word, -n), y = n)) + geom_bar(stat = "identity")

#bigrams

bigrams <- unnest_tokens(ds_senteces, bigram, Sentence, token = "ngrams", n = 2)
bigrams <- bigrams %>% filter(!is.na(bigram))

bigrams %>% count(bigram, sort = TRUE)

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

bigram_counts2 <- bigrams_united %>% 
  count(bigram, sort = TRUE)

bigram_counts2




#trigrams

trigrams <- unnest_tokens(ds_senteces, trigram, Sentence, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

trigrams

trigrams_united <- trigrams %>%
  unite(trigram, word1, word2, word3, sep = " ")

trigrams_united



#tf-idf

bigram_tf_idf <- bigrams_united %>%
  count(Season, bigram) %>%
  bind_tf_idf(bigram, Season, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(Season) %>%
  top_n(6, tf_idf) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = Season)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Season, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "tf-idf of bigram to season",
       x = "")


#pairwise
arrows <- grid::arrow(type="closed", length=unit(0.15, "inches"))
ggraph(head(bigram_counts, 70), layout="fr") +
  geom_edge_link(aes(edge_alpha=n), show.legend = F, arrow=arrows, end_cap=circle(0.03,'inches')) +
  geom_edge_density(aes(fill = n)) +
  geom_node_point(color="black", size=2) +
  geom_node_text(aes(label=name), repel=T) +
  labs(title="Bigram Network")


#cercare come risaltare colore dei nodi più importati
#cercare come rimuoverli dal grafo se sono piccoli

head(bigram_counts, 400) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link0()+
  geom_node_point(color = "lightblue", size = 1) +
  geom_node_text(aes(label = name), vjust = 1, size=3, col = "darkgreen") +
  labs(title="Bigram Network")


#correlazione e quelli più correlati
word_pairs <- ds_singleWord %>% filter(Season == 1) %>% 
  pairwise_count(word, Episode, sort = TRUE)

word_pairs


#correlazione con anche indice di quanto sono uguali, se cambio 
#il filter cambia tutto
word_cors <- ds_singleWord %>% filter(Season == 1) %>% 
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, Episode, sort = TRUE)


word_cors %>%
  filter(correlation > .25) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 1) +
  geom_node_text(aes(label = name), repel = TRUE)

#misure di centralità
word_cors <- ds_singleWord %>% filter(Season == 1) %>% 
  group_by(word) %>%
  pairwise_cor(word, Episode, sort = TRUE)


#av <- word_cors %>% graph_from_data_frame()


word_cor_g <- word_cors %>%
  rename(word1 = item1, word2 = item2, n = correlation) %>%
  mutate(n = round(n*100)) %>%
  filter(n > 18)

#Betweenness centrality
g <- word_cor_g %>%
  as_tbl_graph()

v <- as_tibble(g) %>%
  mutate(v = row_number())

b <- betweenness(g)

names(b) = 1:vcount(g)

betweenness <- data.frame(score = round(b, 2)) %>%
  mutate(v = row_number()) %>%
  full_join(v) %>%
  arrange(desc(score)) %>%
  mutate(word = name) %>%
  select(word, score) %>% 
  head(10)

betweenness

#PageRank centrality
pr <- page_rank(g)
pagerank <- data.frame(score = pr$vector) %>%
  arrange(desc(score)) %>%
  head(10)

pagerank

#community detection

#VORREI RIMUOVERE I VERBI ZIO BEL

g <- word_cor_g %>% 
  filter((word1 == "watch" | word2 == "watch" | word1 == "fun" | word2 == "fun" | word1 == "damn" | word2 == "damn"| word1 == "smith" | word2 == "smith") & !grepl(''', word1) & !grepl(''', word2) )

G = graph_from_data_frame(g)
community = cluster_resolution(G, t = 1.5) # The number of communities typically decreases as the resolution parameter (t) grows.
coords = layout_with_fr(G) 

plot(G, vertex.color = membership(community), layout = coords)
