devtools::install_github("fkeck/subtools", force = TRUE)
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
library(resolution)
library(textdata)
library(reshape2)
library(wordcloud2)

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

#analisi preliminare
#distribuzione della durata degli episodi? o anche del numero d battute di ogni episodio
rM_numberBattute <- ds_senteces %>%
  count(Season, index=Episode)

rM_numberBattute

ggplot(rM_numberBattute, aes(index, n, fill = Season)) +
  facet_wrap(~Season, ncol = 2, scales = "free_x")+
  geom_line(aes(y = n), colour="blue", size=0.5, show.legend = FALSE)

#durata episodi
#??


#distribuzione dei termini negli episodi

occorrences <- ds_singleWord %>% count(word) %>% arrange(-n)
occorrences
totOccur <- ds_singleWord %>% summarize(words = n())
totOccur

ggplot(occorrences, aes(x = reorder(word, -n), y = n, group = 1)) + 
  geom_line(colour="blue", size=0.5, show.legend = FALSE) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#densità dei termini, devo rimuovere i termini sull'asse delle x

ggplot(occorrences, aes(x=n)) + geom_density(alpha=.3)

ggplot(occorrences, aes(x=word, y=n)) + 
  geom_point()

#occorrenze

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
ggraph(head(bigram_counts, 1000), layout="fr") +
  geom_edge_link(aes(edge_alpha=n), show.legend = F, arrow=arrows, end_cap=circle(0.03,'inches')) +
  geom_edge_density(aes(fill = n)) +
  geom_node_point(color="black", size=2) +
  geom_node_text(aes(label=name), repel=T) +
  labs(title="Bigram Network")


#cercare come risaltare colore dei nodi più importati
#cercare come rimuoverli dal grafo se sono piccoli
#togliere i nomi e tenre solo quelli più importanti

head(bigram_counts, 1000) %>%
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

#SOLO STAGIONE 1 BRO

word_cors <- ds_singleWord %>% #filter(Season == 1) %>% 
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, Episode, sort = TRUE)

struct <- word_cors %>%
  #filter(correlation > .25) %>%
  as_tbl_graph() 


struct %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 1) +
  geom_node_text(aes(label = name), repel = TRUE)

#misure di centralità
word_cors <- ds_singleWord %>% filter(Season == 1) %>% 
  group_by(word) %>%
  pairwise_cor(word, Episode, sort = TRUE)

#VEDIAMO LE CARATTERISTICHE DI QUESTO GRAFO GIGANTE
struct <- bigram_counts %>%
  graph_from_data_frame() 
options(ggrepel.max.overlaps = Inf)
vcount(struct)
mean_distance(struct)
diameter(struct)
distance_table(struct)

paths = distance_table(struct)$res
names(paths) = 1:length(paths)
barplot(paths / sum(paths), xlab="Distance", ylab="Frequency")


#VEDIAMO DA PRIMA CHE IL CAMMINO MINIMO TRA DUE NODI è BASSO, sta a 5, diametro max 19
#Una rete presenta un comportamento di tipo small world se e solo se L
#cresce in modo logaritmico (o inferiore) in funzione di n, dove n è il
#numero di nodi della rete. Il grado dei nodi del grafo ha un valore medio
#prefissato

L <- paths
C <- transitivity(struct, type="global")
L
C
# è una rete abbastanza grande (7106 nodi) con dei cammini minimi medi di tot ma
# la centralità di vicinanza non è alta, anzi è bassa, perchè ci sono moltissime parole che non 
# sono associate ad altre. 
ds_singleWord



#COSEEEEEEEEEEEEEEEEEE

is_connected(struct)

(c = components(struct))
nodes = which(c$membership == which.max(c$csize))

# color in red the nodes in the giant component
V(struct)$color = "white"
V(struct)[nodes]$color = "black"
plot(struct, vertex.size = 3, vertex.label=NA, edge.arrow.size=0.005, edge.arrow.width=0.0000001)

#COMPONENTE FORTEMENTE CONNESSA
c = components(struct, mode="strong")
nodes = which(c$membership == which.max(c$csize))
coords = layout_with_fr(struct)
# color in red the nodes in the giant component
V(struct)$color = "white"
V(struct)[nodes]$color = "black"
plot(g, layout=coords, 
     vertex.size = 6, vertex.label=NA, 
     edge.arrow.size = 1, edge.arrow.width=1)

#av <- word_cors %>% graph_from_data_frame()

#DA GUARDARE STA COSA
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



#community detection - ma posso fare anche con altri algoritmi

#VORREI RIMUOVERE I VERBI ZIO BEL

word_cors <- ds_singleWord %>% filter(Season == 1) %>% 
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, Episode, sort = TRUE)

word_cor_g <- word_cors %>%
  rename(word1 = item1, word2 = item2, n = correlation) %>%
  mutate(n = round(n*100)) %>%
  filter(n > 40)

g <- word_cor_g %>% 
  filter((word1 == "watch" | word2 == "watch" | word1 == "fun" | word2 == "fun" | word1 == "damn" | word2 == "damn"| word1 == "smith" | word2 == "smith") & !grepl(''', word1) & !grepl(''', word2) )

G = graph_from_data_frame(g)
community = cluster_resolution(G, t = 1) # The number of communities typically decreases as the resolution parameter (t) grows.
coords = layout_with_fr(G) 

plot(G, vertex.color = membership(community), layout = coords)


#NON SO SE VA

word_cors <- ds_singleWord %>% filter(Season == 1) %>% 
  group_by(word) %>%
  filter(n() >= 10) %>%
  pairwise_cor(word, Episode, sort = TRUE)

word_cor_g <- word_cors %>%
  rename(word1 = item1, word2 = item2, n = correlation) %>%
  mutate(n = round(n*100)) %>%
  filter(n > 10)

g <- word_cor_g %>% 
  filter((word1 == "summer" | word2 == "summer" | word1 == "rick" | word2 == "rick" | word1 == "beth" | word2 == "beth"| word1 == "morty" | word2 == "morty"| word1 == "Jerry" | word2 == "Jerry") & !grepl(''', word1) & !grepl(''', word2) )

G = graph_from_data_frame(g)
community = cluster_resolution(G, t = 1) # The number of communities typically decreases as the resolution parameter (t) grows.
coords = layout_with_fr(G) 

plot(G, vertex.color = membership(community), layout = coords)


#sentiment analisy 
#cambiare colori 

bing <- get_sentiments("bing")

ds_singleWord %>% 
  inner_join(bing, "word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  comparison.cloud(colors=c("#991D1D", "#327CDE"), max.words = 100)

cicioacom <- ds_singleWord %>% 
  inner_join(bing, "word") %>%
  count(word, sentiment, sort=T)

#nrc - molto più dettagliato e sono quasi uguali se uso nrc, con gli altri due invece proprio male
nrc <- get_sentiments("nrc")
sentiments <- ds_singleWord %>% 
  inner_join(nrc, "word") %>%
  count(sentiment, sort=T)

sentiments

sentiments %>% 
  ggplot(aes(x=reorder(sentiment, n), y=n)) +
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=F) +
  geom_label(label=sentiments$n) +
  labs(x="Sentiment", y="Frequency", title="How is the overall mood in Rick&Morty?") +
  coord_flip()

#altro2

ds_singleWord %>% 
  inner_join(nrc, "word") %>% 
  count(sentiment, word, sort=T) %>% 
  group_by(sentiment) %>% 
  arrange(desc(n)) %>% 
  slice(1:7) %>% 
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  coord_flip() +
  theme_bw() +
  labs(x="Word", y="Frequency", title="Sentiment split by most frequent words")





#alslsa

afinn <- get_sentiments("afinn")

ds_singleWord %>% 
  # by word and value count number of occurences
  inner_join(afinn, "word") %>% 
  count(word, value, sort=T) %>% 
  mutate(contribution = n * value,
         sentiment = ifelse(contribution<=0, "Negative", "Positive")) %>% #another variable
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  
  # plot
  ggplot(aes(x=reorder(word, contribution), y=contribution, fill=sentiment)) +
  geom_col(aes(fill=sentiment), show.legend = F) +
  labs(x="Word", y="Contribution", title="Words with biggest contributions in positive/negative moods") +
  coord_flip() +
  scale_fill_manual(values=c("#FA8072", "#08439A")) + 
  theme_bw()



#POSAOKADCA
rick_morty_sentiment2 <- ds_singleWord %>%
  inner_join(get_sentiments("bing")) %>%
  count(Season, index=Episode, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

rick_morty_sentiment2

ggplot(rick_morty_sentiment2, aes(index, sentiment, fill = Season)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Season, ncol = 2, scales = "free_x")


#ROBE SEPARATE PER STAGIONE
df2 <- melt(rick_morty_sentiment2 %>% 
              mutate(Negative = -negative) %>% 
              select(Season, positive, Negative), 
            id.vars='Season')

ggplot(df2, aes(x=variable, y=value, fill=factor(Season))) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_discrete(name="Season",
                      breaks=c(1, 2, 3, 4, 5),
                      labels=c("1", "2", "3", "4", "5"))+
  xlab("Sentiment")+ylab("Count")

#STUDIAMO I SINGOLI EPISODI DELLA STAGIONE 1
df2 <- melt(rick_morty_sentiment2 %>% 
              mutate(Negative = -negative) %>% 
              select(index, positive, Negative), 
            id.vars='index')

ggplot(df2, aes(x=variable, y=value, fill=factor(index))) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_discrete(name="Season",
                      breaks=c(1, 2, 3, 4, 5),
                      labels=c("1", "2", "3", "4", "5"))+
  xlab("Sentiment")+ylab("Count")

#check della positività di tutti gli episodi
#check della negatività rispetto a bing di tutti gli episodi
a <- rick_morty_sentiment2
ggplot(rick_morty_sentiment2 %>% mutate(Sigla = paste(a$Season, a$index)),
       aes(x=Sigla, index, y=positive, fill=factor(index))) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_discrete(name="Season",
                      breaks=c(1, 2, 3, 4, 5),
                      labels=c("1", "2", "3", "4", "5"))+
  xlab("Sentiment")+ylab("Count")


a <- rick_morty_sentiment2
ggplot(rick_morty_sentiment2 %>% mutate(Sigla = paste(a$Season, a$index)),
       aes(x=Sigla, index, y=-negative, fill=factor(index))) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_discrete(name="Season",
                      breaks=c(1, 2, 3, 4, 5),
                      labels=c("1", "2", "3", "4", "5"))+
  xlab("Sentiment")+ylab("Count")


#altro

wordcloud2(occorrences, size=0.9, minSize = 0.5, color='random-light', backgroundColor="black", shape="diamond", fontFamily="HersheySymbol")


#topic modelling?
freq <- ds_singleWord %>% filter(word == "summer") %>%
  count(Season, Episode) %>%
  mutate(Sigla = paste(a$Season, a$index)) %>%
  select(Sigla, n)

ggplot(freq,
       aes(x=Sigla, y=n, fill=factor(Sigla))) +
  geom_bar(stat='identity', position='dodge', show.legend=F) +
  xlab("Season Episode")+ylab("Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
