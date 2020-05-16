#LAVORO DI GRUPPO SOCIAL MEDIA
rm(list=ls())
reviews=read.csv("googleplaystore_user_reviews.csv")
sum(is.na(reviews))
dim(reviews)
table(reviews$App)
reviews=reviews[-which(is.na(reviews[,5])),]
dim(reviews)

head(sort(table(reviews$App),decreasing=T))

#Topic Modeling 
library(devtools)
library(TextWiller)
library(stopwords)

rec=reviews[,2]

# vettore lungo 113 (ogni cella del vettore contiene una delle 178 canzoni degli angry)
length(rec)
library(tm)
library(SnowballC)
library(wordcloud)

rec=gsub("fack","fuck",rec)
rec=gsub("angri","angry",rec)
rec=gsub("adds","ads",rec)
general_corpus = Corpus(VectorSource(rec))

general_corpus = tm_map(general_corpus, content_transformer(tolower))
general_corpus = tm_map(general_corpus, removeNumbers)
general_corpus = tm_map(general_corpus, removePunctuation)
stopwordsGrande=stopwords::stopwords("en",source="stopwords-iso")
length(stopwordsGrande)
general_corpus = tm_map(general_corpus, removeWords, c("the", "and", "aah", "chorus","echo", "oh", "yo", "yeah", "ha ha", 
                                                     "just","dont","like","ohyea","im","aint","youre","gonna",
                                                     "ah ah","wanna",stopwordsGrande,stopwords("english")))
general_corpus =  tm_map(general_corpus, stripWhitespace)

inspect(general_corpus[1:3])

require(tau)
require(lda)
library(topicmodels)

bigrams <- textcnt(general_corpus,method="string",n=2L,split="[[:blank:]]")
sort(bigrams,decreasing=TRUE)[1:20]

trigrams <- textcnt(general_corpus,method="string",n=3L,split="[[:blank:]]")
sort(trigrams,decreasing=TRUE)[1:10]

dtm=DocumentTermMatrix(general_corpus,
                       control = list( stemming = FALSE,
                                       stopwords="english", minWordLength = 2,
                                       removeNumbers = FALSE, removePunctuation = FALSE,
                                       bounds=list(local = c(1,Inf)) ))

q_lda <- LDA(dtm, k = 10, control = list(seed = 112358))
q_lda
# using VEM algorithm instead of Collapsed Gibbs Samplers

# rispetto all'algoritmo di prima con lda: il default di lda dentro topic models usa vem
# vem funziona meglio e quindi porta a risultati diversi

# stiamo cercando le probabilita per topic per parola
library(tidytext)
# con la matrice tidy fa vedere sempre solo le prime 10 righe della matrice
# The tidytext package provides this method for extracting the per-topic-per-word probabilities, 
# called "beta", from the model
q_topics <- tidy(q_lda, matrix = "beta")
q_topics
# the model computes the probability of that term being generated from that topic

# per la parola cosa vedo la probabilita di appartenere ad un certo topic
# caso milgiore: alcune probabilita sono superiori per diversi topic

library(ggplot2)
library(dplyr)
# We could use dplyr's top_n() to find the 10 terms that are most common within each topic. 
# As a tidy data frame, this lends itself well to a ggplot2 visualization
q_top_terms <- q_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>% # estrae 10 termini pi? associati a ciascun topic
  ungroup() %>%
  arrange(topic, -beta)
q_top_terms

q_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# problema: ci sono sempre i link url che risultano i pi? frequenti (sarebbe il caso di rimuoverli con le stop words)

# LDA also models each document as a mixture of topics. 
# We can examine the per-document-per-topic probabilities, called "gamma"
q_documents <- tidy(q_lda, matrix = "gamma")
q_documents # documenti da 1 a 10 rispetto al topic 1


# look at how the probabilities are distributed within each topic
ggplot(q_documents, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))
# come si distribuiscono i topic rispetto ai documenti

# find the topic that was most associated with each document
doc_classifications <- q_documents %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup()

doc_classifications # topic + frequentemente associato a ciascun documento

# find which words in each document were assigned to which topic
assignments <- augment(q_lda, data = dtm)
assignments

# check words in a document
tidy(dtm) %>%
  filter(document == 1) %>%
  arrange(desc(count))

general_corpus = tm_map(general_corpus, content_transformer(tolower))
general_corpus = tm_map(general_corpus, removeNumbers)
general_corpus = tm_map(general_corpus, removePunctuation)
stopwordsGrande=stopwords::stopwords("en",source="stopwords-iso")
length(stopwordsGrande)
general_corpus = tm_map(general_corpus, removeWords, c("the", "and", "aah", "chorus","echo", "oh", "yo", "yeah", "ha ha", 
                                                     "just","dont","like","ohyea","im","aint","youre","gonna",
                                                     "ah ah","wanna",stopwordsGrande,stopwords("english")))
general_corpus =  tm_map(general_corpus, stripWhitespace)
general_corpus=as.vector(general_corpus)
# applico il sentiment (vocabolario personalizzato italiano)
general_sentiment=sentiment(general_corpus$content)
# per ogni parola va dentro il vocabolario di text willer per vedere se ? positiva o negativa
# vedo come si distribuisce il sentiment
prop.table(table(general_sentiment))  # % di tweet tra negativi, neutri, positivi
barplot(table(general_sentiment),col=c("red","grey","blue"))

# si sono diverse le medie


library(tm)
#prova=normalizzaTesti(tweets$text)
#corpus=Corpus(VectorSource(tweets$text))
dtm=DocumentTermMatrix(general_corpus,
                       control = list( stemming = TRUE,
                                       stopwords="english", minWordLength = 2,
                                       removeNumbers = TRUE, removePunctuation = TRUE,
                                       bounds=list(local = c(1,Inf)) ))
#tibble(prova)
# conversione tdm al formato tidy
library(dplyr)
library(tidytext)
library(tidyr)

tidy_se <- tidy(dtm)
tidy_se # count: conteggio delle frequenze nel segmento
names(tidy_se)[2]="word" # chiamo la seconda colonna word invece al posto di term


# uso le parole categorizzate in positive e negative dal lexicon bing
# bing classifica le parole in positiva/negativa
se_sentiment <- tidy_se %>%
  inner_join(get_sentiments("bing")) %>% # prendo dal lexicon bing
  count(document, sentiment) %>% # conteggio il sentiment all'interno del documento
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) # se differenza positiva ottengo un sentiment positivo e viceversa

se_sentiment

# contributo di ogni parola al sentiment
bing_word_counts <- tidy_se %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

# graficamente con le top 10
library(ggplot2)
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
# parole che pesano di + nell'intero corpus per quanto riguarda il sentimento
# stiamo usando un lexicon generico, quindi non si capisce se le parole siano state classificate bene
# bisognerebbe costruirsi un vocabolario apposito per la sharing economy

# tramite wordcloud proietto le parole pi? frequenti
library(wordcloud)

tidy_se %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# aggiungo il sentiment per osservare il contrasto
library(reshape2)
# differenzia le parole positive e negative all'interno del wordcloud
# chiare positive, scure sono negative

tidy_se %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "#00B2FF"),
                   max.words = 100)

