###---Libraries---###
library(stm)
library(tidyverse)
library(tibble)
library(tidytext)

###---Input---###
tokens <- read_csv('data/sentiment_tokens.csv') %>% 
  drop_na(word_lemma)


###---Processing---###

# Split between brands AND positive/negative reviews
adidas_pos <- tokens %>%
  filter(brand == "adidas" & sentiment_vader == "positive")

adidas_neg <- tokens %>%
  filter(brand == "adidas" & sentiment_vader == "negative")

nike_pos <- tokens %>%
  filter(brand == "nike" & sentiment_vader == "positive")

nike_neg <- tokens %>%
  filter(brand == "nike" & sentiment_vader == "negative")

gymshark_pos <- tokens %>%
  filter(brand == "gymshark" & sentiment_vader == "positive")

gymshark_neg <- tokens %>%
  filter(brand == "gymshark" & sentiment_vader == "negative")

puma_pos <- tokens %>% 
  filter(brand == 'puma' & sentiment_vader == "positive")

puma_neg <- tokens %>% 
  filter(brand == 'puma' & sentiment_vader == 'negative')

###---ADIDAS---###

# Establish vocab list to reduce noise
adidas_word_count <- adidas %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 4)


adidas <- adidas %>% 
  filter(word_lemma %in% adidas_word_count$word_lemma)

# Create doc-term matrix
adidas_doc_word_count <- adidas %>% 
  count(id, word_lemma) %>% 
  ungroup()

adidas_cardio_dtm <- adidas_doc_word_count %>% 
  cast_sparse(id, word_lemma, n)

# Model
adidas_cardio_topics <- stm( 
  adidas_cardio_dtm,
  K = 10,
  seed = 1234567890
)

# Explore output
labelTopics(adidas_cardio_topics) 

###---NIKE---###

# Establish vocab list to reduce noise
nike_word_count <- nike %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 4)

nike <- nike %>% 
  filter(word_lemma %in% nike_word_count$word_lemma)

# Create doc-term matrix
nike_doc_word_count <- nike %>% 
  count(id, word_lemma) %>% 
  ungroup()

nike_cardio_dtm <- nike_doc_word_count %>% 
  cast_sparse(id, word_lemma, n)

# Model
nike_cardio_topics <- stm( 
  nike_cardio_dtm,
  K = 10,
  seed = 1234567890
)

# Explore output
labelTopics(nike_cardio_topics) 

###---PUMA---###
# Establish vocab list to reduce noise
puma_word_count <- puma %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 4)

puma <- puma %>% 
  filter(word_lemma %in% puma_word_count$word_lemma)

# Create doc-term matrix
puma_doc_word_count <- puma %>% 
  count(id, word_lemma) %>% 
  ungroup()

puma_cardio_dtm <- puma_doc_word_count %>% 
  cast_sparse(id, word_lemma, n)

# Model
puma_cardio_topics <- stm( 
  puma_cardio_dtm,
  K = 10,
  seed = 1234567890
)

# Explore output
labelTopics(puma_cardio_topics) 

###---GYMSHARK---###

# Establish vocab list to reduce noise
gymshark_word_count <- gymshark %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 4)

gymshark <- gymshark %>% 
  filter(word_lemma %in% gymshark_word_count$word_lemma)

# Create doc-term matrix
gymshark_doc_word_count <- gymshark %>% 
  count(id, word_lemma) %>% 
  ungroup()

gymshark_cardio_dtm <- gymshark_doc_word_count %>% 
  cast_sparse(id, word_lemma, n)

# Model
gymshark_cardio_topics <- stm( 
  gymshark_cardio_dtm,
  K = 10,
  seed = 1234567890
)

# Explore output
labelTopics(gymshark_cardio_topics) 
