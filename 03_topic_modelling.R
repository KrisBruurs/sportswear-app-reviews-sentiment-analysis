###---Libraries---###
library(stm)
library(tidyverse)
library(tibble)
library(tidytext)
library(httr2)

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

###---ADIDAS_POS---###
# Establish vocab list to reduce noise
adidas_pos_word_count <- adidas_pos %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 4)

adidas_pos <- adidas_pos %>% 
  filter(word_lemma %in% adidas_pos_word_count$word_lemma)

# Create doc-term matrix
adidas_pos_doc_word_count <- adidas_pos %>% 
  count(id, word_lemma) %>% 
  ungroup()

adidas_pos_dtm <- adidas_pos_doc_word_count %>% 
  cast_sparse(id, word_lemma, n)

# Model
adidas_pos_topics <- stm( 
  adidas_pos_dtm,
  K = 10,
  seed = 1234567890
)

# Explore output
labelTopics(adidas_pos_topics)

AP_td_beta <- tidy(adidas_pos_topics) 

AP_td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic - ADIDAS POSITIVE",
       subtitle = "Different words are associated with different topics")

# Assigning Topics to Reviews
adidas_pos_td_gamma <-
  tidy(adidas_pos_topics,
       matrix = "gamma",
       document_names = rownames(adidas_pos_dtm)
  ) %>%
  arrange(as.numeric(document), desc(gamma))

adidas_pos_reviews_gamma <-
  adidas_pos_td_gamma %>%
  rename(id = document) %>%
  mutate(id = as.numeric(id)) %>%
  group_by(id) %>%
  slice_max(gamma) %>%
  select(-gamma)


###---ADIDAS_NEG---###
# Establish vocab list to reduce noise
adidas_neg_word_count <- adidas_neg %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 4)

adidas_neg <- adidas_neg %>% 
  filter(word_lemma %in% adidas_neg_word_count$word_lemma)

# Create doc-term matrix
adidas_neg_doc_word_count <- adidas_neg %>% 
  count(id, word_lemma) %>% 
  ungroup()

adidas_neg_dtm <- adidas_neg_doc_word_count %>% 
  cast_sparse(id, word_lemma, n)

# Model
adidas_neg_topics <- stm( 
  adidas_neg_dtm,
  K = 10,
  seed = 1234567890
)

# Explore output
labelTopics(adidas_neg_topics)

AN_td_beta <- tidy(adidas_neg_topics) 

AN_td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic - ADIDAS NEGATIVE",
       subtitle = "Different words are associated with different topics")

# Assigning Topics to Reviews
adidas_neg_td_gamma <-
  tidy(adidas_neg_topics,
       matrix = "gamma",
       document_names = rownames(adidas_neg_dtm)
  ) %>%
  arrange(as.numeric(document), desc(gamma))

adidas_neg_reviews_gamma <-
  adidas_neg_td_gamma %>%
  rename(id = document) %>%
  mutate(id = as.numeric(id)) %>%
  group_by(id) %>%
  slice_max(gamma) %>%
  select(-gamma)


###---NIKE_POS---###
# Establish vocab list to reduce noise
nike_pos_word_count <- nike_pos %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 4)

nike_pos <- nike_pos %>% 
  filter(word_lemma %in% nike_pos_word_count$word_lemma)

# Create doc-term matrix
nike_pos_doc_word_count <- nike_pos %>% 
  count(id, word_lemma) %>% 
  ungroup()

nike_pos_dtm <- nike_pos_doc_word_count %>% 
  cast_sparse(id, word_lemma, n)

# Model
nike_pos_topics <- stm( 
  nike_pos_dtm,
  K = 10,
  seed = 1234567890
)

# Explore output
labelTopics(nike_pos_topics)

NP_td_beta <- tidy(nike_pos_topics) 

NP_td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic - NIKE POSITIVE",
       subtitle = "Different words are associated with different topics")

# Assigning Topics to Reviews 
nike_pos_td_gamma <-
  tidy(nike_pos_topics,
       matrix = "gamma",
       document_names = rownames(nike_pos_dtm)
  ) %>%
  arrange(as.numeric(document), desc(gamma))

nike_pos_reviews_gamma <-
  nike_pos_td_gamma %>%
  rename(id = document) %>%
  mutate(id = as.numeric(id)) %>%
  group_by(id) %>%
  slice_max(gamma) %>%
  select(-gamma)


###---NIKE_NEG---###
# Establish vocab list to reduce noise
nike_neg_word_count <- nike_neg %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 4)

nike_neg <- nike_neg %>% 
  filter(word_lemma %in% nike_neg_word_count$word_lemma)

# Create doc-term matrix
nike_neg_doc_word_count <- nike_neg %>% 
  count(id, word_lemma) %>% 
  ungroup()

nike_neg_dtm <- nike_neg_doc_word_count %>% 
  cast_sparse(id, word_lemma, n)

# Model
nike_neg_topics <- stm( 
  nike_neg_dtm,
  K = 10,
  seed = 1234567890
)

# Explore output
labelTopics(nike_neg_topics)

NN_td_beta <- tidy(nike_neg_topics) 

NN_td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic - NIKE NEGATIVE",
       subtitle = "Different words are associated with different topics")

# Assigning Topics to Reviews 
nike_neg_td_gamma <-
  tidy(nike_neg_topics,
       matrix = "gamma",
       document_names = rownames(nike_neg_dtm)
  ) %>%
  arrange(as.numeric(document), desc(gamma))

nike_neg_reviews_gamma <-
  nike_neg_td_gamma %>%
  rename(id = document) %>%
  mutate(id = as.numeric(id)) %>%
  group_by(id) %>%
  slice_max(gamma) %>%
  select(-gamma)


###---PUMA_POS---###
# Establish vocab list to reduce noise
puma_pos_word_count <- puma_pos %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 4)

puma_pos <- puma_pos %>% 
  filter(word_lemma %in% puma_pos_word_count$word_lemma)

# Create doc-term matrix
puma_pos_doc_word_count <- puma_pos %>% 
  count(id, word_lemma) %>% 
  ungroup()

puma_pos_dtm <- puma_pos_doc_word_count %>% 
  cast_sparse(id, word_lemma, n)

# Model
puma_pos_topics <- stm( 
  puma_pos_dtm,
  K = 10,
  seed = 1234567890
)

# Explore output
labelTopics(puma_pos_topics)

PP_td_beta <- tidy(puma_pos_topics) 

PP_td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic - PUMA POSITIVE",
       subtitle = "Different words are associated with different topics")

# Assigning Topics to Reviews
puma_pos_td_gamma <-
  tidy(puma_pos_topics,
       matrix = "gamma",
       document_names = rownames(puma_pos_dtm)
  ) %>%
  arrange(as.numeric(document), desc(gamma))

puma_pos_reviews_gamma <-
  puma_pos_td_gamma %>%
  rename(id = document) %>%
  mutate(id = as.numeric(id)) %>%
  group_by(id) %>%
  slice_max(gamma) %>%
  select(-gamma)


###---PUMA_NEG---###
# Establish vocab list to reduce noise
puma_neg_word_count <- puma_neg %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 4)

puma_neg <- puma_neg %>% 
  filter(word_lemma %in% puma_neg_word_count$word_lemma)

# Create doc-term matrix
puma_neg_doc_word_count <- puma_neg %>% 
  count(id, word_lemma) %>% 
  ungroup()

puma_neg_dtm <- puma_neg_doc_word_count %>% 
  cast_sparse(id, word_lemma, n)

# Model
puma_neg_topics <- stm( 
  puma_neg_dtm,
  K = 10,
  seed = 1234567890
)

# Explore output
labelTopics(puma_neg_topics)

PN_td_beta <- tidy(puma_neg_topics) 

PN_td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic - PUMA NEGATIVE",
       subtitle = "Different words are associated with different topics")

# Assigning Topics to Reviews
puma_neg_td_gamma <-
  tidy(puma_neg_topics,
       matrix = "gamma",
       document_names = rownames(puma_neg_dtm)
  ) %>%
  arrange(as.numeric(document), desc(gamma))

puma_neg_reviews_gamma <-
  puma_neg_td_gamma %>%
  rename(id = document) %>%
  mutate(id = as.numeric(id)) %>%
  group_by(id) %>%
  slice_max(gamma) %>%
  select(-gamma)


###---GYMSHARK_POS---###
# Establish vocab list to reduce noise
gymshark_pos_word_count <- gymshark_pos %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 4)

gymshark_pos <- gymshark_pos %>% 
  filter(word_lemma %in% gymshark_pos_word_count$word_lemma)

# Create doc-term matrix
gymshark_pos_doc_word_count <- gymshark_pos %>% 
  count(id, word_lemma) %>% 
  ungroup()

gymshark_pos_dtm <- gymshark_pos_doc_word_count %>% 
  cast_sparse(id, word_lemma, n)

# Model
gymshark_pos_topics <- stm( 
  gymshark_pos_dtm,
  K = 10,
  seed = 1234567890
)

# Explore output
labelTopics(gymshark_pos_topics)

GP_td_beta <- tidy(gymshark_pos_topics) 

GP_td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic - GYMSHARK POSITIVE",
       subtitle = "Different words are associated with different topics")

# Assigning Topics to Reviews
gymshark_pos_td_gamma <-
  tidy(gymshark_pos_topics,
       matrix = "gamma",
       document_names = rownames(gymshark_pos_dtm)
  ) %>%
  arrange(as.numeric(document), desc(gamma))

gymshark_pos_reviews_gamma <-
  gymshark_pos_td_gamma %>%
  rename(id = document) %>%
  mutate(id = as.numeric(id)) %>%
  group_by(id) %>%
  slice_max(gamma) %>%
  select(-gamma)


###---GYMSHARK_NEG---###
# Establish vocab list to reduce noise
gymshark_neg_word_count <- gymshark_neg %>% 
  group_by(word_lemma) %>% 
  count(sort = TRUE) %>% 
  filter(n >= 4)

gymshark_neg <- gymshark_neg %>% 
  filter(word_lemma %in% gymshark_neg_word_count$word_lemma)

# Create doc-term matrix
gymshark_neg_doc_word_count <- gymshark_neg %>% 
  count(id, word_lemma) %>% 
  ungroup()

gymshark_neg_dtm <- gymshark_neg_doc_word_count %>% 
  cast_sparse(id, word_lemma, n)

# Model
gymshark_neg_topics <- stm( 
  gymshark_neg_dtm,
  K = 10,
  seed = 1234567890
)

# Explore output
labelTopics(gymshark_neg_topics)

GN_td_beta <- tidy(gymshark_neg_topics) 

GN_td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic - GYMSHARK NEGATIVE",
       subtitle = "Different words are associated with different topics")

# Assigning Topics to Reviews
gymshark_neg_td_gamma <-
  tidy(gymshark_neg_topics,
       matrix = "gamma",
       document_names = rownames(gymshark_neg_dtm)
  ) %>%
  arrange(as.numeric(document), desc(gamma))

gymshark_neg_reviews_gamma <-
  gymshark_neg_td_gamma %>%
  rename(id = document) %>%
  mutate(id = as.numeric(id)) %>%
  group_by(id) %>%
  slice_max(gamma) %>%
  select(-gamma)

###---TOPIC_NAMES---###



# --- get top FREX keywords per topic from an STM model ---
top_frex_terms <- function(model, n_terms = 4) {
  lt <- labelTopics(model, n = n_terms)        # lt$frex is K x n_terms
  # return a list of character vectors, one per topic
  apply(lt$frex, 1, function(row) row[!is.na(row)])
}

# --- call local Ollama to get a short label for one topic ---
ollama_label_once <- function(keywords, model = "mistral", max_tokens = 16, timeout_sec = 60) {
  prompt <- paste0(
    "Return a concise 2-4 word topic label. Only the label, no punctuation.\n",
    "Keywords: ", paste(keywords, collapse = ", ")
  )
  
  req <- request("http://localhost:11434/api/generate") |>
    req_body_json(list(
      model  = model,
      prompt = prompt,
      stream = FALSE,
      options = list(num_predict = max_tokens)
    )) |>
    req_timeout(timeout_sec) |>
    req_error(is_error = function(resp) FALSE)
  
  resp <- req_perform(req)
  
  # parse
  out <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
  lbl <- if (is.null(out)) "" else trimws(out$response)
  
  # light cleanup: keep readable label tokens
  lbl <- gsub("[^[:alnum:] /&-]", "", lbl)
  lbl <- gsub("\\s+", " ", lbl)
  if (identical(lbl, "")) "Misc" else lbl
}

# --- label all topics for a given STM model via Ollama ---
label_topics_ollama <- function(model, n_terms = 4, lm = "mistral", sleep_sec = 0.2) {
  terms_list <- top_frex_terms(model, n_terms)
  labs <- map_chr(terms_list, function(kw) {
    lab <- ollama_label_once(kw, model = lm)
    Sys.sleep(sleep_sec)   # be polite to the local server
    lab
  })
  tibble(topic = seq_along(labs), topic_name = labs)
}

adidas_pos_topic_names    <- label_topics_ollama(adidas_pos_topics,    n_terms = 4, lm = "mistral")


