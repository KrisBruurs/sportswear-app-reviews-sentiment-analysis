###---Libraries---###
library(tidyverse)
library(tibble)   
library(stringr)   
library(tidytext)  
library(textstem)  
library(tokenizers) 
library(reshape2) 
library(wordcloud)

###---Input---###
df <- read_csv('data/reviews.csv')

###---Processing---###

# Add manual column with positive, negative, neutral rating based on stars

df <- df %>% 
  rowid_to_column('id') %>% 
  mutate(
    sentiment_star = case_when(
    score < 3 ~ 'negative',
    score == 3 ~ 'neutral',
    score > 3 ~ 'positive'
  ))

# Add columns with number of words/characters
df <- df %>% 
  mutate(n_words = count_words(content),
         n_char = nchar(content))

# Plot number of words on histogram to inspect data
df %>%
  ggplot() +
  geom_histogram(
    aes(x = n_words)
  ) +
  theme_minimal() +
  scale_x_continuous()

df %>% 
  summarise(
    mean_words = mean(n_words),
    mean_char = mean(n_char)
  )

# Turn reviews into tokens
tokens <- df %>% 
  unnest_tokens(word, content) # word is the level, review is column

# Find most common words
common_words <- tokens %>% # Create dataset of most common words
  group_by(word) %>% 
  count(sort = TRUE)

# Remove standard stopwords
tokens_no_stop <- tokens %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[[:digit:]]+"))

# Check most common words again for manual removal
common_words_2 <- tokens_no_stop %>% 
  group_by(word) %>% 
  count(sort = TRUE)

# Create custom stopwords 
custom_stop_words <-  # words that have no real value, all reviews are about doctors
  tibble(
    word = c(
      'app'
    ),
    lexicon = 'docs' # Create lexicon !!!!
  )

# Remove custom stopwords
tokens_no_stop <- tokens_no_stop %>%
  anti_join(custom_stop_words)

# Plot Common Words after stopword removal
tokens_no_stop %>%
  group_by(word) %>% 
  count(sort = TRUE) %>% 
  ungroup() %>% 
  top_n(25) %>%
  ggplot(aes(x = n, 
             y = reorder(word, n)
  )
  ) +
  geom_col() +
  scale_y_reordered() +
  labs(y = NULL)

# Stemming
tokens_stemmed <- tokens_no_stop %>% 
  mutate(word_stem = stem_words(word), 
         word_lemma = lemmatize_words(word)) 

###---Output---###
write_csv(tokens_stemmed, 'data/tokenized_reviews.csv')
write_csv(df, 'data/cleaned_reviews.csv')
