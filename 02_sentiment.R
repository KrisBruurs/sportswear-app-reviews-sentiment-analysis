###---Libraries---###
library(tidyverse)
library(tibble)     
library(stringr)    
library(tidytext)  
library(vader)   
library(tokenizers)
library(textdata) 
library(textstem)  
library(yardstick)  

###---Input---###
tokens <- read_csv('data/tokenized_reviews.csv')
reviews <- read_csv('data/cleaned_reviews.csv')

###---Process---###

# Sentiment Bing
tokens_bing <- tokens %>% 
  left_join(get_sentiments('bing')) %>% 
  mutate(sentiment = replace_na(sentiment, 'neutral'))

sentiment_bing <- tokens_bing %>% 
  group_by(id) %>% 
  count(sentiment) %>% 
  ungroup() %>% 
  pivot_wider( # you will normally Google this
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  ) %>% 
  mutate(
    sentiment_bing = case_when(
      positive > negative ~ "positive",
      negative > positive ~ "negative",
      .default = "neutral"
    )
  )

# Compare with sentiment star
comparison_df <- reviews %>% 
  select(id, sentiment_star) %>% 
  left_join(sentiment_bing, by = "id") %>% 
  select(id, sentiment_star, sentiment_bing) %>% 
  mutate(
    sentiment_star = as.factor(sentiment_star),
    sentiment_bing = as.factor(sentiment_bing)
  )


accuracy(comparison_df,
         sentiment_star,
         sentiment_bing)

conf_mat(comparison_df,
         sentiment_star,
         sentiment_bing)

# Sentiment Afinn
tokens_afinn <- tokens %>% 
  left_join(get_sentiments('afinn')) %>% 
  mutate(value = replace_na(value, 0))

sentiment_afinn <- tokens_afinn %>% 
  group_by(id) %>% 
  summarise(score = sum(value, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(sentiment_afinn = case_when(
    score > 0 ~ 'positive',
    score < 0 ~ 'negative',
    .default = 'neutral'
  )) %>% 
  select(!score)

comparison_df <- comparison_df %>% 
  left_join(sentiment_afinn, by = 'id') 

comparison_df <- comparison_df %>% 
  mutate(sentiment_afinn = as.factor(sentiment_afinn))


accuracy(comparison_df, 
         sentiment_star,
         sentiment_afinn)

conf_mat(comparison_df,
         sentiment_star,
         sentiment_afinn)

# compare two methods to themselves
accuracy(comparison_df,
         sentiment_bing,
         sentiment_afinn)

conf_mat(comparison_df,
         sentiment_bing,
         sentiment_afinn)

# Sentiment VADER
vader_sent <- vader_df(reviews$content)

vader_sent2 <- vader_sent %>% 
  rowid_to_column('id') %>% 
  mutate(sentiment_vader = case_when(
    compound > 0.05 ~ 'positive',
    compound < -0.05 ~ 'negative',
    .default = 'neutral'
  )) %>% 
  select(id, sentiment_vader) %>% 
  mutate(
    sentiment_vader = as.factor(sentiment_vader))

comparison_df <- comparison_df %>% 
  left_join(vader_sent2, by = 'id')

accuracy(comparison_df,
         sentiment_star,
         sentiment_vader)

conf_mat(comparison_df, 
         sentiment_star,
         sentiment_vader)

sentiment_data <- vader_sent2 %>% 
  left_join(reviews, by ='id')

sentiment_tokens <- vader_sent2 %>% 
  left_join(tokens, by ='id')

###---Output---###
write_csv(sentiment_data, 'data/sentiment_data.csv')
write_csv(sentiment_tokens, 'data/sentiment_tokens.csv')
