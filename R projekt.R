library(tidyverse)
library(tidytext)
library(tm)
library(textstem)
library(hunspell)
library(rvest)
library(stringr)
library(RSelenium)
library(wordcloud)
library(wordcloud2)
library(plotrix)
library(lubridate)
library(topicmodels)
library(reshape2)
library(textdata)


base_url_pt1 <- "https://cryptonews.net/?page="

n_index_pages <- 25

get_article_list_from_page <- function(page_no) {
  page_url <- paste0(base_url_pt1, page_no)
  
  page <- read_html(page_url)
  
  links <- page %>%
    html_node("section") %>%
    html_nodes("div.desc.col-xs")
  
  articles_tmp <- tibble(link=paste0("https://cryptonews.net",url = links %>% html_node("a") %>% html_attr("href")),
                             title =  links %>% html_node("a") %>% html_text())
  
  return(articles_tmp)
}

article_links <- tibble()

for(i in 25:n_index_pages) {
  article_links_tmp <- get_article_list_from_page(i)
  
  article_links <- bind_rows(article_links, article_links_tmp)

  Sys.sleep(sample(seq(0.25, 1, 0.5), 1))
  
  print(i)
}

rm(article_links_tmp, i)

webpage <- read_html(encoding = "ISO_8859-2", 'https://cryptonews.net/news/other/21113274/')
webpage

results <- webpage %>% html_nodes(".short-desc")
results

safe_read_html <- safely(read_html)

get_article <- function(art_url) {
  try(page <- safe_read_html(art_url, encoding = "iso-8859-2"))
  
  if(is.null(page$result)) {
    return(tibble())
  }
  
  page <- page$result
  
  #author <- page %>% html_node('span.source-host') %>% html_text() %>% trimws()
  
  date <- page %>% html_node("span.datetime.flex.middle-xs") %>% html_text() %>% trimws() %>% dmy_hm()
  
  title <- page %>%html_node("h1") %>% html_text() %>% trimws()
  
  lead <- page %>% html_node("div.news-item.detail.content_text") %>% html_node("p") %>% html_text() %>% trimws()
  
  body <- page %>% html_node("div.news-item.detail.content_text") %>% html_nodes("p") %>% html_text() %>% trimws()
  
  
  article <- tibble(url = art_url, title = title, date = date, lead = lead, body = body,)
  
  Sys.sleep(sample(seq(0.25, 1, 0.5), 1))
  
  return(article)
}

articles <- article_links %>%
  rowwise() %>%
  do(get_article(.$link)) %>%
  bind_rows() %>% 
  ungroup()

articles_grouped <- articles %>%
  group_by(url,title,date,lead )  %>%
  summarise(full_body = paste(body, collapse = " "))

#getwd()
#setwd('C:/Users/Dom/Desktop/R/Rcw/Text Analysis/TextAnalysis')
#saveRDS(articles_grouped, file = "articles_full.RDS")


#  anti_join(stop_words, by = 'word') %>%
#  filter(!str_detect(word, '[0-9]')) %>%
#  filter(str_detect(word, '[A-z]')) %>%
#  filter(str_detect(word, '\\w'))

#articles_long <- articles_long %>%
#  mutate(word2 = hunspell_stem(word))

#articles_long$word2 <- sapply(articles_long$word2, function(x) x[1])

#articles_long <- articles_long %>% 
#  mutate(word3 = ifelse(is.na(word2), word, word2))


clean.corpus <- function(corpus){
  corpus <- tm_map(corpus, tolower) 
  corpus <- tm_map(corpus, removeWords, stopwords('en')) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
  return(corpus)
}
articles_grouped$full_body_lemmatized <- NA
for(i in 1:nrow(articles_grouped)) {
  articles_long <- articles_grouped[i,"full_body"] %>% 
    unnest_tokens(word,full_body )
  articlesCorpus <- Corpus(VectorSource(articles_long))
  articlesCorpus <- clean.corpus(articlesCorpus)
  articles_grouped[i,"full_body_lemmatized"] <- data.frame(text = sapply(articlesCorpus, as.character), stringsAsFactors = FALSE)
}

word_tokens <- articles_grouped %>%
  unnest_tokens(word, full_body_lemmatized, drop = FALSE) %>%
  select(-full_body_lemmatized) %>% 
  filter(!str_detect(word, '\\d')) %>%
  filter(nchar(word) > 2)

#chmura słow
word_count <- word_tokens %>% 
  count(word)
par(mar = c(0, 0, 0, 0))
wordcloud(word_count$word, 
          word_count$n, max.words = 100, 
          rot.per = 0.35, colors = brewer.pal(12,"Paired"))

#top n slow

word_tokens$month <- month(ymd_hms(word_tokens$date))

word_top <- word_tokens[,-1:-3] %>% 
  count(month, word, sort = T) %>% 
  group_by(month) %>% 
  top_n(15, n) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n))

ggplot(word_top, aes(x = reorder_within(word, n, month), y = n, fill = month)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~month, scales ="free_y" ) +
  scale_x_reordered()


#top 3 tematy
word_tokens_long<-word_tokens[,-c(1,2,3,5)]
word_tokens_long_corpus <- Corpus(VectorSource(word_tokens_long$word))

dtm <- DocumentTermMatrix(word_tokens_long_corpus,
                          control = list(weighting = weightTf))

topic_minutes <- LDA(dtm, 
                     k = 4,
                     control = list(seed = 2137692115))

mi_topics <- tidy(topic_minutes, matrix = "beta") 

top_terms <- mi_topics %>%
  group_by(topic) %>% # grupujemy wg tematów
  top_n(10, beta) %>% # 10 największych prawdopodobieństw beta w każdym temacie
  ungroup() %>%
  arrange(topic, -beta) # ustalamy wg największych wartości beta

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>% # układa wyrażenia wg malejących wartości beta
  ggplot(aes(term, beta, fill = factor(topic))) + # tematy będą zaznaczone innymi kolorami
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()+
  scale_x_reordered()

#analiza emocji




