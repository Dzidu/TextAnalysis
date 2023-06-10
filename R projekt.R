library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)
library(purrr)
library(dplyr, warn.conflicts = T)
library(RSelenium)

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
  
  author <- page %>% html_node('span.source-host') %>% html_text() %>% trimws()
  
  date <- page %>% html_node("span.datetime.flex.middle-xs") %>% html_text() %>% trimws() %>% dmy_hm()
  
  title <- page %>%html_node("h1") %>% html_text() %>% trimws()
  
  lead <- page %>% html_node("p") %>% html_text() %>% trimws()
  
  body <- page %>% html_node("div.news-item.detail.content_text") %>% html_nodes("p") %>% html_text() %>% trimws()
  
  
  article <- tibble(url = art_url, title = title, author = author, date = date, lead = lead, body = body,)
  
  Sys.sleep(sample(seq(0.25, 1, 0.5), 1))
  
  return(article)
}

articles <- article_links %>%
  rowwise() %>%
  do(get_article(.$link)) %>%
  bind_rows() %>% 
  ungroup()

articles_grouped <- articles %>%
  group_by(url,title,author,date,lead )  %>%
  summarise(full_body = paste(body, collapse = " "))

