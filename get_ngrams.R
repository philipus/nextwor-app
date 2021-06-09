library(tm)
library(dplyr)
library(R.utils)
library(ggplot2)
library(tidytext)
library(tidyr)
library(rlist)
library(stringr)
library(stringi)
library(udpipe)
library(chunked)
lapply(list('igraph','SnowballC','stringr','openNLP','hash','tm'), library, character.only=TRUE)

annotation_en <- readRDS("annotation_en.rds")

annotation_en_sum <-
  annotation_en %>% 
  filter(! (upos %in%  c("PUNCT","SYM", "PART")) ) %>%
  group_by(chunk_id, doc_id, paragraph_id, sentence_id) %>% 
  summarize(n = n())

annotation_en_id <- sample(nrow(annotation_en_sum),100000)

trigram_df <-
  annotation_en %>% inner_join(
    annotation_en_sum[annotation_en_id,] %>% 
      dplyr::filter(n >= 1) %>% 
      dplyr::select(-n) %>% 
      ungroup()
  ) %>%
  filter(! (upos %in%  c("PUNCT","SYM", "PART")) ) %>%
  group_by(chunk_id, doc_id, paragraph_id, sentence_id) %>%
  summarise( ngram = txt_nextgram(lemma, n = 3, sep = ";" )) %>%
  separate(ngram, c("word1", "word2", "word3"), sep = ";") %>%
  ungroup() %>%
  filter(!is.na(word1) & !is.na(word2) & !is.na(word3) ) %>%
  group_by(word1, word2, word3) %>%
  summarise(freq=n())

saveRDS(trigram_df,"data/trigram_df.rds")

bigram_df <-
  annotation_en %>% inner_join(
    annotation_en_sum[annotation_en_id,] %>% 
      dplyr::filter(n >= 1) %>% 
      dplyr::select(-n) %>% 
      ungroup()
  ) %>%
  filter(! (upos %in%  c("PUNCT","SYM", "PART")) ) %>%
  group_by(chunk_id, doc_id, paragraph_id, sentence_id) %>%
  summarise( ngram = txt_nextgram(lemma, n = 2, sep = ";" )) %>%
  separate(ngram, c("word1", "word2"), sep = ";") %>%
  ungroup() %>%
  filter(!is.na(word1) & !is.na(word2) ) %>%
  group_by(word1, word2) %>%
  summarise(freq=n())

saveRDS(bigram_df,"data/bigram_df.rds")

unigram_df <-
  annotation_en %>% inner_join(
    annotation_en_sum[annotation_en_id,] %>% 
      dplyr::filter(n >= 1) %>% 
      dplyr::select(-n) %>% 
      ungroup()
  ) %>%
  filter(! (upos %in%  c("PUNCT","SYM", "PART")) ) %>%
  group_by(chunk_id, doc_id, paragraph_id, sentence_id) %>%
  summarise( word1 = txt_nextgram(lemma, n = 1, sep = ";" )) %>%
  ungroup() %>%
  filter(!is.na(word1) ) %>%
  group_by(word1) %>%
  summarise(freq=n())

saveRDS(unigram_df,"data/unigram_df.rds")

