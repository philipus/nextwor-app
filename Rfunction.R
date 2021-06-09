
######

getObsTrigs_df <- function(aWord1, aWord2, aTrigrams_df) {
  if(nrow(aTrigrams_df) < 1) return(NULL)
  return(aTrigrams_df %>% filter(word1==aWord1 & word2==aWord2))
}

getObsTriProbs_df <- function(obsTrigs_df, bigram_df, aWord1, aWord2, triDisc=0.5) {
  if(nrow(obsTrigs_df) < 1) return(NULL)
  obsCount <- (bigram_df %>% filter(word1==aWord1 & word2==aWord2))$freq[1]
  obsTrigProbs_df <- obsTrigs_df %>% mutate(freq=((freq - triDisc) / obsCount))
  colnames(obsTrigProbs_df) <- c("word1","word2","word3", "prob")
  
  return(obsTrigProbs_df)
}

getUnobsTrigTails_df <- function(obsTrigs_df, unigram_df) {
  unobs_trig_tails_df <- unigram_df %>% 
    anti_join(obsTrigs_df %>% select(word3),
              on=c("word1"="word3")
    ) %>% select(word1)
  return(unobs_trig_tails_df)
}

getAlphaBigram_df <- function(unigram, bigrams_df, bigDisc=0.5) {
  bigsThatStartWithUnig_df <- bigrams_df %>% filter( word1 == unigram$word1 )
  if(nrow(bigsThatStartWithUnig_df) < 1) return(0)
  alphaBi <- 1 - (sum(bigsThatStartWithUnig_df$freq - bigDisc) / unigram$freq)
  return(alphaBi)
}
#########################

getBoBigrams_df <- function(aWord2, unobsTrigTails_df) {
  boBigrams_df <- unobsTrigTails_df %>% rename(word2=word1) %>%
    mutate(word1=aWord2) %>% select(word1, word2)
  return(boBigrams_df)
}

getObsBoBigrams_df <- function(aWord2, unobsTrigTails_df, bigram_df) {
  boBigrams_df <- getBoBigrams_df(aWord2, unobsTrigTails_df)
  obs_bo_bigrams_df <- bigram_df %>% inner_join(boBigrams_df)
  return(obs_bo_bigrams_df)
}

getUnobsBoBigrams_df <- function(aWord2, unobsTrigTails_df, obsBoBigram_df) {
  boBigrams_df <- getBoBigrams_df(aWord2, unobsTrigTails_df)
  unobs_bigs_df <- boBigrams_df %>% anti_join(obsBoBigram_df %>% select(-freq))
  return(unobs_bigs_df)
}

getObsBigProbs_df <- function(obsBoBigrams_df, unigram_df, bigDisc=0.5) {
  first_word_freqs_df <- unigram_df %>% 
    inner_join(obsBoBigrams_df %>% select(word1))
  obsBigProbs <- (obsBoBigrams_df$freq - bigDisc) / first_word_freqs_df$freq
  obsBigProbs_df <- obsBoBigrams_df %>% 
    inner_join(unigram_df %>% rename(freq_uni=freq)) %>%
    mutate(prob = (freq - bigDisc) / freq_uni) %>%
    select(word1, word2, prob)
  
  return(obsBigProbs_df)
}

getQboUnobsBigrams_df <- function(unobsBoBigrams_df, unigram_df, alphaBig) {
  qboUnobsBigs_df <- unigram_df %>% rename(word2=word1) %>%
    inner_join(unobsBoBigrams_df )
  denom <- sum(qboUnobsBigs_df$freq)
  qboUnobsBigs_df <- unobsBoBigrams_df %>% inner_join(
    qboUnobsBigs_df %>% rename(freq_uni=freq)
  ) %>% mutate(
    prob=(alphaBig * freq_uni / sum(freq_uni))
  ) %>%
    select(word1, word2, prob)
  
  return(qboUnobsBigs_df)
}

#############
getAlphaTrigram_df <- function(obsTrigs_df, bigram_filter, triDisc=0.5) {
  if(nrow(obsTrigs_df) < 1) return(1)
  alphaTri <- 1 - sum((obsTrigs_df$freq - triDisc) / bigram_filter$freq[1])
  
  return(alphaTri)
}

#############
getUnobsTriProbs_df <- function(aWord1, qboObsBigrams_df,
                                qboUnobsBigrams_df, alphaTrig) {
  qboBigrams_df <- rbind(qboObsBigrams_df, qboUnobsBigrams_df)
  qboBigrams_df <- qboBigrams_df[order(-qboBigrams_df$prob), ]
  sumQboBigs <- sum(qboBigrams_df$prob)
  unobsTrigNgrams_df <- qboBigrams_df %>% 
    rename(word3=word2) %>% 
    rename(word2=word1) %>%
    mutate(word1=aWord1)
  unobsTrigDf <- unobsTrigNgrams_df %>% 
    mutate(prob=alphaTrig * prob / sumQboBigs) %>%
    select(word1, word2, word3, prob)
  
  return(unobsTrigDf)
}
#############
getPredictionMsg_df <- function(qbo_trigs_df) {
  # pull off tail word of highest prob trigram
  prediction <- qbo_trigs_df$word3[1]
  result <- sprintf("%s%s%s%.4f", "highest prob prediction is >>> ", prediction,
                    " <<< which has probability = ", qbo_trigs_df$prob[1])
  return(result)
}

getPrediction_df <- function(qbo_trigs_df) {
  # pull off tail word of highest prob trigram
  prediction <- qbo_trigs_df$word3[1]
  return(list(pred=prediction,prob=qbo_trigs_df$prob[1]))
}

get_out_msg <- function(aSentence) {
  gamma2 <- 0.5  # bigram discount
  gamma3 <- 0.5  # trigram discount
  
  annotation_s <- (as.data.frame(udpipe(aSentence, dl.model.file)) %>% 
                     filter(! (upos %in%  c("PUNCT","SYM", "PART")) ))$lemma
  
  mWord1 = annotation_s[length(annotation_s)-1]
  mWord2 = annotation_s[length(annotation_s)]
  
  obs_trigs_df <- getObsTrigs_df(mWord1, mWord2, trigram_df)  # get trigrams and counts
  # convert counts to probabilities
  qbo_obs_trigrams_df <- getObsTriProbs_df(obs_trigs_df, bigram_df, mWord1, mWord2, gamma3)
  
  unobs_trig_tails_df <- getUnobsTrigTails_df(obs_trigs_df, unigram_df)
  
  unig <- unigram_df %>% filter(word1 == mWord2)
  alpha_big <- getAlphaBigram_df(unig, bigram_df, gamma2)
  
  #########################
  
  
  bo_bigrams_df <- getBoBigrams_df(mWord2, unobs_trig_tails_df)  # get backed off bigrams
  
  
  # separate bigrams which use eqn 10 and those that use 16
  obs_bo_bigrams_df <- getObsBoBigrams_df(mWord2, unobs_trig_tails_df, bigram_df)
  
  
  
  unobs_bo_bigrams_df <- getUnobsBoBigrams_df(mWord2, 
                                              unobs_trig_tails_df, 
                                              obs_bo_bigrams_df)
  
  
  
  qbo_obs_bigrams_df <- getObsBigProbs_df(obs_bo_bigrams_df, unigram_df, gamma2) #ngram     probs
  
  
  
  # distrib discounted bigram prob mass to unobs bigrams in prop to unigram ML
  qbo_unobs_bigrams_df <- getQboUnobsBigrams_df(unobs_bo_bigrams_df, 
                                                unigram_df, 
                                                alpha_big)
  qbo_bigrams_df <- rbind(qbo_obs_bigrams_df, qbo_unobs_bigrams_df)
  
  #############
  bigram_filter <- bigram_df %>% filter(word1==mWord1 & word2==mWord2)
  alpha_trig <- getAlphaTrigram_df(obs_trigs_df, bigram_filter, gamma3)
  
  
  
  qbo_unobs_trigrams_df <- getUnobsTriProbs_df(mWord1, qbo_obs_bigrams_df,
                                               qbo_unobs_bigrams_df, alpha_trig)
  #############
  qbo_trigrams_df <- rbind(qbo_obs_trigrams_df, qbo_unobs_trigrams_df)
  qbo_trigrams_df <- qbo_trigrams_df[order(-qbo_trigrams_df$prob), ]  # sort by desc prob
  out_msg <- getPredictionMsg_df(qbo_trigrams_df)
  return(out_msg)
  ##
}

get_out_pred <- function(aSentence) {
  gamma2 <- 0.5  # bigram discount
  gamma3 <- 0.5  # trigram discount
  
  annotation_s <- (as.data.frame(udpipe(aSentence, dl.model.file)) %>% 
                     filter(! (upos %in%  c("PUNCT","SYM", "PART")) ))$lemma
  
  mWord1 = annotation_s[length(annotation_s)-1]
  mWord2 = annotation_s[length(annotation_s)]
  
  obs_trigs_df <- getObsTrigs_df(mWord1, mWord2, trigram_df)  # get trigrams and counts
  # convert counts to probabilities
  qbo_obs_trigrams_df <- getObsTriProbs_df(obs_trigs_df, bigram_df, mWord1, mWord2, gamma3)
  
  unobs_trig_tails_df <- getUnobsTrigTails_df(obs_trigs_df, unigram_df)
  
  unig <- unigram_df %>% filter(word1 == mWord2)
  alpha_big <- getAlphaBigram_df(unig, bigram_df, gamma2)
  
  #########################
  
  
  bo_bigrams_df <- getBoBigrams_df(mWord2, unobs_trig_tails_df)  # get backed off bigrams
  
  
  # separate bigrams which use eqn 10 and those that use 16
  obs_bo_bigrams_df <- getObsBoBigrams_df(mWord2, unobs_trig_tails_df, bigram_df)
  
  
  
  unobs_bo_bigrams_df <- getUnobsBoBigrams_df(mWord2, 
                                              unobs_trig_tails_df, 
                                              obs_bo_bigrams_df)
  
  
  
  qbo_obs_bigrams_df <- getObsBigProbs_df(obs_bo_bigrams_df, unigram_df, gamma2) #ngram     probs
  
  
  
  # distrib discounted bigram prob mass to unobs bigrams in prop to unigram ML
  qbo_unobs_bigrams_df <- getQboUnobsBigrams_df(unobs_bo_bigrams_df, 
                                                unigram_df, 
                                                alpha_big)
  qbo_bigrams_df <- rbind(qbo_obs_bigrams_df, qbo_unobs_bigrams_df)
  
  #############
  bigram_filter <- bigram_df %>% filter(word1==mWord1 & word2==mWord2)
  alpha_trig <- getAlphaTrigram_df(obs_trigs_df, bigram_filter, gamma3)
  
  
  
  qbo_unobs_trigrams_df <- getUnobsTriProbs_df(mWord1, qbo_obs_bigrams_df,
                                               qbo_unobs_bigrams_df, alpha_trig)
  #############
  qbo_trigrams_df <- rbind(qbo_obs_trigrams_df, qbo_unobs_trigrams_df)
  qbo_trigrams_df <- qbo_trigrams_df[order(-qbo_trigrams_df$prob), ]  # sort by desc prob
  out_pred <- getPrediction_df(qbo_trigrams_df)
  return(out_pred)
  ##
}

#get_out_pred("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each")

#get_out_msg("I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each")
