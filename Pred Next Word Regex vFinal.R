#####################################################################
# Predict next word regex vFinal using the backoff implementation with incomplete
#   and the knersey-ney probabilities already calculated previously:
#
#             DT_uni_prob.Rdata: data table that include all the unigrams 
#                       frequency and the knersey-ney prob
#             DT_bi_prob.Rdata: data table that include all the bigrams 
#                       frequency and the knersey-ney prob
#             DT_tri_prob.Rdata: data table that include all the trigrams 
#                       frequency and the knersey-ney prob
#             DT_quad_prob.Rdata: data table that include all the quadgrams 
#                       frequency and the knersey-ney prob

library(data.table)

####################################
elapsed_time <- function(tic1,tic2) {
  format((tic2-tic1)[3][[1]], digits = 2)
}

####################################
topn_predict_regex <- function(x,p=0,n=5,f=1) {

  
  print(paste("-----> INIT: topn_predict_regex(",
              " word:=(",paste(x, collapse=","),")",
              ", prob:=",p,
              ", nun_words:=",n,
              ", factor:=",f,
              ").......",sep=""))
  
  
  t1 <- proc.time()
  topn <- data.table(word=character(),prob=numeric())
  
  l <- length(x)
  
  if (l == 1) {
    #uni-gram level, let's check the unigram table for the regex and apply the factor for backoff

    regex_word <- paste("^",x[1],".*",sep="")

    topn_temp <- DT.uni.prob.final[grepl(regex_word,t1),,]
    num_words <- nrow(topn_temp)
    
    if (num_words > 0) {
      topn <- topn_temp[((prob*f) >= p),,]
      topn <- topn[head(order(-prob),n)] 
      topn[,word:=t1,] 
      topn <- topn[,list(word,prob),]
    } 
    
    print(paste("...Found:",num_words," words ..."))
    print(topn)
    
  } else if (l == 2) {
    #bi-gram level, let's check the unigram table for the regex and apply the factor for backoff
    
    regex_word <- paste("^",x[2],".",sep="")
    topn_temp <- DT.bi.prob.final[t1 == x[1] & ((prob*f) >= p),,]
    
    num_words <- nrow(topn_temp)
    
    if (num_words > 0) {
      topn_temp <- topn_temp[grepl(regex_word,t2),,]
      num_words <- nrow(topn_temp)
      
      if (num_words > 0) {
        topn <- topn_temp[head(order(-prob),n)] 
        topn[,word:=t2,] 
        topn <- topn[,list(word,prob),]
      } 
    }
    
    print(paste("...Found:",num_words," words ..."))
    print(topn)  
  
    if (num_words < n) {
      print(paste("... Backoff to Unigram Level with factor:",0.4*f))
      topn <- rbind(topn,topn_predict_regex(c(x[2]),p,n,0.4*f))
    }
  }  else if (l == 3) {
    #tri-gram level, let's check the trigram table for the regex and apply the factor for backoff
    
    regex_word <- paste("^",x[3],".",sep="")
    
    topn_temp <- DT.tri.prob.final[t1 == x[1] & t2 == x[2] & ((prob*f) >= p),,]
    
    num_words <- nrow(topn_temp)
    
    if (num_words > 0) {
      topn_temp <- topn_temp[grepl(regex_word,t3),,]
      num_words <- nrow(topn_temp)
      
      if (num_words > 0) {
        topn <- topn_temp[head(order(-prob),n)] 
        topn[,word:=t3,] 
        topn <- topn[,list(word,prob),]
      }
    }
      
    print(paste("...Found:",num_words," words ..."))
    print(topn)
    
    
    if (num_words < n) {
      print(paste("... Backoff to Bigram Level with factor:",0.4*f))
      topn <- rbind(topn,topn_predict_regex(c(x[2],x[3]),p,n,0.4*f))
    }
    
  } else if (l == 4) {
    #quad-gram level, let's check the quadgram table for the regex and apply the factor for backoff
    
    regex_word <- paste("^",x[4],sep="")
    
    topn_temp <- DT.quad.prob.final[t1 == x[1] & t2 == x[2] & t3 == x[3] & ((prob*f) >= p),,]
    
    num_words <- nrow(topn_temp)
    
    if (num_words > 0) {
      topn_temp <- topn_temp[grepl(regex_word,t4),,]
      num_words <- nrow(topn_temp)
    
      if (num_words > 0) {
        topn <- topn_temp[head(order(-prob),n)] 
        topn[,word:=t4,] 
        topn <- topn[,list(word,prob),]
      } 
    }
      
    print(paste("...Found:",num_words," words ..."))
    print(topn)
    
    
    if (num_words < n) {
      print(paste("... Backoff to Trigram Level with factor:",0.4*f))
      topn <- rbind(topn,topn_predict_regex(c(x[2],x[3],x[4]),p,n,0.4*f))
    }
  }
  t2 <- proc.time()
  
  print(paste("-----> FINISH: topn_predict_regex: Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  
  topn
}

####################################
predict_nextword_regex <- function(x,p=0,n=5,training_set=80) {
  print(paste("-----> INIT: predict_nextword_regex(",
              " word:=(",paste(x, collapse=","),")",
              ", training_set:=",training_set,
              ", prob:=",p,
              ", n:=",n,
              ").......",sep=""))
  
  l <- length(x)
  load_DT_prob_final_table(1,training_set)
  load_DT_prob_final_table(2,training_set)
  load_DT_prob_final_table(3,training_set)
  load_DT_prob_final_table(4,training_set)
  
  
  t1 <- proc.time()
  
  result <- topn_predict_regex(x,p,n,1)

  
  # Remove duplicated values, some words could appers duplicated as a part of
  # backoff strategy
  num_words <- nrow(result)

  if (num_words > 0) {
    setkey(result,"word")
    result <- unique(result)
    result <- result[head(order(-prob),n)] 
  }
  
  t2 <- proc.time()
  print(paste("-----> FINISH: predict_nextword_regex: Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  result
  
}  


