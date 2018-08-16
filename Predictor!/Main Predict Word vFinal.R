
library(stringi)
library(data.table)


####################################
last_n_words <- function(s, num_words = 3) {
  
  # Lower, trim and remove additional blank spaces
  
  s <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", tolower(s))
  s <- gsub("[[:space:]]+", " ", s)
  
  words_list <- unlist(stri_split_fixed(s," "))
  
  l <- length(words_list)
  
  if (l < num_words) {
    # Not enough words in the string, return all the words
    words_list
  } else {
    # Return only the last n words
    words_list[(l-num_words+1):l]
  }
}

####################################
format_number <- function(x) {
  format(round(x, 5), trim=T, nsmall=5)
}

####################################
DT_prob_singletons <- function (training_set=80) { 
  
  print(paste("-----> INIT: DT_prob_singletons(training_set:=",training_set,").......",sep=""))
  t1 <- proc.time()
  
  DT.result <- data.table("Ngram"=character(),
                          "Object Size All Freq (Mbytes)"=double(),
                          "Object Size Freq > 1 (Mbytes)"=double(),"Percent  Freq > 1"=double(),
                          "Object Size Freq >= 5 (Mbytes)"=double(),"Percent Freq >= 5"=double())
  
  for (i in 1:4)
  {
    load_DT_prob_final_table(i,training_set)
    
    switch(i,
           "1" = {
             
             s0 <- object.size(DT.uni.prob.final)
             DT.uni.prob.final <- DT.uni.prob.final[freq1 > 1,,]
             s1 <- object.size(DT.uni.prob.final)
             file.name <- create_filename("DT_uni_prob_final",paste0(training_set,".2"))
             save(DT.uni.prob.final, file = file.name)
             
             DT.uni.prob.final <- DT.uni.prob.final[freq1 >= 5,,]
             s2 <- object.size(DT.uni.prob.final)
             file.name <- create_filename("DT_uni_prob_final",paste0(training_set,".5"))
             save(DT.uni.prob.final, file = file.name)
             rm(DT.uni.prob.final,envir = .GlobalEnv)
             gc()
           },
           "2" = {
             s0 <- object.size(DT.bi.prob.final)
             DT.bi.prob.final <- DT.bi.prob.final[freq2 > 1,,]
             s1 <- object.size(DT.bi.prob.final)
             file.name <- create_filename("DT_bi_prob_final",paste0(training_set,".2"))
             save(DT.bi.prob.final, file = file.name)
             
             DT.bi.prob.final <- DT.bi.prob.final[freq2 >= 5,,]
             s2 <- object.size(DT.bi.prob.final)
             file.name <- create_filename("DT_bi_prob_final",paste0(training_set,".5"))
             save(DT.bi.prob.final, file = file.name)
             
             rm(DT.bi.prob.final,envir = .GlobalEnv)
             gc()
             
           },
           "3" = {
             s0 <- object.size(DT.tri.prob.final)
             DT.tri.prob.final <- DT.tri.prob.final[freq3 > 1,,]
             s1 <- object.size(DT.tri.prob.final)
             file.name <- create_filename("DT_tri_prob_final",paste0(training_set,".2"))
             save(DT.tri.prob.final, file = file.name)
             
             DT.tri.prob.final <- DT.tri.prob.final[freq3 >= 5,,]
             s2 <- object.size(DT.tri.prob.final)
             file.name <- create_filename("DT_tri_prob_final",paste0(training_set,".5"))
             save(DT.tri.prob.final, file = file.name)
             
             rm(DT.tri.prob.final,envir = .GlobalEnv)
             gc()
             
           },
           "4" = {
             
             s0 <- object.size(DT.quad.prob.final)
             DT.quad.prob.final <- DT.quad.prob.final[freq4 > 1,,]
             s1 <- object.size(DT.quad.prob.final)
             file.name <- create_filename("DT_quad_prob_final",paste0(training_set,".2"))
             save(DT.quad.prob.final, file = file.name)
             
             DT.quad.prob.final <- DT.quad.prob.final[freq4 >= 5,,]
             s2 <- object.size(DT.quad.prob.final)
             file.name <- create_filename("DT_quad_prob_final",paste0(training_set,".5"))
             save(DT.quad.prob.final, file = file.name)
             
             rm(DT.quad.prob.final,envir = .GlobalEnv)
             gc()
           }
    )
    mbytes <- 1024 * 1024
    DT.result <- rbind(DT.result,as.list(c(i,format_number(s0[1]/mbytes),
                                           format_number(s1[1]/mbytes),
                                           format_number(s1[1]/s0[1]*100),
                                           format_number(s2[1]/mbytes),
                                           format_number(s2[1]/s0[1]*100))))
  
  }
  
  t2 <- proc.time()
  print(paste("-----> FINISH: DT_prob_singletons(training_set:=",training_set,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  DT.result
}


####################################
DT_prob_freq <- function (training_set=80) { 
  
  print(paste("-----> INIT: DT_prob_freq(training_set:=",training_set,").......",sep=""))
  t1 <- proc.time()
  
  DT.result <- data.table(Ngram=character(),Freq=character(),Amount=double(),Percent=double())
  
  for (i in 1:4)
  {
    load_DT_prob_final_table(i,training_set)
    
    switch(i,
           "1" = {
             l <- nrow(DT.uni.prob.final)
             min_freq <- min(DT.uni.prob.final$freq1)
             max_freq <- max(DT.uni.prob.final$freq1)
             s0 <- nrow(DT.uni.prob.final[freq1 == min_freq,,])
             s1 <- nrow(DT.uni.prob.final[freq1 == (min_freq + 1),,])
             s2 <- nrow(DT.uni.prob.final[freq1 == (min_freq + 2),,])
             s3 <- nrow(DT.uni.prob.final[freq1 == (min_freq + 3),,])
             s4 <- nrow(DT.uni.prob.final[freq1 == (min_freq + 4),,])
             s5 <- nrow(DT.uni.prob.final[freq1 >= (min_freq + 5),,])
             sn <- nrow(DT.uni.prob.final[freq1 >= max_freq,,])
             #rm(DT.uni.prob.final,envir = .GlobalEnv)
             #gc()
           },
           "2" = {
             l <- nrow(DT.bi.prob.final)
             min_freq <- min(DT.bi.prob.final$freq2)
             max_freq <- max(DT.bi.prob.final$freq2)
             s0 <- nrow(DT.bi.prob.final[freq2 == min_freq,,])
             s1 <- nrow(DT.bi.prob.final[freq2 == (min_freq + 1),,])
             s2 <- nrow(DT.bi.prob.final[freq2 == (min_freq + 2),,])
             s3 <- nrow(DT.bi.prob.final[freq2 == (min_freq + 3),,])
             s4 <- nrow(DT.bi.prob.final[freq2 == (min_freq + 4),,])
             s5 <- nrow(DT.bi.prob.final[freq2 >= (min_freq + 5),,])
             sn <- nrow(DT.bi.prob.final[freq2 >= max_freq,,])
             
             #rm(DT.bi.prob.final,envir = .GlobalEnv)
             #gc()
             
           },
           "3" = {
             l <- nrow(DT.tri.prob.final)
             min_freq <- min(DT.tri.prob.final$freq3)
             max_freq <- max(DT.tri.prob.final$freq3)
             s0 <- nrow(DT.tri.prob.final[freq3 == min_freq,,])
             s1 <- nrow(DT.tri.prob.final[freq3 == (min_freq + 1),,])
             s2 <- nrow(DT.tri.prob.final[freq3 == (min_freq + 2),,])
             s3 <- nrow(DT.tri.prob.final[freq3 == (min_freq + 3),,])
             s4 <- nrow(DT.tri.prob.final[freq3 == (min_freq + 4),,])
             s5 <- nrow(DT.tri.prob.final[freq3 >= (min_freq + 5),,])
             sn <- nrow(DT.tri.prob.final[freq3 >= max_freq,,])
             
             #rm(DT.tri.prob.final,envir = .GlobalEnv)
             #gc()
             
           },
           "4" = {
             l <- nrow(DT.quad.prob.final)
             min_freq <- min(DT.quad.prob.final$freq4)
             max_freq <- max(DT.quad.prob.final$freq4)
             s0 <- nrow(DT.quad.prob.final[freq4 == min_freq,,])
             s1 <- nrow(DT.quad.prob.final[freq4 == (min_freq + 1),,])
             s2 <- nrow(DT.quad.prob.final[freq4 == (min_freq + 2),,])
             s3 <- nrow(DT.quad.prob.final[freq4 == (min_freq + 3),,])
             s4 <- nrow(DT.quad.prob.final[freq4 == (min_freq + 4),,])
             s5 <- nrow(DT.quad.prob.final[freq4 >= (min_freq + 5),,])
             sn <- nrow(DT.quad.prob.final[freq4 >= max_freq,,])
             
             #rm(DT.quad.prob.final,envir = .GlobalEnv)
             #gc()
           }
    )
    
    DT.result <- rbind(DT.result,as.list(c(i,"Total Ngrams",l,
                                           format_number(100))))
    DT.result <- rbind(DT.result,as.list(c(i,paste0("Min Freq == ",min_freq),s0,
                                           format_number(s0/l*100))))
    DT.result <- rbind(DT.result,as.list(c(i,paste0("Freq == ",(min_freq+1)),s1,
                                           format_number(s1/l*100))))
    DT.result <- rbind(DT.result,as.list(c(i,paste0("Freq == ",(min_freq+2)),s2,
                                           format_number(s2/l*100))))
    DT.result <- rbind(DT.result,as.list(c(i,paste0("Freq == ",(min_freq+3)),s3,
                                           format_number(s3/l*100))))
    DT.result <- rbind(DT.result,as.list(c(i,paste0("Freq == ",(min_freq+4)),s4,
                                           format_number(s4/l*100))))
    DT.result <- rbind(DT.result,as.list(c(i,paste0("Max Freq == ",max_freq),sn,
                                           format_number(sn/l*100))))
    
    
    DT.result <- rbind(DT.result,as.list(c(i,paste0("Freq <= ",(min_freq+1)),s0+s1,
                                           format_number((s0+s1)/l*100))))
    DT.result <- rbind(DT.result,as.list(c(i,paste0("Freq <= ",(min_freq+2)),s0+s1+s2,
                                           format_number((s0+s1+s2)/l*100))))
    DT.result <- rbind(DT.result,as.list(c(i,paste0("Freq <= ",(min_freq+3)),s0+s1+s2+s3,
                                           format_number((s0+s1+s2+s3)/l*100))))
    DT.result <- rbind(DT.result,as.list(c(i,paste0("Freq <= ",(min_freq+4)),s0+s1+s2+s3+s4,
                                           format_number((s0+s1+s2+s3+s4)/l*100))))
    DT.result <- rbind(DT.result,as.list(c(i,paste0("Freq >= ",(min_freq+5)),s5,
                                           format_number(s5/l*100))))
    
  }
  
  t2 <- proc.time()
  print(paste("-----> FINISH: DT_prob_freq(training_set:=",training_set,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  DT.result
}

####################################
main_predict_word <- function(s,p=0,n=5,consider_regex_words = TRUE, training_set = 80) {
  
  print(paste("-----> INIT: main_predict_word(s:=",s," p:=",p," n:=",n,
              " consider_regex_words:=",consider_regex_words," training_set:=",training_set,
              ").......",sep=""))
  t1 <- proc.time()
  
  if (s != "") {
    s <- tolower(s)
    print(paste("String:",s,"...",sep=""))
    
    is_final_word <- stri_endswith_fixed(s," ")
    
    list_words <- last_n_words(s)
    
    if (consider_regex_words) {
      # Consider Regex Words in the Prediction
     
      if (is_final_word) {
        print("---> Considering Regex: YES, Predicting Next Word with Complete Words ...")
        result <- predict_nextword(list_words,p,n,training_set)
      } else {
        print("---> Considering Regex: YES, Predicting Next Word with Regex Words...")
        result <- predict_nextword_regex(list_words,p,n,training_set)
        
      }
    }
    else {
      # Consider Only Complete Words in the Prediction 
      
      if (is_final_word) {
        print("---> Considering Regex: NO, Predicting Next Word with Complete Words ...")
        result <- predict_nextword(list_words,p,n,training_set)
      } else {
        print("---> Considering Regex: NO, Is a Regex Words ... Wait until a Complete Word Appear ...")
        result <- data.table(character(),numeric())
      }
    }
  } else {
    result <- data.table(character(),numeric())
  } 
  
  t2 <- proc.time()
  print(paste("-----> FINISH: main_predict_word(s:=",s," p:=",p," n:=",n,
              " consider_regex_words:=",consider_regex_words," training_set:=",training_set,
              "): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  
  result
}

