#####################################################################
# Predict Next Word vFinal using the backoff implementation 
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
create_filename <- function(x,training_set=80) {
  
  if (training_set < 0) {
    paste(x,"_all.RData",sep= "")
  }
  else {
    paste(x,"_",training_set,".RData",sep= "")
  }
}

####################################
load_DT_prob_final_table <- function (n,training_set=80) {
  print(paste("-----> INIT: load_DT_prob_final_table(n:=",n,
              " training_set:=",training_set,").......",sep=""))
  t1 <- proc.time()
  
  switch(n,
         "1"= {
           var.name <- "DT.uni.prob.final"
           file.name <- create_filename("DT_uni_prob_final",training_set)
         },
         "2"= {
           var.name <- "DT.bi.prob.final"
           file.name <- create_filename("DT_bi_prob_final",training_set)
         },
         "3"= {
           var.name <- "DT.tri.prob.final"
           file.name <- create_filename("DT_tri_prob_final",training_set)
         },
         "4"= {
           var.name <- "DT.quad.prob.final"
           file.name <- create_filename("DT_quad_prob_final",training_set)
         } 
  )
         
  #Validate if the DT exists in the enviroment
  if (!exists(var.name)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name)) {
     
      
      print(paste("Loading DT probability file: ",file.name, sep =""))
      load(file.name,.GlobalEnv) 
      
      print(paste("Initialization of DT Prob Table from:",var.name, sep =""))
      
      switch(n,
             "1" = DT.uni.prob.final <<- as.data.table(DT.uni.prob.final, key = "t1"),
             "2" = DT.bi.prob.final <<- as.data.table(DT.bi.prob.final, key = "t1,t2"),
             "3" = DT.tri.prob.final <<- as.data.table(DT.tri.prob.final, key = "t1,t2,t3"),
             "4" = DT.quad.prob.final <<- as.data.table(DT.quad.prob.final, key = "t1,t2,t3,t4")
      )
    }
    else {
      # Error file doesn't exists
      print(paste("Error file doesnt exist:",file.name, sep=""))
    }
  }
  t2 <- proc.time()
  print(paste("-----> FINISH: load_DT_prob_final_table(n:=",n,
              " training_set:=",training_set,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  
}

####################################
topn_predict <- function(x,p=0,n=5,f=1) {
  # f is the factor
  
  
  print(paste("-----> INIT: topn_predict(",
              " word:=(",paste(x, collapse=","),")",
              ", prob:=",p,
              ", nun_words:=",n,
              ", factor:=",f,
              ").......",sep=""))
  t1 <- proc.time()
  topn <- data.table(word=character(),prob=numeric())
  
  
  l <- length(x)
  
  if (l == 1) {
    #uni-gram level, let's check the bigram table and apply the factor for backoff

    topn <- DT.bi.prob.final[t1 == x[1] & ((prob*f) >= p),,]
    num_words <- nrow(topn)

    if (num_words > 0) {
      topn[,c("word","prob") := list(t2,prob*f),]
      topn <- topn[,list(word,prob),][head(order(-prob),n)]
    } else {
      topn <- data.table(word=character(),prob=numeric())
    }  
    
    print(paste("... Found:",num_words," words ..."))
    print(topn)
    
    if (num_words < n) {
      
      print("... Default Checking Probability in Unigram Data Table")
      
      a <- DT.uni.prob.final[prob >= p,,]
      
      num_words <- nrow(a)
      
      if (num_words > 0) {
        a <- a[head(order(-prob),n)] 
        a[,word:=t1,] 
        a <- a[,list(word,prob),]
      } else {
        a <- data.table(word=character(),prob=numeric())
      }
    
      print(paste("...Found:",num_words," words ..."))
      print(a)
      
      topn <- rbind(topn,a)
      
    }
  }
  else if (l == 2) {
    #bi-gram level, let's check the trigram table and apply the factor for backoff

    topn <- DT.tri.prob.final[t1 == x[1] & t2 == x[2] & ((prob*f) >= p),,]
    
    num_words <- nrow(topn)
    
    if (num_words > 0) {
      topn[,c("word","prob") := list(t3,prob*f),]
      topn <- topn[,list(word,prob),][head(order(-prob),n)]
    } else {
      topn <- data.table(word=character(),prob=numeric())
    }
    print(paste("...Found:",num_words," words ..."))
    print(topn)
    
    if (num_words < n) {
      print(paste("... Backoff to Unigram Level with factor:",0.4*f))
      topn <- rbind(topn,topn_predict(c(x[2]),p,n,0.4*f))
    } 
  }
  else if (l == 3) {
    #tri-gram level, let's check the quad.gram table
    topn <- DT.quad.prob.final[t1 == x[1] & t2 == x[2] & t3 ==x[3] & ((prob*f) >= p),,]
    num_words <- nrow(topn)
    if (num_words > 0) {
      topn[,c("word","prob") := list(t4,prob*f),]
      topn <- topn[,list(word,prob),][head(order(-prob),n)]
    } else {
      topn <- data.table(word=character(),prob=numeric())
    }
    
    print(paste("...Found:",num_words," words..."))
    print(topn)
    
    if (num_words < n) {
      print(paste("... Backoff to Bigram Level with factor:",0.4*f))
      #topn <- rbind(topn,topn_predict(c(x[2],x[3]),p,n-num_words,0.4*f))
      topn <- rbind(topn,topn_predict(c(x[2],x[3]),p,n,0.4*f))
    }
  }

  t2 <- proc.time()
  print(paste("-----> topn_predict: Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  
  topn
}

####################################
predict_nextword <- function(x,p=0,n=5,training_set=80) {
  print(paste("-----> INIT: predict_nextword(",
              " word:=(",paste(x, collapse=","),")",
              ", training_set:=",training_set,
              ", prob:=",p,
              ", n:=",n,
              ").......",sep=""))
  t1 <- proc.time()
  
  l <- length(x)
  load_DT_prob_final_table(1,training_set)
  load_DT_prob_final_table(2,training_set)
  load_DT_prob_final_table(3,training_set)
  load_DT_prob_final_table(4,training_set)

  result <- topn_predict(x,p,n,1)

  # Remove duplicated values, some words could appers duplicated as a part of
  # backoff strategy
  num_words <- nrow(result)
  
  if (num_words > 0) {
    setkey(result,"word")
    result <- unique(result)
    result <- result[head(order(-prob),n)] 
  }
  t2 <- proc.time()
  print(paste("-----> FINISH: predict_nextword: Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  result
}  








