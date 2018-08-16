#####################################################################
# Knersey-ney Optimazed VFinal
# Calculate Knersey-ney of the differents ngrams (unigram, bigram, trigram and quadgrams)
#          to be used later with Kneser-ney algortihm to calculate the
#          probability of each ngram for predict the next word.
#          This script was run on my PC and takes long time to finish,
#          at the end this script saves the data into four files:
#
#             uni_dt.Rdata: data frame that include all the unigrams and the
#                          frequency for each one.
#             bi_dt.Rdata: data frame that include all the bigrams and the
#                          frequency for each one.
#             tri_dt.Rdata: data frame that include all the trigrams and the
#                          frequency for each one.
#             quad_dt.Rdata: data frame that include all the quadgrams and the
#                          frequency for each one.
# In this version we did some optimizations related with the recursion.
# We start calculating the unigrams probability. This calculation could be used
# on bigrams, trigrams and quadgrams calculations. To do that we also store some 
# values as N1plus_pre(x), N1plus_suc(x), alpha(x), lambda(x). 

library(data.table)



####################################
load_DT_table <- function (n,training_set=80) {
  print(paste("-----> INIT: load_DT_table(n:=",n," training_set:=",training_set,").......",sep=""))
  t1 <- proc.time()
  
  switch(n,
         "1" = {
           var.name <- "DT.uni"
           file.name <- create_filename("DT_uni",training_set)
         },
         "2" = {
           var.name <- "DT.bi"
           file.name <- create_filename("DT_bi",training_set)
         },
         "3" = {
           var.name <- "DT.tri"
           file.name <- create_filename("DT_tri",training_set)
         },
         "4" = {
           var.name <- "DT.quad"
           file.name <- create_filename("DT_quad",training_set)
         }
  )
  
  #Validate if the DT exists in the enviroment
  if (!exists(var.name)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name)) {
      print(paste("... Loading DT file: ",file.name, sep =""))
      load(file.name,.GlobalEnv) 
      
      print(paste("... Creating DT Table from:",var.name, sep =""))
      
      switch(n,
             "1" = DT.uni <<- as.data.table(DT.uni, key = "t1"),
             "2" = DT.bi <<- as.data.table(DT.bi, key = "t1,t2"),
             "3" = DT.tri <<- as.data.table(DT.tri, key = "t1,t2,t3"),
             "4" = DT.quad <<- as.data.table(DT.quad, key = "t1,t2,t3,t4")
      )
    }
    else {
      # Error file doesn't exists
      print(paste("-----> ERROR: load_DT_table(n:=",n,",training_set:=",training_set,"): Error file doesnt exist:",file.name, sep=""))
    }
  }
  t2 <- proc.time()
  print(paste("-----> FINISH: load_DT_table(n:=",n," training_set:=",training_set,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
}


####################################
load_DT_prob_table <- function (n,training_set=80,p1=5) {
  print(paste("-----> INIT: load_DT_prob_table(","n:=",n," p1:=",p1," training_set:=",training_set,").......",sep=""))
  t1 <- proc.time()
  
  # Values needed for Knersey-ney prob calculations
  p1 <<- p1
  p2 <<- p1 + 1
  
  switch(n,
         "1" = {
           var.name <- "DT.uni.prob"
           file.name <- create_filename("DT_uni_prob",training_set)
           file.name.temp <- create_filename("DT_uni_prob_temp", training_set)
           
         },
         "2" = {
           var.name <- "DT.bi.prob"
           file.name <- create_filename("DT_bi_prob",training_set)
           file.name.temp <- create_filename("DT_bi_prob_temp", training_set)
         },
         "3" = {
           var.name <- "DT.tri.prob"
           file.name <- create_filename("DT_tri_prob",training_set)
           file.name.temp <- create_filename("DT_tri_prob_temp", training_set)
         },
         "4" = {
           var.name <- "DT.quad.prob"
           file.name <- create_filename("DT_quad_prob",training_set)
           file.name.temp <- create_filename("DT_quad_prob_temp", training_set)
         }
  )
  
  #Validate if the DT prob temp exists in the enviroment
  if (!exists(var.name)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name.temp)) {
      print(paste("... Loading DT Prob Temp File: ",file.name.temp, sep =""))
      load(file.name.temp,.GlobalEnv) 
    } else {
      
      
      load_DT_table(n,training_set)
     
      
      switch(n,
             "1" = {
               DT.uni.prob <<- as.data.table(DT.uni, key = "t1")
               DT.uni.prob <<- DT.uni.prob[,freq1:=freq,][,list(t1,freq1),]
               rm(DT.uni,envir =.GlobalEnv)
             },
             "2" = {
               
               DT.bi.prob <<- as.data.table(DT.bi, key = "t1,t2")
               DT.bi.prob <<- DT.bi.prob[,freq2:=freq,][,list(t1,t2,freq2),]
               rm(DT.bi,envir =.GlobalEnv)
             },
             "3" = {
               DT.tri.prob <<- as.data.table(DT.tri, key = "t1,t2,t3")
               DT.tri.prob <<- DT.tri.prob[,freq3:=freq,][,list(t1,t2,t3,freq3),]
               rm(DT.tri,envir =.GlobalEnv)
             },
             "4" = {
               DT.quad.prob <<- as.data.table(DT.quad, key = "t1,t2,t3,t4")
               DT.quad.prob <<- DT.quad.prob[,freq4:=freq,][,list(t1,t2,t3,t4,freq4),]
               rm(DT.quad,envir =.GlobalEnv)
             }
      )
      
    }
  }
  
  switch(n, # Calculate important values for Knersey-ney, including discount values 
         # (D2, D3, D4) for lambda function
         
         "1" = {
           n.uni <<- nrow(DT.uni.prob)
           numwords.uni <<- sum(DT.uni.prob$freq1)
         },
         
         "2" = {
           
           #   n1: number of bigrams that occurs exactly p1 times
           #   n2: number of bigrams that occurs exactly p1+1 times
           #   D2 = n1 / (n1 + 2 * n2)
           
           numwords.bi <<- sum(DT.bi.prob$freq2)
           
           n.bi <<- nrow(DT.bi.prob)
           n1.bi <<- nrow(DT.bi.prob[freq2 == p1,,])
           n2.bi <<- nrow(DT.bi.prob[freq2 == p2,,])
           
           D2 <<- n1.bi / (n1.bi + 2 * n2.bi) 
         },
         
         "3" = {
           #   n1: number of trigrams that occurs exactly p1 times
           #   n2: number of trigrams that occurs exactly p1+1 times
           #   D3 = n1 / (n1 + 2 * n2)
           numwords.tri <<- sum(DT.tri.prob$freq3)
           
           n.tri <<- nrow(DT.tri.prob)
           n1.tri <<- nrow(DT.tri.prob[freq3 == p1,,])
           n2.tri <<- nrow(DT.tri.prob[freq3 == p2,,])
           D3 <<- n1.tri / (n1.tri + 2 * n2.tri) 
         },
         
         "4" = {
           #   n1: number of quadgrams that occurs exactly p1 times
           #   n2: number of quadgrams that occurs exactly p1+1 times
           #   D4 = n1 / (n1 + 2 * n2)
           numwords.quad <<- sum(DT.quad.prob$freq4)
           
           n.quad <<- nrow(DT.quad.prob)
           n1.quad <<- nrow(DT.quad.prob[freq4 == p1,,])
           n2.quad <<- nrow(DT.quad.prob[freq4 == p2,,])
           D4 <<- n1.quad / (n1.quad + 2 * n2.quad) 
         }
         
  )
  t2 <- proc.time()
  print(paste("-----> FINISH: load_DT_prob_table(","n:=",n," p1:=",p1," training_set:=",training_set,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
}    




####################################
load_DT_prob_tables <- function (n,training_set=80,p1=5) { 
  
  print(paste("-----> INIT: load_DT_prob_tables(n:=",n," p1:=",p1," training_set:=",training_set,").......",sep=""))
  t1 <- proc.time()
  
  switch(n,
         "1" = {
           load_DT_prob_table(1,training_set,p1)
           load_DT_prob_table(2,training_set,p1)
         },
         "2" = {
           load_DT_prob_table(1,training_set,p1)
           load_DT_prob_table(2,training_set,p1)
         },
         "3" = {
           load_DT_prob_table(1,training_set,p1)
           load_DT_prob_table(2,training_set,p1)
           load_DT_prob_table(3,training_set,p1)
         },
         "4" = {
           load_DT_prob_table(3,training_set,p1)
           load_DT_prob_table(4,training_set,p1)
         }
  )
  t2 <- proc.time()
  
  print(paste("-----> FINISH: load_DT_prob_tables(n:=",n," p1:=",p1," training_set:=",training_set,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
}



####################################  
calculate_prob_kn <- function(n,training_set=80,p1=5) {
  print(paste("-----> INIT: calculate_prob_kn(n:=",n," training_set:=",training_set," p1:=",p1,").......",sep=""))
  tic1 <- proc.time()
    
  switch(n,
         "1" = {
           var.name <- "DT.uni.prob"
           file.name <- create_filename("DT_uni_prob",training_set)
           var.name.final <- "DT.uni.prob.final"
           file.name.final <- create_filename("DT_uni_prob_final",training_set)
           file.name.temp <- create_filename("DT_uni_prob_temp",training_set)
           
         },
         "2" = {
           var.name <- "DT.bi.prob"
           file.name <- create_filename("DT_bi_prob",training_set)
           var.name.final <- "DT.bi.prob.final"
           file.name.final <- create_filename("DT_bi_prob_final",training_set)
           file.name.temp <- create_filename("DT_bi_prob_temp",training_set)
         },
         "3" = {
           var.name <- "DT.tri.prob"
           file.name <- create_filename("DT_tri_prob",training_set)
           var.name.final <- "DT.tri.prob.final"
           file.name.final <- create_filename("DT_tri_prob_final",training_set)
           file.name.temp <- create_filename("DT_tri_prob_temp",training_set)
         },
         "4" = {
           var.name <- "DT.quad.prob.final"
           file.name <- create_filename("DT_quad_prob_final",training_set)
           file.name.final <- create_filename("DT_quad_prob_final",training_set)
           var.name.final <- "DT.quad.prob.final"
           file.name.temp <- create_filename("DT_quad_prob_temp",training_set)
         }
  )
  
  #Validate if the DT with the probability exists in the enviroment
  if (!exists(var.name.final)) {
    #Validate if the file exists an load the value
    if (file.exists(file.name.final)) {
      print(paste("... Loading DT Prob Final file: ",file.name, sep =""))
      load(file.name.final,.GlobalEnv) 
    } else {
      
      load_DT_prob_tables(n,training_set,p1)
      
      print(paste("... Calculating DT Prob Table:", var.name, sep=""))
      
      
      switch(n,
             "1" = { # For each unigram lets calculate the kneser-ney prob
               
               #################################################################################################
               # Calculate Knersey Prob for Unigrams
               #  Pkn(t1) := N1+(* t1) / N1+(* *) = N1+(* t1) / (sum(w) N1+(* w))
               
               ##### To calculate this we need: 
               #       N1+(* t1) = n11(t1)            (to be calculated) 
               #       sum(w) N1+(* w) = n11.all      (to be calculated)
               
               print("---> Calculating and Adding neccesary values for Unigrams Prob Calculation: Pkn(t1) ...")
               
               # Calculate n11 
               print("... Calculating: N1+(* t1) = n11(t1) ...")
               DT.uni.temp <- copy(DT.uni.prob)
               DT.bi.temp <- copy(DT.bi.prob)
               
               setkey(DT.uni.temp,"t1")
               setkey(DT.bi.temp,"t2")
               DT.n11 <- DT.bi.temp[DT.uni.temp, .N, by = .EACHI] [,c("t1","n11","t2","N"):=list(t2,N,NULL,NULL)]
               DT.uni.prob <- merge(DT.uni.prob, DT.n11 , by = "t1")
               rm(DT.uni.temp,DT.bi.temp)
               gc()
               
               # Calculate n11.all 
               print("... Calculating: sum (w) N1+(* w) = n11.all ...")
               
               n11.all <<- DT.uni.prob[,sum(n11),]
               
               # Final Calculation
               print("---> Calculating: Kneser-ney Prob for Unigrams: Pkn(t1) ...")
               print("     Pkn(t1) = N1+(* t1) / N1+(* *) = N1+(* t1) / (sum(w) N1+(* w)) ...")
               DT.uni.prob[,pkn12:= n11 / n11.all]
               
               #################################################################################################
               # Calculate MLE Prob for Unigrams
               #  MLE(t1)= c(t1) / (sum(w) c(w)) 
               
               ##### To calculate this we need: 
               #       c(t1) = freq1(t1)      (on Unigram Prob Table) 
               #       sum(w) c(w) = uni.all  (to be calculated)
               
               # Calculate MLE of Unigrams
               print("... Calculating: MLE(t1) = c(t1) / (sum(w c(w) ... ")
               
               uni.all <<- DT.uni.prob[, sum(freq1) ,]
               DT.uni.prob[,pkn11:= freq1 / uni.all]
               
               print(paste("... Saving DT probability Temp file:",file.name.temp,sep=""))
               save(DT.uni.prob,file=file.name.temp)
               
               # Clean the DT with only three columns (t1,freq1,prob)
               DT.uni.prob[,prob:=pkn11]
               DT.uni.prob.final <<- DT.uni.prob[,list(t1,freq1,prob),]

               rm(DT.uni.prob,envir =.GlobalEnv)
               gc()
               print(paste("... Saving DT probability final file:",file.name.final,sep=""))
               save(DT.uni.prob.final,file=file.name.final)
             },
             
             "2" = {
              
               #################################################################################################
               # Calculate Knersey Prob for High order Bigrams
               #  Pkn(t1 t2) := max{ c(t1 t2) - D2, 0 } / (sum(w) c(t1 w)) + D2 / (sum(w) c(t1 w)) * N1+(t1 *) x Pknr (t2)
               
               ##### To calculate this we need 
               #       c(t1 t2) = freq2(t1 t2)        (on Bigrams Prob Table) 
               #       sum(w) c(t1 w) = sum.freq2(t1) (to be calculated)
               #       D2                             (already calculated on init)
               #       N1+(t1 *) = n12(t1)            (to be calculated)
               #       Pknr(t2)  = pkn12(t2)          (to be calculated)
               
               print("---> Calculating and Adding neccesary values for Bigrams High Order Prob Calculation: Pkn(t1 t2) ...")
               
               # Let's calculate sum.freq2
               print("... Calculating: sum(w) c(t1 w) = sum.freq2(t1) ...")
               DT.bi.prob[, sum.freq2:= sum(freq2), by = "t1"]
              
               # Let's add n12(t1) to the Bigrams Table
               print("... Calculating: N1+(t1 *) = n12(t1) ...")
               DT.bi.prob[,n12:=.N,by="t1"]
               
               # Let's add pkn12(t2) to the Bigrams Table
               print("... Adding to Bigrams Table: pkn12(t2) ...")
               DT.uni.temp1 <- copy(DT.uni.prob)
               DT.uni.temp1 <- DT.uni.temp1[,list(t1,pkn12),]
               DT.uni.temp1[,c("t2","pkn12","t1"):= list(t1,pkn12,NULL)] # Change t1 for t2 for merge
               DT.bi.prob <- merge(DT.bi.prob, DT.uni.temp1 , by = "t2")
               rm(DT.uni.temp1)
               gc()
               
               # Let's calculate Pkn(t1 t2)
               print("--> Calculating Kneser-ney Prob for High Order Bigrams ...")
               print("    Pkn(t1 t2) = max{ c(t1 t2) - D2, 0 } / (sum(w) c(t1 w)) + ")
               print("                    D2 / (sum(w) c(t1 w)) * N1+(t1 *) x Pknr (t2) ...")
 
               DT.bi.prob[, pkn21:= 
                            ifelse(D2 > freq2,0,freq2 - D2) /  sum.freq2 + 
                            D2 / sum.freq2 * n12 * # lambda
                            pkn12,]
               
            
               
               print(paste("... Saving DT probability Temp file:",file.name.temp,sep=""))
               save(DT.bi.prob,file=file.name.temp)
               
               # Clean the DT with only four columns (t1,t2,freq2,prob)
               
               DT.bi.prob[,prob:=pkn21]
               DT.bi.prob.final <<- DT.bi.prob[,list(t1,t2,freq2,prob),]
               
               rm(DT.bi.prob,envir =.GlobalEnv)
               gc()
               print(paste("... Saving DT probability final file:",file.name.final,sep=""))
               save(DT.bi.prob.final,file=file.name.final)
               
             },
             
             "3" = {
               
               #################################################################################################
               # Calculate Knersey Prob for Trigrams
               #  Pkn(t1 t2 t3) = max{ c(t1 t2 t3) - D3, 0 } / (sum(w) c(t1 t2 w)) + 
               #                  D3 / (sum(w) c(t1 t2 w)) * N1+(t1 t2 *) x Pknr(t2 t3)
               
               ##### To calculate this we need 
               #       c(t1 t2 t3) = freq3(t1 t2 t3)            (on Trigrams Prob Table) 
               #       sum(w) c(t1 t2 w) = sum.freq3(t1 t2)     (to be calculated)
               #       D3                                       (already calculated on init)
               #       N1+(t1 t2 *) = n22(t1 t2)                (to be calculated)
               #       Pknr(t2 t3)  = pkn22(t2 t3)              (to be calculated)
               
               print("---> Calculating and Adding neccesary values for Trigrams High Order Prob Calculation: Pkn(t1 t2 t3) ...")
               
               # Let's calculate and add to Trigrams Table: sum.freq3(t1 t2)
               print("... Calculating: sum(w) c(t1 t2 w) = sum.freq3(t1 t2) ...")
               DT.tri.prob[, sum.freq3:= sum(freq3), by = c("t1","t2")]
               
               # Let's calculate and add to Trigrams Prob Table: n22(t1 t2) 
               print("... Calculating: N1+(t1 t2 *) = n22(t1 t2) ...")
               DT.tri.prob[, n22:= .N , by = c("t1","t2")]
               
               ##########################################################################################
               # Let's calculate PKnr(t2 t3) and add to the Trigrams Table
               
               #  Pknr(t2 t3) = max{ N1+(* t2 t3) - D2, 0 } / (sum(w) N1+(* t2 w)) + 
               #                D2 / (sum(w) N1+(* t2 w)) * N1+(t2 *) x Pknr (t3)
              
               ##### To calculate this we need 
               #       N1+(* t2 t3) = n21(t2 t3)                (to be calculated) 
               #       sum(w) N1+(* t2 w) = sum.n21(t2)         (to be calculated)
               #       D2                                       (already calculated on init)
               #       N1+(t2 *) = n12(t2)                      (on Bigram Prob Table)
               #       Pknr(t3) = pkn12(t3)                     (on Unigram Prob Table)

               print("... Calculating and Adding neccesary values for Bigrams Low Order Prob Calculation: Pknr(t2 t3)...")
               
               # Let's calculate and add to Trigrams Table: n21(t2 t3)
               print("...... Calculating: N1+(* t2 t3) = n21(t2 t3) ...")
               
               DT.tri.prob[, n21:= .N, by = c("t2","t3")]
               
               # Let's calculate and add to Trigrams Table: sum.n21(t2)
               print("...... Calculating: sum(w) N1+(* t2 w) = sum.n21(t2) ...")
               #DT.tri.prob[, sum.n21:= .N , by = "t2"]
               
               DT.tri.prob[, sum.n21:= sum(n21) , by = "t2"]
               
               # Let's add n12(t2) to the Trigrams Table
               print("...... Adding to Trigrams Table: n12(t2) ...")
               DT.bi.temp1 <- copy(DT.bi.prob)
               DT.bi.temp1 <- DT.bi.temp1[,list(t1,n12),]
               DT.bi.temp1[, c("t2","n12","t1"):=list(t1,n12,NULL), ]
               setkey(DT.bi.temp1,"t2")
               DT.bi.temp1 <- unique(DT.bi.temp1)
               DT.tri.prob <- merge(DT.tri.prob, DT.bi.temp1 , by = "t2")
               
               # Let's add pkn12(t3) to the Trigrams Table
               print("...... Adding to Trigrams Table: pkn12(t3) ...")
               DT.bi.temp1 <- copy(DT.bi.prob)
               DT.bi.temp1 <- DT.bi.temp1[,list(t2,pkn12),]
               DT.bi.temp1[,c("t3","pkn12","t2"):= list(t2,pkn12,NULL)] # Change t1 for t3 for merge
               setkey(DT.bi.temp1,"t3")
               DT.bi.temp1 <- unique(DT.bi.temp1)
               DT.tri.prob <- merge(DT.tri.prob, DT.bi.temp1 , by = "t3")
               
               rm(DT.bi.temp1)
               gc()
               
               # Let's calculate Pknr(t2 t3)
               print("...... Calculating Kneser-ney Prob for Low Order Bigrams = pkn22(t2 t3)...")
               print("...... Pknr(t2 t3) = max{ N1+(t2 t3) - D2, 0 } / (sum(w) N1+(* t2 w)) + ")
               print("                     D2 / (sum(w) N1+(* t2 w)) * N1+(t2 *) x Pknr(t3) ...")
               
              
               DT.tri.prob[, pkn22:= 
                             ifelse(D2 > n21,0,n21 - D2) /  sum.n21 + 
                             D2 / sum.n21 * n12 * # lambda
                             pkn12,]
               
               # Let's calculate Pkn(t1 t2 t3)
               print("--> Calculating Kneser-ney Prob for High Order Trigrams = pkn31(t1 t2 t3)...")
               print("    Pkn(t1 t2 t3) = max{ c(t1 t2 t3) - D3, 0 } / (sum(w) c(t1 t2 w)) + ")
               print("                    D3 / (sum(w) c(t1 t2 w)) * N1+(t1 t2 *) x Pknr (t2 t3)")
               
               DT.tri.prob[, pkn31:= 
                             ifelse(D3 > freq3,0,freq3 - D3) /  sum.freq3 + 
                             D3 / sum.freq3 * n22 * # lambda
                             pkn22,]
               
              
               
               print(paste("... Saving DT probability Temp file:",file.name.temp,sep=""))
               save(DT.tri.prob,file=file.name.temp)
               
               # Clean the DT with only five columns (t1,t2,t3,freq3,prob)
               
               DT.tri.prob[,prob:=pkn31]
               DT.tri.prob.final <<- DT.tri.prob[,list(t1,t2,t3,freq3,prob),]
               
               rm(DT.tri.prob,envir =.GlobalEnv)
               gc()
               
               print(paste("... Saving DT probability final file:",file.name.final,sep=""))
               save(DT.tri.prob.final,file=file.name.final)
               
             },
             "4" = {
               
               #################################################################################################
               # Calculate Knersey Prob for Quadgrams
               #  Pkn(t1 t2 t3 t4) = max{ c(t1 t2 t3 t4) - D4, 0 } / (sum(w) c(t1 t2 t3 w)) + 
               #                     D4 / (sum(w) c(t1 t2 t3 w)) * N1+(t1 t2 t3 *) x Pknr(t2 t3 t4)
               
               ##### To calculate this we need 
               #       c(t1 t2 t3 t4) = freq4(t1 t2 t3 t4)            (on Quadgrams Prob Table) 
               #       sum(w) c(t1 t2 t3 w) = sum.freq4(t1 t2 t3)     (to be calculated)
               #       D4                                             (already calculated on init)
               #       N1+(t1 t2 t3 *) = n32(t1 t2 t3)                (to be calculated)
               #       Pknr(t2 t3 t4)  = pkn32(t2 t3 t4)              (to be calculated)
               
               print("---> Calculating and Adding neccesary values for Quadgrams High Order Prob Calculation: Pkn(t1 t2 t3 t4) ...")
               
               # Let's calculate and add to Quadgrams Table: sum.freq4(t1 t2 t3)
               print("... Calculating: sum(w) c(t1 t2 t3 w) = sum.freq4(t1 t2 t3) ...")
               DT.quad.prob[, sum.freq4:= sum(freq4), by = c("t1","t2","t3")]
               
               # Let's calculate and add to Quadgrams Prob Table: n32(t1 t2 t3) 
               print("... Calculating: N1+(t1 t2 t3 *) = n32(t1 t2 t3) ...")
               DT.quad.prob[, n32:= .N , by = c("t1","t2","t3")]
               
               ##########################################################################################
               # Let's calculate PKnr(t2 t3 t4) and add to the Quadgrams Table
               
               #  Pknr(t2 t3 t4) = max{ N1+(* t2 t3 t4) - D3, 0 } / (sum(w) N1+(* t2 t3 w)) + 
               #                   D3 / (sum(w) N1+(* t2 t3 w)) * N1+(t2 t3 *) x Pknr(t3 t4)
               
               ##### To calculate this we need 
               #       N1+(* t2 t3 t4) = n31(t2 t3 t4)                (to be calculated) 
               #       sum(w) N1+(* t2 t3 w) = sum.n31(t2 t3)         (to be calculated)
               #       D3                                             (already calculated on init)
               #       N1+(t2 t3 *) = n22(t2 t3)                      (on Trigram Prob Table)
               #       Pknr(t3 t4)  = pkn22(t3 t4)                    (on Trigram Prob Table)
               
               print("... Calculating and Adding neccesary values for Trigrams Low Order Prob Calculation: Pknr(t2 t3 t4)...")
               
               # Let's calculate and add to Quadgrams Table: n31(t2 t3 t4)
               print("...... Calculating: N1+(* t2 t3 t4) = n31(t2 t3 t4) ...")
               
               DT.quad.prob[, n31:= .N, by = c("t2","t3","t4")]
               
               # Let's calculate and add to Quadgrams Table: sum.n31(t2 t3)
               print("...... Calculating: sum(w) N1+(* t2 t3 w) = sum.n31(t2 t3) ...")
               #DT.quad.prob[, sum.n31:= .N , by = c("t2","t3")]
               DT.quad.prob[, sum.n31:= sum(n31) , by = c("t2","t3")]
               
               # Let's add n22(t2 t3) to the Quadgrams Table
               print("...... Adding to Quadgrams Table: n22(t2 t3) ...")
               DT.tri.temp1 <- copy(DT.tri.prob)
               DT.tri.temp1 <- DT.tri.temp1[,list(t1,t2,n22),]
               DT.tri.temp1[, c("t2","t3","n22","t1"):=list(t1,t2,n22,NULL), ]
               setkeyv(DT.tri.temp1,c("t2","t3"))
               DT.tri.temp1 <- unique(DT.tri.temp1)
               DT.quad.prob <- merge(DT.quad.prob, DT.tri.temp1 , by = c("t2","t3"))
               
               # Let's add pkn22(t3 t4) to the Quadgrams Table
               print("...... Adding to Quadgrams Table: pkn22(t3 t4) ...")
               DT.tri.temp1 <- copy(DT.tri.prob)
               DT.tri.temp1 <- DT.tri.temp1[,list(t2,t3,pkn22),]
               DT.tri.temp1[,c("t3","t4","pkn22_1","t2"):= list(t2,t3,pkn22,NULL)] # Change t2 for t3 & t3 for t4 for merge
               setkeyv(DT.tri.temp1,c("t3","t4"))
               DT.tri.temp1 <- unique(DT.tri.temp1)
               DT.quad.prob <- merge(DT.quad.prob, DT.tri.temp1 , by = c("t3","t4"))
               rm(DT.tri.temp1)
               gc()
               
               # Let's calculate Pknr(t2 t3 t4)
               print("...... Calculating Kneser-ney Prob for Low Order Trigrams = pkn32(t2 t3 t4)...")
               print("...... Pknr(t2 t3 t4) = max{ N1+(t2 t3 t4) - D3, 0 } / (sum(w) N1+(* t2 t3 w)) + ")
               print("                        D3 / (sum(w) N1+(* t2 t3 w)) * N1+(t2 t3 *) x Pknr(t3 t4) ...")
               
               DT.quad.prob[, pkn32:= 
                              ifelse(D3 > n31,0,n31 - D3) /  sum.n31 + 
                              D3 / sum.n31 * n22 * # lambda
                              pkn22,]

               # Let's calculate Pkn(t1 t2 t3 t4)
               print("--> Calculating Kneser-ney Prob for High Order Quadgrams = pkn41(t1 t2 t3 t4)...")
               print("    Pkn(t1 t2 t3 t4) = max{ c(t1 t2 t3 t4) - D4, 0 } / (sum(w) c(t1 t2 t3 w) + ")
               print("                       D4 / (sum(w) c(t1 t2 t3 w)) * N1+(t1 t2 t3 *) x Pknr (t2 t3 t4)")
               
               DT.quad.prob[, pkn41:= 
                              ifelse(D4 > freq4,0,freq4 - D4) /  sum.freq4 + 
                              D4 / sum.freq4 * n32 * # lambda
                              pkn32,]
               
               print(paste("... Saving DT probability Temp file:",file.name.temp,sep=""))
               save(DT.quad.prob,file=file.name.temp)
               
               # Clean the DT with only six columns (t1,t2,t3,t4,freq4,prob)
               
               DT.quad.prob[,prob:=pkn41]
               DT.quad.prob.final <<- DT.quad.prob[,list(t1,t2,t3,t4,freq4,prob),]
               
               rm(DT.quad.prob,envir =.GlobalEnv)
               gc()
               
               print(paste("... Saving DT probability final file:",file.name.final,sep=""))
               save(DT.quad.prob.final,file=file.name.final)
             }
      )
      tic2 <- proc.time()
      print(paste("-----> FINISH: calculate_prob_kn(n:=",n," training_set:=",training_set," p1:=",p1,"): Running Time .......",
                  elapsed_time(tic1,tic2)," seconds ...",sep=""))
    }
  }
}
