#####################################################################
# Create Ngram Data Table VFinal:
#          Generate the information about the frequency of 
#          the differents ngrams (unigram, bigram, trigram and quadgrams)
#          to be used later with Kneser-ney algortihm to calculate the
#          probability of each ngram for predict the next word.
#          This script was run on my PC and takes long time to finish,
#          at the end this script they produce the following files:
#
#             DT_uni_x.Rdata: data table that include all the unigrams and the
#                          frequency for each one.
#             DT_bi_x.Rdata: data table that include all the bigrams and the
#                          frequency for each one.
#             DT_tri_x.Rdata: data table that include all the trigrams and the
#                          frequency for each one.
#             DT_quad_x.Rdata: data table that include all the quadgrams and the
#                          frequency for each one.
#      Where X represent the number of lines of each file (twitter, news and blogs)
#      that was readed. In case of all lines, x = "all"


library(quanteda)
library(data.table)
library(stringi)

#setwd("D:/Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")
#setwd("D:/001 -- Coursera/Capstone Project/Coursera-SwiftKey/final/en_US")

####################################
# For print elapsed time
elapsed_time <- function(tic1,tic2) {
  format((tic2-tic1)[3][[1]], digits = 2)
}

####################################
# For file name creation
create_filename <- function(x,training_set) {
  
  if (training_set < 0) {
    paste(x,"_all.RData",sep= "")
  }
  else {
    paste(x,"_",training_set,".RData",sep= "")
  }
}


####################################
# Load, filter and transform the data (twitter, news, blogs). The
# lines parameter define the number of lines per each dataset to consider.
# If lines = -1, read all lines.

create_mydata <- function(list_filenames=NULL,training_set=80) {

  print(paste("-----> INIT: create_mydata(",training_set,").......",sep=""))
  t1 <- proc.time()
    
  var.name <- "mydata"
  file.name <- create_filename("mydata",training_set)
  
  #Validate if "mydata" exists in the enviroment
  if (!exists(var.name)) {
    
    #Validate if "mydata.Rdata" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("... Loading file: ",file.name,"....",sep=""))
      load(file.name,.GlobalEnv)
    }  
    else {
      ## Load the data of the files 
      l <- length(list_filenames)
      mydata <- c()
      for (i in 1:l)
      {
        filename <- list_filenames[i]
        print(paste("... Loading the Data from the file: ",filename," ...", sep =""))
        file.data <- readLines(filename, encoding="UTF-8", warn = FALSE)        
        mydata <- c(mydata,file.data)
      }
      
      if (training_set < 100)
      {
        print(paste("... Taking a Training Sample of: ", training_set,"% ...",sep=""))
        mydata <- sample(mydata, length(mydata) * training_set / 100)
      }
      
      # Remove emojies and other characters.
      print("... Removing emojies and other characters ....")
      mydata <- iconv(mydata, "latin1", "ASCII", sub="")
      mydata <- iconv(mydata, "ISO-8859-2", "ASCII", sub="")
      
      # toLower
      print("... To Lower Data ....")
      mydata <- toLower(mydata)
      
      # Replace URL 
      print("... Replace URL ....")
      mydata <- stri_replace_all_regex(mydata,"(f|ht)tp(s?)://(.*)[.][a-z]+", " ", vectorize_all=FALSE)
      
      # Email
      print("... Replace Email ....")
      mydata <- stri_replace_all_regex(mydata,"[[:alnum:].-_]+@[[:alnum:].-_]+", " ", vectorize_all=FALSE)

      # Twitter 
      print("... Replace twitter ....")
      mydata <- stri_replace_all_regex(mydata,"@[[:alnum:].-_]+", " ", vectorize_all=FALSE)
      
      # Hastag  
      print("... Replace Hashtag ....")
      mydata <- stri_replace_all_regex(mydata,"#[[:alnum:].-_]+", " ", vectorize_all=FALSE)
      
      # To preserve  special english character ' between words replace for ffff 
      print("... Replacing special english character between words ' for special character ffff ....")   
      mydata <- stri_replace_all_regex(mydata,"([:alpha:]+)'([:alpha:]+)", "$1 ffff $2", vectorize_all=FALSE)
      
      # Remove left '
      print("... Replacing left ' ....")   
      mydata <- stri_replace_all_regex(mydata,"'+", " ", vectorize_all=FALSE)
      
      # Replace punctuation for a special word "eeee" in order to 
      # avoid some ngrams that doesn't exists
      print("... Replacing punctuation for special characters ....")   
      mydata <- stri_replace_all_regex(mydata,"[.!?,;:]+", " eeee ", vectorize_all=FALSE)
      
      # Remove characters FALTA * [] ¬ ¬° !1¬#$%&/()=?¡´¨*¨{}{}
      print("... Replacing $ + < > ....")    
      mydata <- stri_replace_all_regex(mydata,"[\\[\\]\\+\\-(){}°$#@<>=_%&¿´¨\\^~]+", " ", vectorize_all=FALSE)
      
      # Words that start with numbers: 123end  
      print("... Replace Word that start with numbers ....")
      mydata <- stri_replace_all_regex(mydata,"[:digit:]+[:alpha:]+", " " , vectorize_all=FALSE)
      
      # Words that finish with numbers: end123  
      print("... Replace Word that finish with numbers ....")
      mydata <- stri_replace_all_regex(mydata,"[:alpha:]+[:digit:]+", " " , vectorize_all=FALSE)

      # Digits   
      print("... Replace Digits ....")
      mydata <- stri_replace_all_regex(mydata,"[:digit:]+", " " , vectorize_all=FALSE)
      
      # Replacing rest of puntuation 
      print("... Replacing rest of punctuation ....")   
      mydata <- stri_replace_all_regex(mydata,"[:punct:]+", " ", vectorize_all=FALSE)
      
      # Put back '  
      print("... Put back ' ...")   
      mydata <- stri_replace_all_regex(mydata," ffff ", "'", vectorize_all=FALSE)
      
      # Final Data    
      mydata <<- mydata
      
      print(paste("... Saving mydata file:",file.name,sep=""))
      save(mydata,file=file.name)
      
      rm(filename,file.data)
    }
  }
  t2 <- proc.time()
  print(paste("-----> FINISH: create_mydata(",training_set,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  gc()
}




####################################
create_alltokens <- function(list_filenames=NULL,training_set=80) {
  
  print(paste("-----> INIT: create_alltokens(training_set:=",training_set,").......",sep=""))
  t1 <- proc.time()
  
  
  var.name <- "alltokens"
  file.name <- create_filename("alltokens",training_set)
  
  #Validate if "alltokens" exists in the enviroment
  if (!exists(var.name)) {
    #Validate if "alltokens.Rdata" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("... Loading file",file.name," ...."))
      load(file.name,.GlobalEnv) 
    }  
    else {
      ## Load "mydata" 
      create_mydata(list_filenames,training_set)
      
      print("... Creating alltokens ...")
      # Create "alltokens" and save it into the file
      alltokens <<- tokenize(mydata, what = "fastestword", 
                        removeNumbers = FALSE, 
                        removePunct = FALSE,
                        removeSymbols = FALSE, 
                        removeSeparators = FALSE, 
                        removeTwitter = FALSE,
                        removeHyphens = FALSE, 
                        removeURL = FALSE, 
                        verbose = TRUE)
      
      print(paste("... Saving alltokens file:",file.name,sep=""))
      save(alltokens,file=file.name)
      rm(mydata,envir =.GlobalEnv)
    }
  }
  t2 <- proc.time()
  print(paste("-----> INIT: create_alltokens(",training_set,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  gc()
}

####################################
create_ngram <- function(n,list_filenames=NULL,training_set=80)
{
  print(paste("-----> INIT: create_ngram(n:=",n," training_set:=",training_set,").......",sep=""))
  t1 <- proc.time()
  
  switch(n,
         "1" = {
           var.name <- "uni.ngram"
           file.name <- create_filename("uni_ngram",training_set)
         },
         "2" = {
           var.name <- "bi.ngram"
           file.name <- create_filename("bi_ngram",training_set)
         },
         "3"={
           var.name <- "tri.ngram"
           file.name <- create_filename("tri_ngram",training_set)
         },
         "4"={
           var.name <- "quad.ngram"
           file.name <- create_filename("quad_ngram",training_set)
         }
  )

  #Validate if the "var.name" exists in the enviroment
  if (!exists(var.name)) {
    #Validate if "file.name" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("... Loading ngram file: ",file.name, sep =""))
      load(file.name,.GlobalEnv) 
    }  else {
      ## Load "alltokens" and create the ngrams
      create_alltokens(list_filenames,training_set)
      
      
      print(paste("... Creating Ngram:",var.name,sep=""))
      
      switch(n,
             "1"= { #Unigrams
               uni.ngram <<- ngrams(alltokens, 1)
               print(paste("... Saving Ngram file:",file.name, sep=""))
               save(uni.ngram,file=file.name)
             },
             "2"= { #Bigrams
               bi.ngram <<- ngrams(alltokens, 2)
               print(paste("... Saving Ngram file:",file.name,sep=""))
               save(bi.ngram,file=file.name)
             },
             "3"= { #Trigrams
               tri.ngram <<- ngrams(alltokens, 3)
               print(paste("... Saving Ngram file:",file.name,sep=""))
               save(tri.ngram,file=file.name)
             },
             "4"= { #Quadgrams
               quad.ngram <<- ngrams(alltokens, 4)
               print(paste("... Saving Ngram file:",file.name,sep=""))
               save(quad.ngram,file=file.name)
             }
      )
   
      rm(alltokens,envir =.GlobalEnv)
    }
    
  }
  t2 <- proc.time()
  print(paste("-----> FINISH: create_ngram(n:=",n," training_set:=",training_set,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  
  gc()
}
    
####################################   
clean_ngram <- function(n,list_filenames=NULL,training_set=80)
{
  print(paste("-----> INIT: clean_ngram(n:= ",n," training_set:=",training_set,").......",sep=""))
  t1 <- proc.time()
  
  # List of Profanity words to be removed
  profanityList <- c("shit","piss","fuck","cunt","cocksucker","motherfucker","tits")
  
  switch(n,
         "1" = {
           var.name <- "uni.ngram.clean"
           file.name <- create_filename("uni_ngram_clean",training_set)
           ngram.name <- "uni.ngram"
         },
         "2" = {
           var.name <- "bi.ngram.clean"
           file.name <- create_filename("bi_ngram_clean",training_set)
           ngram.name <- "bi.ngram"
         },
         "3" = {
           var.name <- "tri.ngram.clean"
           file.name <- create_filename("tri_ngram_clean",training_set)
           ngram.name <- "tri.ngram"
         },
         "4" = {
           var.name <- "quad.ngram.clean"
           file.name <- create_filename("quad_ngram_clean",training_set)
           ngram.name <- "quad.ngram"
         }
  )
  
  #Validate if the "var.name" exists in the enviroment
  if (!exists(var.name)) {
    #Validate if "file.name" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("... Loading ngram cleaned file: ",file.name, sep =""))
      load(file.name,.GlobalEnv) 
    }  else {
      ## Load the ngrams and cleaned it  
     
      create_ngram(n,list_filenames,training_set)
      
      
      print(paste("... Cleaning Ngram: ",ngram.name, sep =""))
      
      switch(n,
             "1" = {
               uni.ngram.clean <<- 
                 selectFeatures(uni.ngram, c(profanityList,"eeee"),
                                selection = "remove", valuetype = "regex")
               rm(uni.ngram,envir =.GlobalEnv)
               print(paste("... Saving Ngram Cleaned file: ",file.name, sep =""))
               save(uni.ngram.clean,file=file.name)
             },
             "2" = {
               bi.ngram.clean <<- 
                 selectFeatures(bi.ngram, c(profanityList,"eeee"),
                                selection = "remove", valuetype = "regex")
               rm(bi.ngram,envir =.GlobalEnv)
               print(paste("... Saving Ngram Cleaned file: ",file.name, sep =""))
               save(bi.ngram.clean,file=file.name)
             },
             "3" = {
               tri.ngram.clean <<- 
                 selectFeatures(tri.ngram, c(profanityList,"eeee"),
                                selection = "remove", valuetype = "regex")
               rm(tri.ngram,envir =.GlobalEnv)
               print(paste("... Saving Ngram Cleaned file: ",file.name, sep =""))
               save(tri.ngram.clean,file=file.name)
             },
             "4" = {
               quad.ngram.clean <<- 
                 selectFeatures(quad.ngram, c(profanityList,"eeee"),
                                selection = "remove", valuetype = "regex")
               rm(quad.ngram,envir =.GlobalEnv)
               print(paste("... Saving Ngram Cleaned file: ",file.name, sep =""))
               save(quad.ngram.clean,file=file.name)
             }
      )
    }
  }
  t2 <- proc.time()
  print(paste("-----> INIT: clean_ngram(n:= ",n," training_set:=",training_set,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  gc() 
}
  
####################################  
create_dfm <- function(n,list_filenames=NULL,training_set=80) {
  
  print(paste("-----> INIT: create_dfm(n:=",n," training_set:=",training_set,").......",sep=""))
  t1 <- proc.time()
  
  switch(n,
         "1" = {
           var.name <- "uni.dfm"
           file.name <- create_filename("uni_dfm",training_set)
           ngram.clean.name <- "uni.ngram.clean"
         },
         "2" = {
           var.name <- "bi.dfm"
           file.name <- create_filename("bi_dfm",training_set)
           ngram.clean.name <- "bi.ngram.clean"
         },
         "3" = {
           var.name <- "tri.dfm"
           file.name <- create_filename("tri_dfm",training_set)
           ngram.clean.name <- "tri.ngram.clean"
         },
         "4" = {
           var.name <- "quad.dfm"
           file.name <- create_filename("quad_dfm",training_set)
           ngram.clean.name <- "quad.ngram.clean"
         }
  )
  
  #Validate if the "var.name" exists in the enviroment
  if (!exists(var.name)) {
    #Validate if "file.name" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("... Loading dfm file:", file.name, sep=""))
      load(file.name,.GlobalEnv) 
    }  else {
      ## Load the ngrams and cleaned it 
      clean_ngram(n,list_filenames,training_set)
      
      print(paste("... Creating dfm:", var.name, sep=""))
      
      switch(n,
             "1" = {
               uni.dfm <<- dfm(uni.ngram.clean,toLower = FALSE)
               rm(uni.ngram.clean,envir =.GlobalEnv)
               print(paste("... Saving dfm file:", file.name, sep=""))
               save(uni.dfm,file=file.name)
             },
             "2" = {
               bi.dfm <<- dfm(bi.ngram.clean,toLower = FALSE)
               rm(bi.ngram.clean,envir =.GlobalEnv)
               print(paste("... Saving dfm file:", file.name, sep=""))
               save(bi.dfm,file=file.name)
             },
             "3" = {
               tri.dfm <<- dfm(tri.ngram.clean,toLower = FALSE)
               rm(tri.ngram.clean,envir =.GlobalEnv)
               print(paste("... Saving dfm file:", file.name, sep=""))
               save(tri.dfm,file=file.name)
             },
             "4" = {
               quad.dfm <<- dfm(quad.ngram.clean,toLower = FALSE)
               rm(quad.ngram.clean,envir =.GlobalEnv)
               print(paste("... Saving dfm file:", file.name, sep=""))
               save(quad.dfm,file=file.name)
             }
      )
    }
  }  
  t2 <- proc.time()
  print(paste("-----> FINISH: create_dfm(n:=",n," training_set:=",training_set,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  gc()
}

####################################
trim_dfm <- function(n,list_filenames=NULL,training_set=80,mincount=5) {
  
  print(paste("-----> INIT: trim_dfm(n:=",n," training_set:=",training_set," mincount:=",mincount,").......",sep=""))
  t1 <- proc.time()
  
  switch(n,
         "1" = {
           var.name <- "uni.dfm.trim"
           file.name <- create_filename("uni_dfm_trim",training_set)
           dfm.name <- "uni.dfm"        
         },
         "2" = {
           var.name <- "bi.dfm.trim"
           file.name <- create_filename("bi_dfm_trim",training_set)
           dfm.name <- "bi.dfm"
         },
         "3" = {
           var.name <- "tri.dfm.trim"
           file.name <- create_filename("tri_dfm_trim",training_set)
           dfm.name <- "tri.dfm"
         },
         "4" = {
           var.name <- "quad.dfm.trim"
           file.name <- create_filename("quad_dfm_trim",training_set)
           dfm.name <- "quad.dfm"       
         }
  )

  #Validate if the "var.name" exists in the enviroment
  if (!exists(var.name)) {
    #Validate if "file.name" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("... Loading dfm trim file:",file.name,sep=""))
      load(file.name,.GlobalEnv) 
    }  else {
      ## Load the dfm  
      create_dfm(n,list_filenames,training_set)
      
      
      print(paste("... Trim dfm:", var.name, sep=""))
      
      switch(n,
             "1" = {
               if (mincount <= 1) {
                 uni.dfm.clean <<- uni.dfm
               } else {
                 uni.dfm.clean <<- trim(uni.dfm,minCount = mincount)  
               }
               rm(uni.dfm,envir =.GlobalEnv)
               gc()
               print("... Saving dfm clean: uni.dfm.clean ..")
               save(uni.dfm.clean,file=file.name)
             },
             "2" = {
               if (mincount <= 1) {
                 bi.dfm.clean <<- bi.dfm
               } else {
                 bi.dfm.clean <<- trim(bi.dfm,minCount = mincount)  
               }
               
               rm(bi.dfm,envir =.GlobalEnv)
               gc()
               print("... Saving dfm clean: bi.dfm.clean ..")
               save(bi.dfm.clean,file=file.name)
             },
             "3" = {
               if (mincount <= 1) {
                 tri.dfm.clean <<- tri.dfm
               } else {
                 tri.dfm.clean <<- trim(tri.dfm,minCount = mincount)  
               }
               
               rm(tri.dfm,envir =.GlobalEnv)
               gc()
               print("... Saving dfm clean: tri.dfm.clean ..")
               save(tri.dfm.clean,file=file.name)
             },
             "4" = {
               if (mincount <= 1) {
                 quad.dfm.clean <<- quad.dfm
               } else {
                 quad.dfm.clean <<- trim(quad.dfm,minCount = mincount)  
               }
               
               rm(quad.dfm,envir =.GlobalEnv)
               gc()
               print("... Saving dfm clean: quad.dfm.clean ..")
               save(quad.dfm.clean,file=file.name)
             }
      )
    }
  } 
  t2 <- proc.time()
  print(paste("-----> FINISH: trim_dfm(n:=",n," training_set:=",training_set," mincount:=",mincount,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  
  gc()
} 

  
####################################      
create_DT <- function(n,list_filenames=NULL,training_set=80,mincount=5) {
  print(paste("-----> INIT: create_DT(n:=",n," training_set:=",training_set," mincount:=",mincount,").......",sep=""))
  t1 <- proc.time()
  
  switch(n,
         "1" = {
           var.name <- "DT.uni"
           file.name <- create_filename("DT_uni",training_set)
           dfm.name <- "uni.dfm.trim"        
         },
         "2" = {
           var.name <- "DT.bi"
           file.name <- create_filename("DT_bi",training_set)
           dfm.name <- "bi.dfm.trim"           
         },
         "3" = {
           var.name <- "DT.tri"
           file.name <- create_filename("DT_tri",training_set)
           dfm.name <- "tri.dfm.trim"           
         },
         "4" = {
           var.name <- "DT.quad"
           file.name <- create_filename("DT_quad",training_set)
           dfm.name <- "quad.dfm.trim"           
         }
  )
           
  #Validate if the "var.name" exists in the enviroment
  if (!exists(var.name)) {
    #Validate if "file.name" file exists an load the value
    if (file.exists(file.name)) {
      print(paste("... Loading DT file:",file.name,sep=""))
      load(file.name,.GlobalEnv) 
    } else {
      ## Load the dfm trim 
      trim_dfm(n,list_filenames,training_set,mincount)
      
      
      print(paste("... Creating DT:", var.name, sep=""))
      
      switch(n,
             "1" = {
               #
               # Create DT for unigrams:
               #     t1  freq  
               n.uni <- nfeature(uni.dfm.clean)
               top.uni <- topfeatures(uni.dfm.clean,n.uni)
               rm(uni.dfm.clean,envir =.GlobalEnv)
               gc()
               DT.uni <<- data.table(t1=names(top.uni),freq=top.uni)
               print("... Saving DT.uni ..")
               save(DT.uni,file=file.name)
               rm(top.uni,n.uni)    
             },
             "2" = {
               #
               # DT for bigrams:
               #     t1  t2  freq
               n.bi <- nfeature(bi.dfm.clean)
               top.bi <- topfeatures(bi.dfm.clean,n.bi)
               rm(bi.dfm.clean,envir =.GlobalEnv)
               gc()
               DT.bi <<- data.table(V1=names(top.bi),freq=top.bi)
               rm(top.bi,n.bi)
               gc()
               DT.bi[,c("t1","t2") := tstrsplit(V1, "_", fixed=TRUE),]
               DT.bi <<- DT.bi[,list(t1,t2,freq),]
               print("... Saving DT.bi ..")
               save(DT.bi,file=file.name)
             },
             "3" = {
               #
               # DT for trigrams:
               #     t1  t2  t3 freq  
               
               n.tri <- nfeature(tri.dfm.clean)
               top.tri <- topfeatures(tri.dfm.clean,n.tri)
               rm(tri.dfm.clean,envir =.GlobalEnv)
               gc()
               DT.tri <<- data.table(V1=names(top.tri),freq=top.tri)
               rm(top.tri,n.tri)
               gc()
               DT.tri[,c("t1", "t2","t3") := tstrsplit(V1, "_", fixed=TRUE),]
               DT.tri <<- DT.tri[,list(t1,t2,t3,freq),]
               print("... Saving DT.tri ..")
               save(DT.tri,file=file.name)
             },
             "4" = {
               #
               # DT for quadgrams:
               #     t1  t2  t3 t4 freq
               
               n.quad <- nfeature(quad.dfm.clean)
               top.quad <- topfeatures(quad.dfm.clean,n.quad)
               rm(quad.dfm.clean, envir =.GlobalEnv)
               gc()
               DT.quad <<- data.table(V1=names(top.quad),freq=top.quad)
               rm(top.quad,n.quad)
               gc()
               DT.quad <- DT.quad[,c("t1", "t2","t3","t4") := tstrsplit(V1, "_", fixed=TRUE),]
               DT.quad <<- DT.quad[,list(t1,t2,t3,t4,freq),]
               print("... Saving DT.quad ...")
               save(DT.quad,file=file.name)
             }
      )
    }
  } 
  t2 <- proc.time()
  
  print(paste("-----> FINISH: create_DT(n:=",n," training_set:=",training_set," mincount:=",mincount,"): Running Time .......",
              elapsed_time(t1,t2)," seconds ...",sep=""))
  
  gc()
} 