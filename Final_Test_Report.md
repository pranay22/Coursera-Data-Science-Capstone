# Capstone Project - Final Test Report
Pranay Sarkar  
August 16, 2018  




## Executive Summary

This is a Test Final Report related with the Coursera Capstone Project, the target is show the total running steps of the implementation.


## 1. Load the Neccesary Libraries

For this project we will use basicly the **quanteda**,**ggplot2**, **data.table** and **knitr**.


```r
library(quanteda)
```

```
## quanteda version 0.9.6.9
## 
## 
## Attaching package: 'quanteda'
## 
## The following object is masked from 'package:base':
## 
##     sample
```

```r
library(data.table)
library(ggplot2)
library(knitr)

wd.R <- "D:/001 -- Coursera/Capstone Project/Coursera---Data-Science---Capstone-Project"

setwd(wd.R)

source("Create Ngrams Data Table vFinal.R")
source("Knersey-ney Optimazed vFinal.R")
source("Main Predict Word vFinal.R")
source("Pred Next Word Regex vFinal.R")
source("Pred Next Word vFinal.R")

# For reproducibility
set.seed(12345)
```

## 2. Create Ngram Data Table

We will create the ngrams table for our quadgram model using **80%** of the corpora.

### 2.1 Load and Clean the Data

This function load the 80% and clean the data:


```r
list_filenames <- c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt")
create_mydata(list_filenames, 80) 
```

```
## [1] "-----> INIT: create_mydata(80)......."
## [1] "... Loading the Data from the file: en_US.blogs.txt ..."
## [1] "... Loading the Data from the file: en_US.news.txt ..."
## [1] "... Loading the Data from the file: en_US.twitter.txt ..."
## [1] "... Taking a Training Sample of: 80% ..."
## [1] "... Removing emojies and other characters ...."
## [1] "... To Lower Data ...."
## [1] "... Replace URL ...."
## [1] "... Replace Email ...."
## [1] "... Replace twitter ...."
## [1] "... Replace Hashtag ...."
## [1] "... Replacing apostrophe between words (') for special character ffff ...."
## [1] "... Replacing left ' ...."
## [1] "... Replacing punctuation for special characters ...."
## [1] "... Replacing $ + < > ...."
## [1] "... Replace Word that start with numbers ...."
## [1] "... Replace Word that finish with numbers ...."
## [1] "... Replace Digits ...."
## [1] "... Replacing rest of punctuation ...."
## [1] "... Removing Profanity Words ...."
## [1] "... Putting back apostrophe (') ..."
## [1] "... Saving mydata file:mydata_80.RData"
## [1] "-----> FINISH: create_mydata(80): Running Time .......413 seconds ..."
```

```
##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  3989025 213.1   12002346  641.0   9272007  495.2
## Vcells 56700430 432.6  158781237 1211.5 158781205 1211.5
```

An example of 'mydata' content is: 


```r
mydata[1:5] 
```

```
## [1] "listening to vh   presents eeee  donna summer live in concert eeee  rip"   
## [2] "one of the most interestingly informative posts yet eeee "                 
## [3] "plzzz followww me i love u so much   "                                     
## [4] "we have   new salads for spring eeee  garden ranch and carrot ginger eeee "
## [5] "beautiful sunday  beautiful brunch eeee  happy easter friends eeee "
```


### 2.2 Create All Tokens

One important step is create alltokens in order to be used to generate the ngrams (unigrams, bigrams, trigrams and quadgrams)


```r
create_alltokens(list_files,80)
```

```
## [1] "-----> INIT: create_alltokens(training_set:=80)......."
## [1] "-----> INIT: create_mydata(80)......."
## [1] "-----> FINISH: create_mydata(80): Running Time .......0 seconds ..."
## [1] "... Creating alltokens ..."
## Starting tokenization...
##   ...tokenizing texts...total elapsed:  54.96 seconds.
##   ...replacing names...total elapsed:  0.07 seconds.
## Finished tokenizing and cleaning 2,669,356 texts.
## [1] "... Saving alltokens file:alltokens_80.RData"
## [1] "-----> INIT: create_alltokens(80): Running Time .......100 seconds ..."
```

```
##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  4478643 239.2   14442815  771.4  14442815  771.4
## Vcells 75381939 575.2  218007256 1663.3 213006711 1625.2
```

An example of 'alltokens' content is:


```r
alltokens[1:5] 
```

```
## [[1]]
##  [1] "listening" "to"        "vh"        "presents"  "eeee"     
##  [6] "donna"     "summer"    "live"      "in"        "concert"  
## [11] "eeee"      "rip"      
## 
## [[2]]
## [1] "one"           "of"            "the"           "most"         
## [5] "interestingly" "informative"   "posts"         "yet"          
## [9] "eeee"         
## 
## [[3]]
## [1] "plzzz"    "followww" "me"       "i"        "love"     "u"       
## [7] "so"       "much"    
## 
## [[4]]
##  [1] "we"     "have"   "new"    "salads" "for"    "spring" "eeee"  
##  [8] "garden" "ranch"  "and"    "carrot" "ginger" "eeee"  
## 
## [[5]]
## [1] "beautiful" "sunday"    "beautiful" "brunch"    "eeee"      "happy"    
## [7] "easter"    "friends"   "eeee"
```

### 2.3 Create Unigrams Frequency Table 

To create the unigrams frequency table (dfm), we will use the alltokens to create the unigrams, clean it removing fake unigrams and finally create the dfm.

#### 2.3.1 Create and Clean Unigrams

Let's create and clean the unigrams:


```r
create_ngram(n=1,list_filenames,training_set= 80) 
```

```
## [1] "-----> INIT: create_ngram(n:=1 training_set:=80)......."
## [1] "-----> INIT: create_alltokens(training_set:=80)......."
## [1] "-----> INIT: create_alltokens(80): Running Time .......0 seconds ..."
## [1] "... Creating Ngram:uni.ngram"
## [1] "... Saving Ngram file:uni_ngram_80.RData"
## [1] "-----> FINISH: create_ngram(n:=1 training_set:=80): Running Time .......117 seconds ..."
```

```
##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  4477976 239.2   14442815  771.4  14442815  771.4
## Vcells 75382218 575.2  209595281 1599.1 295041012 2251.0
```



```r
clean_ngram(n=1,list_filenames,training_set= 80) 
```

```
## [1] "-----> INIT: clean_ngram(n:= 1 training_set:=80)......."
## [1] "-----> INIT: create_ngram(n:=1 training_set:=80)......."
## [1] "-----> FINISH: create_ngram(n:=1 training_set:=80): Running Time .......0 seconds ..."
## [1] "... Cleaning Ngram: uni.ngram"
## [1] "... Saving Ngram Cleaned file: uni_ngram_clean_80.RData"
## [1] "-----> INIT: clean_ngram(n:= 1 training_set:=80): Running Time .......781 seconds ..."
```

```
##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  4477453 239.2   14442815  771.4  14442815  771.4
## Vcells 68379486 521.7  209594796 1599.1 295041012 2251.0
```

An example of 'uni.ngram' content is:


```r
uni.ngram[1:5] 
```

```
## [[1]]
##  [1] "listening" "to"        "vh"        "presents"  "eeee"     
##  [6] "donna"     "summer"    "live"      "in"        "concert"  
## [11] "eeee"      "rip"      
## 
## [[2]]
## [1] "one"           "of"            "the"           "most"         
## [5] "interestingly" "informative"   "posts"         "yet"          
## [9] "eeee"         
## 
## [[3]]
## [1] "plzzz"    "followww" "me"       "i"        "love"     "u"       
## [7] "so"       "much"    
## 
## [[4]]
##  [1] "we"     "have"   "new"    "salads" "for"    "spring" "eeee"  
##  [8] "garden" "ranch"  "and"    "carrot" "ginger" "eeee"  
## 
## [[5]]
## [1] "beautiful" "sunday"    "beautiful" "brunch"    "eeee"      "happy"    
## [7] "easter"    "friends"   "eeee"
```


#### 2.3.2 Create and Trim Uni-dfm

Let's create and Trim the unigrams dfm:


```r
create_dfm(n=1,list_filenames,training_set= 80)
```

```
## [1] "-----> INIT: create_dfm(n:=1 training_set:=80)......."
## [1] "-----> INIT: clean_ngram(n:= 1 training_set:=80)......."
## [1] "-----> INIT: clean_ngram(n:= 1 training_set:=80): Running Time .......0 seconds ..."
## [1] "... Creating dfm:uni.dfm"
## 
##    ... indexing documents: 2,669,356 documents
##    ... indexing features: 408,925 feature types
##    ... created a 2669356 x 408926 sparse dfm
##    ... complete. 
## Elapsed time: 19.09 seconds.
## [1] "... Saving dfm file:uni_dfm_80.RData"
## [1] "-----> FINISH: create_dfm(n:=1 training_set:=80): Running Time .......43 seconds ..."
```

```
##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  4484302 239.5   14442815  771.4  14442815  771.4
## Vcells 84942988 648.1  424108907 3235.7 526561784 4017.4
```



```r
trim_dfm(n=1,list_filenames,training_set= 80,mincount=1)
```

```
## [1] "-----> INIT: trim_dfm(n:=1 training_set:=80 mincount:=1)......."
## [1] "-----> INIT: create_dfm(n:=1 training_set:=80)......."
## [1] "-----> FINISH: create_dfm(n:=1 training_set:=80): Running Time .......0 seconds ..."
## [1] "... Trim dfm:uni.dfm.trim"
## [1] "... Saving dfm clean: uni.dfm.clean .."
## [1] "-----> FINISH: trim_dfm(n:=1 training_set:=80 mincount:=1): Running Time .......23 seconds ..."
```

```
##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  4484314 239.5   14442815  771.4  14442815  771.4
## Vcells 84943119 648.1  271429700 2070.9 526561784 4017.4
```


#### 2.3.3 Create Unigram Data Table with Tokens and Frequency

Finally let's create the unigram data table with tokens and frequency:


```r
create_DT(n=1,list_filenames,training_set= 80,mincount=1)
```

```
## [1] "-----> INIT: create_DT(n:=1 training_set:=80 mincount:=1)......."
## [1] "-----> INIT: trim_dfm(n:=1 training_set:=80 mincount:=1)......."
## [1] "... Loading dfm trim file:uni_dfm_trim_80.RData"
## [1] "-----> FINISH: trim_dfm(n:=1 training_set:=80 mincount:=1): Running Time .......3.6 seconds ..."
## [1] "... Creating DT:DT.uni"
## [1] "... Saving DT.uni .."
## [1] "-----> FINISH: create_DT(n:=1 training_set:=80 mincount:=1): Running Time .......5.3 seconds ..."
```

```
##           used (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells 1832753 97.9    9243401  493.7  14442815  771.4
## Vcells 8368213 63.9  173715008 1325.4 526561784 4017.4
```

An example of unigrams frequency and tokens table content is:


```r
kable(DT.uni[order(-freq)][1:10])
```



t1        freq
----  --------
the    2360302
to     1543244
and    1280366
a      1268408
i      1211750
of     1037463
in      826934
you     683341
is      650369
for     621363



### 2.4 Create Bigrams Frequency Table 

To create the bigrams frequency table (dfm), we will use the alltokens to create the bigrams, clean it removing fake bigrams and finally create the dfm.

#### 2.4.1 Create and Clean Bigrams

Let's create and clean the bigrams:


```r
create_ngram(n=2,list_filenames,training_set= 80)
```

```
## [1] "-----> INIT: create_ngram(n:=2 training_set:=80)......."
## [1] "-----> INIT: create_alltokens(training_set:=80)......."
## [1] "... Loading file alltokens_80.RData  ...."
## [1] "-----> INIT: create_alltokens(80): Running Time .......21 seconds ..."
## [1] "... Creating Ngram:bi.ngram"
## [1] "... Saving Ngram file:bi_ngram_80.RData"
## [1] "-----> FINISH: create_ngram(n:=2 training_set:=80): Running Time .......393 seconds ..."
```

```
##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells 11747240 627.4   19381835 1035.2  19381835 1035.2
## Vcells 90318593 689.1  208538009 1591.1 526561784 4017.4
```



```r
clean_ngram(n=2,list_filenames,training_set= 80)
```

```
## [1] "-----> INIT: clean_ngram(n:= 2 training_set:=80)......."
## [1] "-----> INIT: create_ngram(n:=2 training_set:=80)......."
## [1] "-----> FINISH: create_ngram(n:=2 training_set:=80): Running Time .......0 seconds ..."
## [1] "... Cleaning Ngram: bi.ngram"
## [1] "... Saving Ngram Cleaned file: bi_ngram_clean_80.RData"
## [1] "-----> INIT: clean_ngram(n:= 2 training_set:=80): Running Time .......870 seconds ..."
```

```
##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells 11438747 610.9   23298202 1244.3  23298202 1244.3
## Vcells 77725068 593.0  220500314 1682.3 526561784 4017.4
```

An example of 'bi.ngram' content is:


```r
bi.ngram[1:5] 
```

```
## [[1]]
##  [1] "listening_to"  "to_vh"         "vh_presents"   "presents_eeee"
##  [5] "eeee_donna"    "donna_summer"  "summer_live"   "live_in"      
##  [9] "in_concert"    "concert_eeee"  "eeee_rip"     
## 
## [[2]]
## [1] "one_of"                    "of_the"                   
## [3] "the_most"                  "most_interestingly"       
## [5] "interestingly_informative" "informative_posts"        
## [7] "posts_yet"                 "yet_eeee"                 
## 
## [[3]]
## [1] "plzzz_followww" "followww_me"    "me_i"           "i_love"        
## [5] "love_u"         "u_so"           "so_much"       
## 
## [[4]]
##  [1] "we_have"       "have_new"      "new_salads"    "salads_for"   
##  [5] "for_spring"    "spring_eeee"   "eeee_garden"   "garden_ranch" 
##  [9] "ranch_and"     "and_carrot"    "carrot_ginger" "ginger_eeee"  
## 
## [[5]]
## [1] "beautiful_sunday" "sunday_beautiful" "beautiful_brunch"
## [4] "brunch_eeee"      "eeee_happy"       "happy_easter"    
## [7] "easter_friends"   "friends_eeee"
```


#### 2.4.2 Create and Trim Bi-dfm

Let's create and Trim the bigrams dfm:


```r
create_dfm(n=2,list_filenames,training_set= 80)
```

```
## [1] "-----> INIT: create_dfm(n:=2 training_set:=80)......."
## [1] "-----> INIT: clean_ngram(n:= 2 training_set:=80)......."
## [1] "-----> INIT: clean_ngram(n:= 2 training_set:=80): Running Time .......0 seconds ..."
## [1] "... Creating dfm:bi.dfm"
## 
##    ... indexing documents: 2,669,356 documents
##    ... indexing features: 6,947,567 feature types
##    ... created a 2669356 x 6947568 sparse dfm
##    ... complete. 
## Elapsed time: 48.19 seconds.
## [1] "... Saving dfm file:bi_dfm_80.RData"
## [1] "-----> FINISH: create_dfm(n:=2 training_set:=80): Running Time .......81 seconds ..."
```

```
##             used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  11449635 611.5   23298202 1244.3  23298202 1244.3
## Vcells 115966483 884.8  498714379 3804.9 526561784 4017.4
```



```r
trim_dfm(n=2,list_filenames,training_set= 80,mincount=1)
```

```
## [1] "-----> INIT: trim_dfm(n:=2 training_set:=80 mincount:=1)......."
## [1] "-----> INIT: create_dfm(n:=2 training_set:=80)......."
## [1] "-----> FINISH: create_dfm(n:=2 training_set:=80): Running Time .......0 seconds ..."
## [1] "... Trim dfm:bi.dfm.trim"
## [1] "... Saving dfm clean: bi.dfm.clean .."
## [1] "-----> FINISH: trim_dfm(n:=2 training_set:=80 mincount:=1): Running Time .......34 seconds ..."
```

```
##             used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  11449645 611.5   23298202 1244.3  23298202 1244.3
## Vcells 115966610 884.8  319177202 2435.2 526561784 4017.4
```


#### 2.4.3 Create Bigram Data Table with Tokens and Frequency

Finally let's create the bigram data table with tokens and frequency:


```r
create_DT(n=2,list_filenames,training_set= 80,mincount=1)
```

```
## [1] "-----> INIT: create_DT(n:=2 training_set:=80 mincount:=1)......."
## [1] "-----> INIT: trim_dfm(n:=2 training_set:=80 mincount:=1)......."
## [1] "... Loading dfm trim file:bi_dfm_trim_80.RData"
## [1] "-----> FINISH: trim_dfm(n:=2 training_set:=80 mincount:=1): Running Time .......8.6 seconds ..."
## [1] "... Creating DT:DT.bi"
## [1] "... Saving DT.bi .."
## [1] "-----> FINISH: create_DT(n:=2 training_set:=80 mincount:=1): Running Time .......35 seconds ..."
```

```
##            used  (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  1835594  98.1   18638561  995.5  23298202 1244.3
## Vcells 53436665 407.7  165369663 1261.7 526561784 4017.4
```

An example of bigrams frequency and tokens table content is:


```r
kable(DT.bi[order(-freq)][1:10])
```



t1    t2        freq
----  -----  -------
of    the     206830
in    the     196376
for   the     109707
to    the     108480
on    the     103227
to    be       95381
at    the      71282
i     have     64409
and   the      62197
i     was      61034



### 2.5 Create Trigrams Frequency Table 

To create the trigrams frequency table (dfm), we will use the alltokens to create the trigrams, clean it removing fake trigrams and finally create the dfm.

#### 2.5.1 Create and Clean Trigrams

Let's create and clean the trigrams:


```r
create_ngram(n=3,list_filenames,training_set= 80)
```

```
## [1] "-----> INIT: create_ngram(n:=3 training_set:=80)......."
## [1] "-----> INIT: create_alltokens(training_set:=80)......."
## [1] "... Loading file alltokens_80.RData  ...."
## [1] "-----> INIT: create_alltokens(80): Running Time .......21 seconds ..."
## [1] "... Creating Ngram:tri.ngram"
## [1] "... Saving Ngram file:tri_ngram_80.RData"
## [1] "-----> FINISH: create_ngram(n:=3 training_set:=80): Running Time .......528 seconds ..."
```

```
##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  28893214 1543.1   44920746 2399.1  44920746 2399.1
## Vcells 181876255 1387.7  412087965 3144.0 526561784 4017.4
```



```r
clean_ngram(n=3,list_filenames,training_set= 80)
```

```
## [1] "-----> INIT: clean_ngram(n:= 3 training_set:=80)......."
## [1] "-----> INIT: create_ngram(n:=3 training_set:=80)......."
## [1] "-----> FINISH: create_ngram(n:=3 training_set:=80): Running Time .......0 seconds ..."
## [1] "... Cleaning Ngram: tri.ngram"
## [1] "... Saving Ngram Cleaned file: tri_ngram_clean_80.RData"
## [1] "-----> INIT: clean_ngram(n:= 3 training_set:=80): Running Time .......924 seconds ..."
```

```
##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  23955286 1279.4   44920746 2399.1  44920746 2399.1
## Vcells 149555134 1141.1  493528279 3765.4 552953330 4218.7
```

An example of 'tri.ngram' content is:


```r
tri.ngram[1:5] 
```

```
## [[1]]
##  [1] "listening_to_vh"     "to_vh_presents"      "vh_presents_eeee"   
##  [4] "presents_eeee_donna" "eeee_donna_summer"   "donna_summer_live"  
##  [7] "summer_live_in"      "live_in_concert"     "in_concert_eeee"    
## [10] "concert_eeee_rip"   
## 
## [[2]]
## [1] "one_of_the"                      "of_the_most"                    
## [3] "the_most_interestingly"          "most_interestingly_informative" 
## [5] "interestingly_informative_posts" "informative_posts_yet"          
## [7] "posts_yet_eeee"                 
## 
## [[3]]
## [1] "plzzz_followww_me" "followww_me_i"     "me_i_love"        
## [4] "i_love_u"          "love_u_so"         "u_so_much"        
## 
## [[4]]
##  [1] "we_have_new"        "have_new_salads"    "new_salads_for"    
##  [4] "salads_for_spring"  "for_spring_eeee"    "spring_eeee_garden"
##  [7] "eeee_garden_ranch"  "garden_ranch_and"   "ranch_and_carrot"  
## [10] "and_carrot_ginger"  "carrot_ginger_eeee"
## 
## [[5]]
## [1] "beautiful_sunday_beautiful" "sunday_beautiful_brunch"   
## [3] "beautiful_brunch_eeee"      "brunch_eeee_happy"         
## [5] "eeee_happy_easter"          "happy_easter_friends"      
## [7] "easter_friends_eeee"
```


#### 2.5.2 Create and Trim Tri-dfm

Let's create and Trim the trigrams dfm:


```r
create_dfm(n=3,list_filenames,training_set= 80)
```

```
## [1] "-----> INIT: create_dfm(n:=3 training_set:=80)......."
## [1] "-----> INIT: clean_ngram(n:= 3 training_set:=80)......."
## [1] "-----> INIT: clean_ngram(n:= 3 training_set:=80): Running Time .......0 seconds ..."
## [1] "... Creating dfm:tri.dfm"
## 
##    ... indexing documents: 2,669,356 documents
##    ... indexing features: 19,499,404 feature types
##    ... created a 2669356 x 19499405 sparse dfm
##    ... complete. 
## Elapsed time: 50.29 seconds.
## [1] "... Saving dfm file:tri_dfm_80.RData"
## [1] "-----> FINISH: create_dfm(n:=3 training_set:=80): Running Time .......109 seconds ..."
```

```
##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  24004392 1282.0   44920746 2399.1  44920746 2399.1
## Vcells 200222836 1527.6  571254934 4358.4 585518015 4467.2
```



```r
trim_dfm(n=3,list_filenames,training_set= 80,mincount=1)
```

```
## [1] "-----> INIT: trim_dfm(n:=3 training_set:=80 mincount:=1)......."
## [1] "-----> INIT: create_dfm(n:=3 training_set:=80)......."
## [1] "-----> FINISH: create_dfm(n:=3 training_set:=80): Running Time .......0 seconds ..."
## [1] "... Trim dfm:tri.dfm.trim"
## [1] "... Saving dfm clean: tri.dfm.clean .."
## [1] "-----> FINISH: trim_dfm(n:=3 training_set:=80 mincount:=1): Running Time .......63 seconds ..."
```

```
##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  24004402 1282.0   44920746 2399.1  44920746 2399.1
## Vcells 200222962 1527.6  571254934 4358.4 585518015 4467.2
```

#### 2.5.3 Create Trigram Data Table with Tokens and Frequency

Finally let's create the trigrams data table with tokens and frequency:


```r
create_DT(n=3,list_filenames,training_set= 80,mincount=1)
```

```
## [1] "-----> INIT: create_DT(n:=3 training_set:=80 mincount:=1)......."
## [1] "-----> INIT: trim_dfm(n:=3 training_set:=80 mincount:=1)......."
## [1] "... Loading dfm trim file:tri_dfm_trim_80.RData"
## [1] "-----> FINISH: trim_dfm(n:=3 training_set:=80 mincount:=1): Running Time .......19 seconds ..."
## [1] "... Creating DT:DT.tri"
## [1] "... Saving DT.tri .."
## [1] "-----> FINISH: create_DT(n:=3 training_set:=80 mincount:=1): Running Time .......105 seconds ..."
```

```
##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells   1835654   98.1   35936596 1919.3  44920746 2399.1
## Vcells 196978578 1502.9  457003947 3486.7 585518015 4467.2
```

An example of trigrams frequency and tokens table content is:


```r
kable(DT.tri[order(-freq)][1:10])
```



t1        t2        t3      freq
--------  --------  ----  ------
thanks    for       the    19056
one       of        the    16899
a         lot       of     15576
i         want      to     10669
to        be        a      10502
going     to        be     10182
i         have      a       8812
looking   forward   to      8425
i         have      to      8279
it        was       a       8235





### 2.6 Create Quadgrams Frequency Table 

To create the quadgrams frequency table (dfm), we will use the alltokens to create the quadgrams, clean it removing fake quadgrams and finally create the dfm.

#### 2.6.1 Create and Clean Quadgrams

Let's create and clean the quadgrams:


```r
create_ngram(n=4,list_filenames,training_set= 80)
```

```
## [1] "-----> INIT: create_ngram(n:=4 training_set:=80)......."
## [1] "-----> INIT: create_alltokens(training_set:=80)......."
## [1] "... Loading file alltokens_80.RData  ...."
## [1] "-----> INIT: create_alltokens(80): Running Time .......22 seconds ..."
## [1] "... Creating Ngram:quad.ngram"
## [1] "... Saving Ngram file:quad_ngram_80.RData"
## [1] "-----> FINISH: create_ngram(n:=4 training_set:=80): Running Time .......604 seconds ..."
```

```
##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  44452070 2374.0   71835060 3836.5  59829217 3195.3
## Vcells 353594819 2697.8  658261683 5022.2 585518015 4467.2
```



```r
clean_ngram(n=4,list_filenames,training_set= 80)
```

```
## [1] "-----> INIT: clean_ngram(n:= 4 training_set:=80)......."
## [1] "-----> INIT: create_ngram(n:=4 training_set:=80)......."
## [1] "-----> FINISH: create_ngram(n:=4 training_set:=80): Running Time .......0 seconds ..."
## [1] "... Cleaning Ngram: quad.ngram"
## [1] "... Saving Ngram Cleaned file: quad_ngram_clean_80.RData"
## [1] "-----> INIT: clean_ngram(n:= 4 training_set:=80): Running Time .......1023 seconds ..."
```

```
##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  31290638 1671.2   71835060 3836.5  71835060 3836.5
## Vcells 281834779 2150.3  891387480 6800.8 782282630 5968.4
```

An example of 'quad.ngram' content is:


```r
quad.ngram[1:5] 
```

```
## [[1]]
## [1] "listening_to_vh_presents"   "to_vh_presents_eeee"       
## [3] "vh_presents_eeee_donna"     "presents_eeee_donna_summer"
## [5] "eeee_donna_summer_live"     "donna_summer_live_in"      
## [7] "summer_live_in_concert"     "live_in_concert_eeee"      
## [9] "in_concert_eeee_rip"       
## 
## [[2]]
## [1] "one_of_the_most"                     
## [2] "of_the_most_interestingly"           
## [3] "the_most_interestingly_informative"  
## [4] "most_interestingly_informative_posts"
## [5] "interestingly_informative_posts_yet" 
## [6] "informative_posts_yet_eeee"          
## 
## [[3]]
## [1] "plzzz_followww_me_i" "followww_me_i_love"  "me_i_love_u"        
## [4] "i_love_u_so"         "love_u_so_much"     
## 
## [[4]]
##  [1] "we_have_new_salads"       "have_new_salads_for"     
##  [3] "new_salads_for_spring"    "salads_for_spring_eeee"  
##  [5] "for_spring_eeee_garden"   "spring_eeee_garden_ranch"
##  [7] "eeee_garden_ranch_and"    "garden_ranch_and_carrot" 
##  [9] "ranch_and_carrot_ginger"  "and_carrot_ginger_eeee"  
## 
## [[5]]
## [1] "beautiful_sunday_beautiful_brunch" "sunday_beautiful_brunch_eeee"     
## [3] "beautiful_brunch_eeee_happy"       "brunch_eeee_happy_easter"         
## [5] "eeee_happy_easter_friends"         "happy_easter_friends_eeee"
```


#### 2.6.2 Create and Trim Quad-dfm

Let's create and Trim the quadgrams dfm:


```r
create_dfm(n=4,list_filenames,training_set= 80)
```

```
## [1] "-----> INIT: create_dfm(n:=4 training_set:=80)......."
## [1] "-----> INIT: clean_ngram(n:= 4 training_set:=80)......."
## [1] "-----> INIT: clean_ngram(n:= 4 training_set:=80): Running Time .......0 seconds ..."
## [1] "... Creating dfm:quad.dfm"
## 
##    ... indexing documents: 2,669,356 documents
##    ... indexing features: 26,919,002 feature types
##    ... created a 2669356 x 26919003 sparse dfm
##    ... complete. 
## Elapsed time: 27.78 seconds.
## [1] "... Saving dfm file:quad_dfm_80.RData"
## [1] "-----> FINISH: create_dfm(n:=4 training_set:=80): Running Time .......120 seconds ..."
```

```
##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  31424051 1678.3   71835060 3836.5  71835060 3836.5
## Vcells 341047952 2602.0  891387480 6800.8 868194854 6623.9
```



```r
trim_dfm(n=4,list_filenames,training_set= 80,mincount=1)
```

```
## [1] "-----> INIT: trim_dfm(n:=4 training_set:=80 mincount:=1)......."
## [1] "-----> INIT: create_dfm(n:=4 training_set:=80)......."
## [1] "-----> FINISH: create_dfm(n:=4 training_set:=80): Running Time .......0 seconds ..."
## [1] "... Trim dfm:quad.dfm.trim"
## [1] "... Saving dfm clean: quad.dfm.clean .."
## [1] "-----> FINISH: trim_dfm(n:=4 training_set:=80 mincount:=1): Running Time .......101 seconds ..."
```

```
##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  31424061 1678.3   71835060 3836.5  71835060 3836.5
## Vcells 341048082 2602.0  891387480 6800.8 868194854 6623.9
```

#### 2.6.2 Create Quadgram Data Table with Tokens and Frequency

Finally let's create the quadgrams data table with tokens and frequency:


```r
create_DT(n=4,list_filenames,training_set= 80,mincount=1)
```

```
## [1] "-----> INIT: create_DT(n:=4 training_set:=80 mincount:=1)......."
## [1] "-----> INIT: trim_dfm(n:=4 training_set:=80 mincount:=1)......."
## [1] "... Loading dfm trim file:quad_dfm_trim_80.RData"
## [1] "-----> FINISH: trim_dfm(n:=4 training_set:=80 mincount:=1): Running Time .......25 seconds ..."
## [1] "... Creating DT:DT.quad"
## [1] "... Saving DT.quad ..."
## [1] "-----> FINISH: create_DT(n:=4 training_set:=80 mincount:=1): Running Time .......247 seconds ..."
```

```
##             used   (Mb) gc trigger   (Mb)  max used   (Mb)
## Ncells  28754719 1535.7   71835060 3836.5  71835060 3836.5
## Vcells 536919939 4096.4  891387480 6800.8 886870239 6766.3
```

An example of unigrams frequency and tokens table content is:


```r
kable(DT.quad[order(-freq)][1:10])
```



t1       t2      t3      t4        freq
-------  ------  ------  -------  -----
thanks   for     the     follow    5090
the      end     of      the       4050
the      rest    of      the       3727
at       the     end     of        3427
for      the     first   time      3030
at       the     same    time      2863
is       going   to      be        2816
thanks   for     the     rt        2685
thank    you     for     the       2540
can't    wait    to      see       2366

### 3. Calculate Probability for Each Ngram

Let's calculate the Kneser-ney Probability for each ngram:

### 3.1 Create Unigram Knersey-ney Probability Table


```r
calculate_prob_kn(n=1,training_set=80,p1=1)
```

```
## [1] "-----> INIT: calculate_prob_kn(n:=1 training_set:=80 p1:=1)......."
## [1] "... Loading DT Prob Final file: DT_uni_prob_80.RData"
```

An example of unigrams probability table content is:


```r
kable(DT.uni.prob.final[order(-freq1)][1:10])
```



t1       freq1        prob
----  --------  ----------
the    2360302   0.0427344
to     1543244   0.0279412
and    1280366   0.0231816
a      1268408   0.0229651
i      1211750   0.0219393
of     1037463   0.0187838
in      826934   0.0149720
you     683341   0.0123722
is      650369   0.0117752
for     621363   0.0112501

### 3.2 Create Bigram Knersey-ney Probability Table


```r
calculate_prob_kn(n=2,training_set=80,p1=1)
```

```
## [1] "-----> INIT: calculate_prob_kn(n:=2 training_set:=80 p1:=1)......."
## [1] "-----> INIT: load_DT_prob_tables(n:=2 p1:=1 training_set:=80)......."
## [1] "-----> INIT: load_DT_prob_table(n:=1 p1:=1 training_set:=80)......."
## [1] "... Loading DT Prob Temp File: DT_uni_prob_temp_80.RData"
## [1] "-----> FINISH: load_DT_prob_table(n:=1 p1:=1 training_set:=80): Running Time .......0.31 seconds ..."
## [1] "-----> INIT: load_DT_prob_table(n:=2 p1:=1 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_table(n:=2 p1:=1 training_set:=80): Running Time .......0.34 seconds ..."
## [1] "-----> FINISH: load_DT_prob_tables(n:=2 p1:=1 training_set:=80): Running Time .......0.65 seconds ..."
## [1] "... Calculating DT Prob Table:DT.bi.prob"
## [1] "---> Calculating and Adding neccesary values for Bigrams High Order Prob Calculation: Pkn(t1 t2) ..."
## [1] "... Calculating: sum(w) c(t1 w) = sum.freq2(t1) ..."
## [1] "... Calculating: N1+(t1 *) = n12(t1) ..."
## [1] "... Adding to Bigrams Table: pkn12(t2) ..."
## [1] "--> Calculating Kneser-ney Prob for High Order Bigrams ..."
## [1] "    Pkn(t1 t2) = max{ c(t1 t2) - D2, 0 } / (sum(w) c(t1 w)) + "
## [1] "                    D2 / (sum(w) c(t1 w)) * N1+(t1 *) x Pknr (t2) ..."
## [1] "... Saving DT probability Temp file:DT_bi_prob_temp_80.RData"
## [1] "... Saving DT probability final file:DT_bi_prob_final_80.RData"
## [1] "-----> FINISH: calculate_prob_kn(n:=2 training_set:=80 p1:=1): Running Time .......47 seconds ..."
```

An example of bigrams probability table content is:



```r
kable(DT.bi.prob.final[order(-freq2)][1:10])
```



t1    t2       freq2        prob
----  -----  -------  ----------
of    the     206830   0.2022995
in    the     196376   0.2468229
for   the     109707   0.1809920
to    the     108480   0.0714742
on    the     103227   0.2364533
to    be       95381   0.0627854
at    the      71282   0.2570547
i     have     64409   0.0535525
and   the      62197   0.0493147
i     was      61034   0.0507584

### 3.3 Create Trigram Knersey-ney Probability Table


```r
calculate_prob_kn(n=3,training_set=80,p1=1)
```

```
## [1] "-----> INIT: calculate_prob_kn(n:=3 training_set:=80 p1:=1)......."
## [1] "-----> INIT: load_DT_prob_tables(n:=3 p1:=1 training_set:=80)......."
## [1] "-----> INIT: load_DT_prob_table(n:=1 p1:=1 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_table(n:=1 p1:=1 training_set:=80): Running Time .......0.02 seconds ..."
## [1] "-----> INIT: load_DT_prob_table(n:=2 p1:=1 training_set:=80)......."
## [1] "... Loading DT Prob Temp File: DT_bi_prob_temp_80.RData"
## [1] "-----> FINISH: load_DT_prob_table(n:=2 p1:=1 training_set:=80): Running Time .......8.1 seconds ..."
## [1] "-----> INIT: load_DT_prob_table(n:=3 p1:=1 training_set:=80)......."
## [1] "-----> INIT: load_DT_table(n:=3 training_set:=80)......."
## [1] "-----> FINISH: load_DT_table(n:=3 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> FINISH: load_DT_prob_table(n:=3 p1:=1 training_set:=80): Running Time .......3 seconds ..."
## [1] "-----> FINISH: load_DT_prob_tables(n:=3 p1:=1 training_set:=80): Running Time .......11 seconds ..."
## [1] "... Calculating DT Prob Table:DT.tri.prob"
## [1] "---> Calculating and Adding neccesary values for Trigrams High Order Prob Calculation: Pkn(t1 t2 t3) ..."
## [1] "... Calculating: sum(w) c(t1 t2 w) = sum.freq3(t1 t2) ..."
## [1] "... Calculating: N1+(t1 t2 *) = n22(t1 t2) ..."
## [1] "... Calculating and Adding neccesary values for Bigrams Low Order Prob Calculation: Pknr(t2 t3)..."
## [1] "...... Calculating: N1+(* t2 t3) = n21(t2 t3) ..."
## [1] "...... Calculating: sum(w) N1+(* t2 w) = sum.n21(t2) ..."
## [1] "...... Adding to Trigrams Table: n12(t2) ..."
## [1] "...... Adding to Trigrams Table: pkn12(t3) ..."
## [1] "...... Calculating Kneser-ney Prob for Low Order Bigrams = pkn22(t2 t3)..."
## [1] "...... Pknr(t2 t3) = max{ N1+(t2 t3) - D2, 0 } / (sum(w) N1+(* t2 w)) + "
## [1] "                     D2 / (sum(w) N1+(* t2 w)) * N1+(t2 *) x Pknr(t3) ..."
## [1] "--> Calculating Kneser-ney Prob for High Order Trigrams = pkn31(t1 t2 t3)..."
## [1] "    Pkn(t1 t2 t3) = max{ c(t1 t2 t3) - D3, 0 } / (sum(w) c(t1 t2 w)) + "
## [1] "                    D3 / (sum(w) c(t1 t2 w)) * N1+(t1 t2 *) x Pknr (t2 t3)"
## [1] "... Saving DT probability Temp file:DT_tri_prob_temp_80.RData"
## [1] "... Saving DT probability final file:DT_tri_prob_final_80.RData"
## [1] "-----> FINISH: calculate_prob_kn(n:=3 training_set:=80 p1:=1): Running Time .......281 seconds ..."
```

An example of trigrams probability table content is:



```r
kable(DT.tri.prob.final[order(-freq3)][1:10])
```



t1        t2        t3     freq3        prob
--------  --------  ----  ------  ----------
thanks    for       the    19056   0.5311848
one       of        the    16899   0.4328772
a         lot       of     15576   0.6659718
i         want      to     10669   0.5680719
to        be        a      10502   0.1148977
going     to        be     10182   0.2120998
i         have      a       8812   0.1390641
looking   forward   to      8425   0.9848397
i         have      to      8279   0.1306504
it        was       a       8235   0.1507128

### 3.4 Create Quadgram Knersey-ney Probability Table




```r
calculate_prob_kn(n=4,training_set=80,p1=1)
```

```
## [1] "-----> INIT: calculate_prob_kn(n:=4 training_set:=80 p1:=1)......."
## [1] "-----> INIT: load_DT_prob_tables(n:=4 p1:=1 training_set:=80)......."
## [1] "-----> INIT: load_DT_prob_table(n:=3 p1:=1 training_set:=80)......."
## [1] "... Loading DT Prob Temp File: DT_tri_prob_temp_80.RData"
## [1] "-----> FINISH: load_DT_prob_table(n:=3 p1:=1 training_set:=80): Running Time .......34 seconds ..."
## [1] "-----> INIT: load_DT_prob_table(n:=4 p1:=1 training_set:=80)......."
## [1] "-----> INIT: load_DT_table(n:=4 training_set:=80)......."
## [1] "-----> FINISH: load_DT_table(n:=4 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> FINISH: load_DT_prob_table(n:=4 p1:=1 training_set:=80): Running Time .......11 seconds ..."
## [1] "-----> FINISH: load_DT_prob_tables(n:=4 p1:=1 training_set:=80): Running Time .......45 seconds ..."
## [1] "... Calculating DT Prob Table:DT.quad.prob.final"
## [1] "---> Calculating and Adding neccesary values for Quadgrams High Order Prob Calculation: Pkn(t1 t2 t3 t4) ..."
## [1] "... Calculating: sum(w) c(t1 t2 t3 w) = sum.freq4(t1 t2 t3) ..."
## [1] "... Calculating: N1+(t1 t2 t3 *) = n32(t1 t2 t3) ..."
## [1] "... Calculating and Adding neccesary values for Trigrams Low Order Prob Calculation: Pknr(t2 t3 t4)..."
## [1] "...... Calculating: N1+(* t2 t3 t4) = n31(t2 t3 t4) ..."
## [1] "...... Calculating: sum(w) N1+(* t2 t3 w) = sum.n31(t2 t3) ..."
## [1] "...... Adding to Quadgrams Table: n22(t2 t3) ..."
## [1] "...... Adding to Quadgrams Table: pkn22(t3 t4) ..."
## [1] "...... Calculating Kneser-ney Prob for Low Order Trigrams = pkn32(t2 t3 t4)..."
## [1] "...... Pknr(t2 t3 t4) = max{ N1+(t2 t3 t4) - D3, 0 } / (sum(w) N1+(* t2 t3 w)) + "
## [1] "                        D3 / (sum(w) N1+(* t2 t3 w)) * N1+(t2 t3 *) x Pknr(t3 t4) ..."
## [1] "--> Calculating Kneser-ney Prob for High Order Quadgrams = pkn41(t1 t2 t3 t4)..."
## [1] "    Pkn(t1 t2 t3 t4) = max{ c(t1 t2 t3 t4) - D4, 0 } / (sum(w) c(t1 t2 t3 w) + "
## [1] "                       D4 / (sum(w) c(t1 t2 t3 w)) * N1+(t1 t2 t3 *) x Pknr (t2 t3 t4)"
## [1] "... Saving DT probability Temp file:DT_quad_prob_temp_80.RData"
## [1] "... Saving DT probability final file:DT_quad_prob_final_80.RData"
## [1] "-----> FINISH: calculate_prob_kn(n:=4 training_set:=80 p1:=1): Running Time .......455 seconds ..."
```

An example of quadgrams probability table content is:



```r
kable(DT.quad.prob.final[order(-freq4)][1:10])
```



t1       t2      t3      t4        freq4        prob
-------  ------  ------  -------  ------  ----------
thanks   for     the     follow     5090   0.2758026
the      end     of      the        4050   0.5268055
the      rest    of      the        3727   0.5764063
at       the     end     of         3427   0.8932204
for      the     first   time       3030   0.7775625
at       the     same    time       2863   0.8641693
is       going   to      be         2816   0.4511452
thanks   for     the     rt         2685   0.1454642
thank    you     for     the        2540   0.3158351
can't    wait    to      see        2366   0.3605652

## 4. Ngrams Probability Frequency Table Analysis

Let's show the information regarding the frequency of the ngrams in the data tables:


```r
DT.prob.freq <- DT_prob_freq(training_set=80) 
```

```
## [1] "-----> INIT: DT_prob_freq(training_set:=80)......."
## [1] "-----> INIT: load_DT_prob_final_table(n:=1 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=1 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=2 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=2 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=3 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=3 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=4 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=4 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> FINISH: DT_prob_freq(training_set:=80): Running Time .......46 seconds ..."
```



```r
kable(DT.prob.freq)
```



Ngram   Freq                  Amount     Percent   
------  --------------------  ---------  ----------
1       Total Ngrams          408926     100.00000 
1       Min Freq == 1         220031     53.80705  
1       Freq == 2             50423      12.33059  
1       Freq == 3             23894      5.84311   
1       Freq == 4             14795      3.61801   
1       Freq == 5             10085      2.46622   
1       Max Freq == 2360302   1          0.00024   
1       Freq <= 2             270454     66.13764  
1       Freq <= 3             294348     71.98075  
1       Freq <= 4             309143     75.59876  
1       Freq <= 5             319228     78.06498  
1       Freq >= 6             89698      21.93502  
2       Total Ngrams          6947568    100.00000 
2       Min Freq == 1         4823856    69.43230  
2       Freq == 2             821013     11.81727  
2       Freq == 3             342442     4.92895   
2       Freq == 4             191209     2.75217   
2       Freq == 5             124632     1.79389   
2       Max Freq == 206830    1          1e-05     
2       Freq <= 2             5644869    81.24957  
2       Freq <= 3             5987311    86.17852  
2       Freq <= 4             6178520    88.93069  
2       Freq <= 5             6303152    90.72458  
2       Freq >= 6             644416     9.27542   
3       Total Ngrams          19499405   100.00000 
3       Min Freq == 1         16151800   82.83227  
3       Freq == 2             1610852    8.26103   
3       Freq == 3             572366     2.93530   
3       Freq == 4             292746     1.50131   
3       Freq == 5             177797     0.91181   
3       Max Freq == 19056     1          1e-05     
3       Freq <= 2             17762652   91.09330  
3       Freq <= 3             18335018   94.02860  
3       Freq <= 4             18627764   95.52991  
3       Freq <= 5             18805561   96.44172  
3       Freq >= 6             693844     3.55828   
4       Total Ngrams          26919003   100.00000 
4       Min Freq == 1         24657722   91.59969  
4       Freq == 2             1288670    4.78721   
4       Freq == 3             383326     1.42400   
4       Freq == 4             178843     0.66437   
4       Freq == 5             101637     0.37757   
4       Max Freq == 5090      1          0.00000   
4       Freq <= 2             25946392   96.38690  
4       Freq <= 3             26329718   97.81090  
4       Freq <= 4             26508561   98.47527  
4       Freq <= 5             26610198   98.85284  
4       Freq >= 6             308805     1.14716   

## 5. Singleton Probability Table

We will create two groups of probability tables to be used with the Shiny app removing:

* Ngrams with frequency == 1
* Ngrmas with frequency < 5 


```r
DT.prob.sing <- DT_prob_singletons(training_set=80) 
```

```
## [1] "-----> INIT: DT_prob_singletons(training_set:=80)......."
## [1] "-----> INIT: load_DT_prob_final_table(n:=1 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=1 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=2 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=2 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=3 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=3 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=4 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=4 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> FINISH: DT_prob_singletons(training_set:=80): Running Time .......45 seconds ..."
```



```r
kable(DT.prob.sing)
```



Ngram   Object Size All Freq (Mbytes)   Object Size Freq > 1 (Mbytes)   Percent  Freq > 1   Object Size Freq >= 5 (Mbytes)   Percent Freq >= 5 
------  ------------------------------  ------------------------------  ------------------  -------------------------------  ------------------
1       31.33272                        13.62089                        43.47177            7.18066                          22.91744          
2       272.76759                       73.72966                        27.03021            27.44813                         10.06283          
3       863.69994                       135.00061                       15.63050            35.86026                         4.15194           
4       1389.64787                      109.46494                       7.87717             20.42956                         1.47012           

## 6. Prediction of Next Word

Let's see some example of prediction:

### 6.1 Example with Unigram


```r
prediction1 <- predict_nextword(c("how"),p=0,n=5,training_set = 80) 
```

```
## [1] "-----> INIT: predict_nextword( word:=(how), training_set:=80, prob:=0, n:=5)......."
## [1] "-----> INIT: load_DT_prob_final_table(n:=1 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=1 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=2 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=2 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=3 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=3 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=4 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=4 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: topn_predict( word:=(how), prob:=0, nun_words:=5, factor:=1)......."
## [1] "... Found: 5728  words ..."
##    word       prob
## 1:   to 0.11710652
## 2: much 0.06558854
## 3: many 0.04785503
## 4:    i 0.04610697
## 5:   do 0.04034563
## [1] "-----> topn_predict: Running Time .......0.48 seconds ..."
## [1] "-----> FINISH: predict_nextword: Running Time .......0.48 seconds ..."
```


```r
kable(prediction1)
```



word         prob
-----  ----------
to      0.1171065
much    0.0655885
many    0.0478550
i       0.0461070
do      0.0403456

### 6.2 Example with Bigram


```r
prediction2 <- predict_nextword(c("how","are"),p=0,n=5,training_set = 80) 
```

```
## [1] "-----> INIT: predict_nextword( word:=(how,are), training_set:=80, prob:=0, n:=5)......."
## [1] "-----> INIT: load_DT_prob_final_table(n:=1 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=1 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=2 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=2 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=3 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=3 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=4 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=4 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: topn_predict( word:=(how,are), prob:=0, nun_words:=5, factor:=1)......."
## [1] "...Found: 121  words ..."
##      word       prob
## 1:    you 0.73549958
## 2:      u 0.05091751
## 3: things 0.04909117
## 4:     we 0.02743726
## 5:    the 0.02378666
## [1] "-----> topn_predict: Running Time .......4.8 seconds ..."
## [1] "-----> FINISH: predict_nextword: Running Time .......4.8 seconds ..."
```


```r
kable(prediction2)
```



word           prob
-------  ----------
you       0.7354996
u         0.0509175
things    0.0490912
we        0.0274373
the       0.0237867

### 6.3 Example with Trigram


```r
prediction3 <- predict_nextword(c("how","are","you"),p=0,n=5,training_set = 80) 
```

```
## [1] "-----> INIT: predict_nextword( word:=(how,are,you), training_set:=80, prob:=0, n:=5)......."
## [1] "-----> INIT: load_DT_prob_final_table(n:=1 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=1 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=2 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=2 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=3 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=3 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=4 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=4 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: topn_predict( word:=(how,are,you), prob:=0, nun_words:=5, factor:=1)......."
## [1] "...Found: 278  words..."
##           word       prob
## 1:       doing 0.22016549
## 2:       today 0.08081740
## 3:     feeling 0.05787095
## 4:       going 0.04778157
## 5: celebrating 0.03711988
## [1] "-----> topn_predict: Running Time .......4.7 seconds ..."
## [1] "-----> FINISH: predict_nextword: Running Time .......4.7 seconds ..."
```


```r
kable(prediction3)
```



word                prob
------------  ----------
doing          0.2201655
today          0.0808174
feeling        0.0578709
going          0.0477816
celebrating    0.0371199


## 7. Prediction of Next Word using Regex

Let's see some examples of prediction using regex:


```r
prediction1 <- predict_nextword_regex(c("how","are",""),p=0,n=5,training_set = 80)
```

```
## [1] "-----> INIT: predict_nextword_regex( word:=(how,are,), training_set:=80, prob:=0, n:=5)......."
## [1] "-----> INIT: load_DT_prob_final_table(n:=1 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=1 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=2 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=2 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=3 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=3 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=4 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=4 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: topn_predict_regex( word:=(how,are,), prob:=0, nun_words:=5, factor:=1)......."
## [1] "...Found: 121  words ..."
##      word       prob
## 1:    you 0.73549958
## 2:      u 0.05091751
## 3: things 0.04909117
## 4:     we 0.02743726
## 5:    the 0.02378666
## [1] "-----> FINISH: topn_predict_regex: Running Time .......2.3 seconds ..."
## [1] "-----> FINISH: predict_nextword_regex: Running Time .......2.3 seconds ..."
```

```r
prediction2 <- predict_nextword(c("how","are"),p=0,n=5,training_set = 80)
```

```
## [1] "-----> INIT: predict_nextword( word:=(how,are), training_set:=80, prob:=0, n:=5)......."
## [1] "-----> INIT: load_DT_prob_final_table(n:=1 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=1 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=2 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=2 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=3 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=3 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=4 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=4 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: topn_predict( word:=(how,are), prob:=0, nun_words:=5, factor:=1)......."
## [1] "...Found: 121  words ..."
##      word       prob
## 1:    you 0.73549958
## 2:      u 0.05091751
## 3: things 0.04909117
## 4:     we 0.02743726
## 5:    the 0.02378666
## [1] "-----> topn_predict: Running Time .......2.3 seconds ..."
## [1] "-----> FINISH: predict_nextword: Running Time .......2.3 seconds ..."
```

```r
prediction3 <- predict_nextword_regex(c("how","are","y"),p=0,n=5,training_set = 80)
```

```
## [1] "-----> INIT: predict_nextword_regex( word:=(how,are,y), training_set:=80, prob:=0, n:=5)......."
## [1] "-----> INIT: load_DT_prob_final_table(n:=1 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=1 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=2 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=2 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=3 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=3 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: load_DT_prob_final_table(n:=4 training_set:=80)......."
## [1] "-----> FINISH: load_DT_prob_final_table(n:=4 training_set:=80): Running Time .......0 seconds ..."
## [1] "-----> INIT: topn_predict_regex( word:=(how,are,y), prob:=0, nun_words:=5, factor:=1)......."
## [1] "...Found: 19  words ..."
##     word        prob
## 1:   you 0.735499580
## 2:    ya 0.023784606
## 3:  your 0.015175273
## 4:  youu 0.002391419
## 5: y'all 0.001086965
## [1] "-----> FINISH: topn_predict_regex: Running Time .......2.3 seconds ..."
## [1] "-----> FINISH: predict_nextword_regex: Running Time .......2.3 seconds ..."
```



```r
# Print the basic information about the files. 
kable(prediction1)
```



word           prob
-------  ----------
you       0.7354996
u         0.0509175
things    0.0490912
we        0.0274373
the       0.0237867

```r
kable(prediction2)
```



word           prob
-------  ----------
you       0.7354996
u         0.0509175
things    0.0490912
we        0.0274373
the       0.0237867

```r
kable(prediction3)
```



word          prob
------  ----------
you      0.7354996
ya       0.0237846
your     0.0151753
youu     0.0023914
y'all    0.0010870
