# Capstone Project - Milestone Exploratory Data Analysis Report
Pranay Sarkar  
August 16, 2018  




## Executive Summary

This is a Milestone report related with the Coursera Capstone Project, the target is show initial exploratory data analysis about the US dataset that include three kind of files:

* Twitter
* News
* Blogs


## 1. Load the Neccesary Libraries

For this project we will use basicly the **quanteda**,**ggplot2**, **knitr** and **RColorBrewer**.


```r
library(quanteda)
library(ggplot2)
library(knitr)
library(RColorBrewer)

set.seed(12345) # For reproducibility
```

## 2. Exploratory Data Analysis
### 2.1 Basic Files Information

The files that we will use in the project are bigger than **150Mbytes** each one. In order to do the exploratory data analysis and to have an acceptable runtime, I will use only **10%** of the data. 



Those are the main characteristics of the files:


```r
# Print the basic information about the files. 
kable(dt)
```



Filename              Filesize   Total Lines   Subset Lines
------------------  ----------  ------------  -------------
en_US.twitter.txt    167105338       2360148          23601
en_US.news.txt       205811889         77259            772
en_US.blogs.txt      210160014        899288           8992

According with this, we will use only *23601*, *772* and *8992* lines of **twitter**, **news** and **blogs** datasets. 

### 2.2 File Content   

Let's see some some examples of the content for each of the files:


```r
twitter.data[1:3]
```

```
## [1] "Listening to the #socialmedia experts from at 's Cyberposium now! They are awesome!"
## [2] "god I miss those"                                                                   
## [3] "kay! Thanks(:"
```

```r
news.data[1:3]
```

```
## [1] "But here in Minnesota, Dayton and other Democrats can't seriously mean to argue both that the policies of George Bush, who left office three years ago, are wholly responsible for today's continuing national recession, and that the policies of Tim Pawlenty, who left office 11 months ago, have had nothing whatever to do with Minnesota's above-average recovery over the past four years."                                                                                                                                           
## [2] "Jets: D+"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
## [3] "President Obama has been criticized -- including in a Times editorial -- for his ahistorical statement that it would be \"an unprecedented, extraordinary step\" if the court overturned \"a law that was passed by a strong majority of a democratically elected Congress.\" The law in question, of course, was \"Obamacare,\" and the president seemed to be trying to muscle the court into upholding it by lamenting \"that an unelected group of people would somehow overturn a duly constituted and passed law.\" That was unseemly."
```

```r
blogs.data[1:3]
```

```
## [1] "It contains the full version of my complaint submitted to the BBC at:"                                                                                                                                                                                                                                                                                           
## [2] "Pasir Ris Park Town Beach and Pasir Ris Park Beach are connected by a bridge. From here, you can connect to the Bedok Park Connector Network, Tampines Park Connector Network or even go to Changi Beach and East Coast if you have the energy. At the end of this article, there is a brochure, from the National Parks, on the Eastern Coastal Park Connector."
## [3] "Cheese: Camembert Tremblaye: shhhh, a thermalized Camembert from France."
```


### 2.3 Character Basis Analysis 



Let's see some information regarding the amount of characters per line for each dataset:


```r
kable(dt)
```



Filename              Filesize   Total Lines   Subset Lines   Max Chars per Line   Avg Chars per Line   Min Chars per Line
------------------  ----------  ------------  -------------  -------------------  -------------------  -------------------
en_US.twitter.txt    167105338       2360148          23601                  140                   64                    4
en_US.news.txt       205811889         77259            772                  782                  188                    7
en_US.blogs.txt      210160014        899288           8992                 4596                  159                    1

We can see that the longest line belongs to **blogs** dataset (*4596*), but the longest in average is the **news** dataset (*188*), and the maximun for **twitter** is already *140* (the maximun amount defined in the social media platform, until now). 

Let's make some plots in order to see that:


```r
# Print Histogram of Number of Characters per Line
ggplot(data=lines_char.all, aes(x=num_char, fill=type)) +
  geom_histogram() +
  facet_wrap(~ type, ncol = 1, scales="free") +
  labs(title="Histogram for Number of Characters per Line") +
  labs(x="Number of Characters",y="Number of Lines") 
```

![](Middle_Report_files/figure-html/unnamed-chunk-7-1.png) 

```r
ggplot(data=lines_char.all, aes(x=num_char, fill=type, colour=type)) +
  geom_freqpoly() +
  labs(title="Histogram for Number of Characters per Line") +
  labs(x="Number of Characters",y="Number of Lines") 
```

![](Middle_Report_files/figure-html/unnamed-chunk-7-2.png) 



### 2.4 Basic Words Analysis 

Let's see some characteristics about the amount of words per line. For this step, we consider a "word" any group of characters separated by " ". We will use the following function to count the number of words per line:



```r
f_num_words <- function(x) length(unlist(strsplit(x,split=" ")))
```

Let's see the minimun, average and maximun amount of words per line for each type of files:





```r
kable(dt)
```



Filename              Filesize   Total Lines   Subset Lines   Max Chars per Line   Avg Chars per Line   Min Chars per Line   Max Words per Line   Avg Words per Line   Min Words per Line
------------------  ----------  ------------  -------------  -------------------  -------------------  -------------------  -------------------  -------------------  -------------------
en_US.twitter.txt    167105338       2360148          23601                  140                   64                    4                   39                   12                    1
en_US.news.txt       205811889         77259            772                  782                  188                    7                  135                   32                    1
en_US.blogs.txt      210160014        899288           8992                 4596                  159                    1                  715                   29                    1

The results are similar to the previous character analysis, in terms of *average words per line*, **blogs** and **news** are very similar (*29* and *32*), and the *maximun words per line* for **blogs** dataset is very big compare with the others (*715* compare to *135* and *39*).

Let's see that information with some plots:


```r
ggplot(data=lines_word.all,aes(x=num_words, fill=type)) +
  geom_histogram() +
  facet_wrap(~ type, ncol = 1, scales="free") +
  labs(title="Histogram for Number of Words per Line") +
  labs(x="Number of Words per Line",y="Frequency") 
```

![](Middle_Report_files/figure-html/unnamed-chunk-11-1.png) 

```r
ggplot(data=lines_word.all,aes(x=num_words,fill=type,colour=type)) +
  geom_freqpoly() +
  labs(title="Histogram for Number of Words per Line") +
  labs(x="Number of Words per Line",y="Frequency") 
```

![](Middle_Report_files/figure-html/unnamed-chunk-11-2.png) 

### 2.5 Corpora

Let's built the corpora for each of the files to be used in future analysis, using the *corpus()* function of **quanteda** library:


```r
twitter.docvars <- data.frame(Source = rep("twitter",lines.twitter.data))
blogs.docvars <- data.frame(Source = rep("blogs",lines.blogs.data))
news.docvars <- data.frame(Source = rep("news",lines.news.data))

twitter.corpus <- corpus(twitter.data, docvars = twitter.docvars)
news.corpus <- corpus(news.data, docvars = news.docvars)
blogs.corpus <- corpus(blogs.data, docvars = blogs.docvars)

## Let's see information about the corpus
summary(twitter.corpus,1)
```

```
## Corpus consisting of 23601 documents, showing 1 document.
## 
##   Text Types Tokens Sentences  Source
##  text1    15     16         2 twitter
## 
## Source:  D:/001 -- Coursera/Capstone Project/Coursera---Data-Science---Capstone-Project/* on x86-64 by enrique
## Created: Sun Jul 17 02:08:42 2016
## Notes:
```

```r
summary(news.corpus,1)
```

```
## Corpus consisting of 772 documents, showing 1 document.
## 
##   Text Types Tokens Sentences Source
##  text1    54     71         1   news
## 
## Source:  D:/001 -- Coursera/Capstone Project/Coursera---Data-Science---Capstone-Project/* on x86-64 by enrique
## Created: Sun Jul 17 02:08:42 2016
## Notes:
```

```r
summary(blogs.corpus,1)
```

```
## Corpus consisting of 8992 documents, showing 1 document.
## 
##   Text Types Tokens Sentences Source
##  text1    13     14         1  blogs
## 
## Source:  D:/001 -- Coursera/Capstone Project/Coursera---Data-Science---Capstone-Project/* on x86-64 by enrique
## Created: Sun Jul 17 02:08:42 2016
## Notes:
```

We can see that the **twitter**, **news** and **blogs** *Corpus* have *23601**, **772* and *8992* documents (equal to number of lines of each dataset).

With **quanteda** packages is very simple to create a new Corpus combining the previous ones:


```r
all.corpus <- (twitter.corpus + news.corpus) + blogs.corpus
summary(all.corpus,1)
```

```
## Corpus consisting of 33365 documents, showing 1 document.
## 
##   Text Types Tokens Sentences  Source
##  text1    15     16         2 twitter
## 
## Source:  Combination of corpuses (twitter.corpus + news.corpus) and blogs.corpus
## Created: Sun Jul 17 02:08:42 2016
## Notes:
```

We can see that this new Corpus have have *33365* documents (equal to add the number of documents of each corpus).

### 2.6 Word Analysis with Document Feature Matrix

Let's built the document-feature matrix using the *dfm()* function to analyze the features and frequencies. We will also clean the data by doing the following: 

* Lower all the characters (*toLower = TRUE*)
* Remove numbers (*removeNumbers = TRUE*)
* Remove punctuation symbols (*removePunct = TRUE*)
* Remove separators (*removeSeparators = TRUE*)
* Remove twitter characters (*removeTwitter = TRUE*)
* Remove bad/profanity english words (*stopwords("english")*)



```r
all.dfm <- dfm(all.corpus, 
             toLower = TRUE,
             removeNumbers = TRUE, 
             removePunct = TRUE, 
             removeSeparators = TRUE,
             removeTwitter = TRUE, 
             stem = FALSE, 
             language = "english",
             ignoredFeatures = stopwords("english"))
```

```
## Creating a dfm from a corpus ...
##    ... lowercasing
##    ... tokenizing
##    ... indexing documents: 33,365 documents
##    ... indexing features: 44,290 feature types
##    ... removed 173 features, from 174 supplied (glob) feature types
##    ... created a 33365 x 44118 sparse dfm
##    ... complete. 
## Elapsed time: 2.04 seconds.
```

```r
## Total number of features (words)
print(num.words.all <- nfeature(all.dfm))
```

```
## [1] 44118
```

```r
## Information of dfm
head(all.dfm,5)
```

```
## Document-feature matrix of: 33,365 documents, 44,118 features.
## (showing first 5 documents and first 6 features)
##        features
## docs    listening socialmedia experts s cyberposium now
##   text1         1           1       1 1           1   1
##   text2         0           0       0 0           0   0
##   text3         0           0       0 0           0   0
##   text4         0           0       0 0           0   0
##   text5         0           0       0 0           0   0
```

We can see that the total number of features is *44119*, that's is the number of words that are included in the Corpora.

### 2.6.1 Top 20 Words

We can use the data feature matrix created previously to analyze the *frequency* and *accumulated frequency* of words in the Copora.



Let's see the top-20 words and the frequency related information:


```r
head(all.words,20)
```

```
##        freq acumfreq   perfreq acumperfreq   word numword
## just   2636     2636 0.7021862   0.7021862   just       1
## like   2327     4963 0.6198738   1.3220600   like       2
## will   2179     7142 0.5804491   1.9025091   will       3
## one    2168     9310 0.5775189   2.4800279    one       4
## can    1944    11254 0.5178490   2.9978769    can       5
## get    1854    13108 0.4938745   3.4917514    get       6
## time   1758    14866 0.4683017   3.9600532   time       7
## good   1548    16414 0.4123612   4.3724144   good       8
## love   1479    17893 0.3939808   4.7663952   love       9
## day    1477    19370 0.3934480   5.1598433    day      10
## now    1469    20839 0.3913170   5.5511602    now      11
## know   1356    22195 0.3612157   5.9123759   know      12
## new    1282    23477 0.3415033   6.2538792    new      13
## go     1206    24683 0.3212582   6.5751374     go      14
## see    1203    25886 0.3204590   6.8955964    see      15
## great  1111    26997 0.2959518   7.1915482  great      16
## people 1089    28086 0.2900913   7.4816395 people      17
## back   1088    29174 0.2898250   7.7714645   back      18
## think  1051    30225 0.2799688   8.0514333  think      19
## make   1028    31253 0.2738420   8.3252752   make      20
```

We can see the top-20 words, the meaning of each of the columns is:

* *freq*: number of times that the word appears in the Corpora.

* *acumfreq*: accumulated frequency (considering the previous n-1 words).

* *perfreq*: percentage frequency of the work in the Corpora. 

* *acumperfreq*: accumulated percentage frequency (considering the previous n-1 words).

* *numwords*: the number of words included in the accumulated values.

For example, we can see that the word **now** in row **11**, appears *1469* times into the Corpora (*freq*), this value is equivalent to *0,39%* of the total corpora size (*perfreq*). Because the table is sorted, the *acumulated frequency* of the word **now** is *20839*, it means the sum of all the previous words *frecuencies* in the table plus it's *freq* value: *19370 + 1469 = 20839* (*acumfreq*) that is equivalent to *5.55%* of the total corpora size (*acumperfreq*). Also, we can see that in order to cover the *5.55%* of the Corpora we need only **11** words (*numword*).

Let's plot the top-20 words and also make the worcloud:


```r
## Plot Top 20 Words
ggplot(data=head(all.words,20),
       aes(x=reorder(word,-freq), y=freq)) +
  geom_bar(stat ="identity", position= "identity") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1,size=12)) +
  labs(title="Top 20 Words") +
  labs(x="Top Words",y="Count") 
```

![](Middle_Report_files/figure-html/unnamed-chunk-17-1.png) 

```r
## Plot Worcloud with top 20 words
plot(all.dfm, max.words = 20, random.order = FALSE, colors = brewer.pal(6, "Dark2"))
```

![](Middle_Report_files/figure-html/unnamed-chunk-17-2.png) 

### 2.7 Amount of Unique Words needed to Cover all Word Instances in the Language

We can use the previous information to validate how many unique words we need in order
to cover **50%** and **90%** of the total language (the *numwords* associated with the desired *acumfreq* value): 


```r
head(subset(all.words, acumperfreq >= 50 & acumperfreq <= 51),1)
```

```
##        freq acumfreq    perfreq acumperfreq   word numword
## hoping   74   187757 0.01971236    50.01532 hoping     820
```

```r
head(subset(all.words, acumperfreq >= 90 & acumperfreq <= 91),1)
```

```
##             freq acumfreq      perfreq acumperfreq        word numword
## disciplined    3   337860 0.0007991497    90.00024 disciplined   13493
```

We can see that we only need aproximately **820** words in order to cover the **50%**, and **13493** to cover the **90%**.

In the following plot we can observe this:


```r
ggplot(data=all.words, aes(x=numword, y=acumperfreq)) +
  geom_line(stat ="identity", position= "identity",size=1.2, colour="black") +
  geom_text(data=subset(all.words, numword == 820 | numword == 13493),
            aes(label=paste("(",acumperfreq,",",numword,")")), 
            hjust = 1.2, vjust = -0.4) +
  geom_vline(xintercept = 820, color="red") +
  geom_hline(aes(yintercept=50), color="red") +
  geom_vline(xintercept = 13493, color="blue") +
  geom_hline(aes(yintercept=90), color="blue") +
  labs(title="Number of Words needed to Cover all Words Instances") +
  scale_x_continuous( trans = "log10") +   
  labs(x="Number of Words",y="%Coverage")
```

![](Middle_Report_files/figure-html/unnamed-chunk-19-1.png) 

We can use those results in the prediction algorithm in order to speed up the processing time. 

## 2.8 N-grams

Let's analyze the n-grams of the dataset. 

### 2.8.1 Uni-grams

In order to check the unigrams, bigrams and trigrams of the all dataset, we will use the function *dfm()* from **quanteda** using the *ngram* option parameter. 

By default, the *dfm()* function calculate the unigrams of the texts, so the results that we got in the section **2.6 Word Analysis with Document Feature Matrix** correspond to the **unigrams** of the Corpora.  
Let's calculate the Bigrams and Trigrams of the corpora and plot the top-20 n-grams and wordcloud for each one:

### 2.8.2 Bigrams

We will calculate the **bigrams** of the Corpora trought the *dfm()* function, using the same cleaning options for the unigrams.


```r
all.bigrams.dfm <- dfm(all.corpus, 
             toLower = TRUE,
             removeNumbers = TRUE, 
             removePunct = TRUE, 
             removeSeparators = TRUE,
             removeTwitter = TRUE, 
             stem = FALSE, 
             language = "english",
             ignoredFeatures = stopwords("english"),
             ngrams=2)
```

```
## Creating a dfm from a corpus ...
##    ... lowercasing
##    ... tokenizing
##    ... indexing documents: 33,365 documents
##    ... indexing features: 313,479 feature types
##    ... removed 178,553 features, from 174 supplied (glob) feature types
##    ... created a 33365 x 134927 sparse dfm
##    ... complete. 
## Elapsed time: 8.59 seconds.
```

We can see that exists **313479** features.

Let's plot the **top-20 bigrams** and the *wordcloud* related:


```r
top20.bigrams <- data.frame(topfeatures(all.bigrams.dfm,20))
colnames(top20.bigrams)[1] <- "freq"

top20.bigrams[,2] <- rownames(top20.bigrams)
colnames(top20.bigrams)[2] <- "bigrams"

ggplot(data=top20.bigrams,aes(x=reorder(bigrams,-freq), y=freq)) +
  geom_bar(stat ="identity", position= "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Top 20 Bigrams") +
  labs(x="bigrams",y="Count") 
```

![](Middle_Report_files/figure-html/unnamed-chunk-21-1.png) 

```r
## Plot Worcloud with top 20 words
plot(all.bigrams.dfm, max.words = 20, random.order = FALSE, 
     rot.per=0.35,scale=c(3,0.5),
     colors = brewer.pal(6, "Dark2"))
```

![](Middle_Report_files/figure-html/unnamed-chunk-21-2.png) 

### 2.8.3 Trigrams

Let's calculate the **trigrams** of the Corpora, using the same *dfm()* function:


```r
all.trigrams.dfm <- dfm(all.corpus, 
             toLower = TRUE,
             removeNumbers = TRUE, 
             removePunct = TRUE, 
             removeSeparators = TRUE,
             removeTwitter = TRUE, 
             stem = FALSE, 
             language = "english",
             ignoredFeatures = stopwords("english"),
             ngrams=3)
```

```
## Creating a dfm from a corpus ...
##    ... lowercasing
##    ... tokenizing
##    ... indexing documents: 33,365 documents
##    ... indexing features: 531,460 feature types
##    ... removed 462,272 features, from 174 supplied (glob) feature types
##    ... created a 33365 x 69189 sparse dfm
##    ... complete. 
## Elapsed time: 13.88 seconds.
```

We can see that exists **531460** features.

Let's plot the **top-20 trigrams** and the *wordcloud* related:


```r
top20.trigrams <- data.frame(topfeatures(all.trigrams.dfm,20))
colnames(top20.trigrams)[1] <- "freq"

top20.trigrams[,2] <- rownames(top20.trigrams)
colnames(top20.trigrams)[2] <- "trigrams"

ggplot(data=top20.trigrams,aes(x=reorder(trigrams,-freq), y=freq)) +
  geom_bar(stat ="identity", position= "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Top 20 Trigrams") +
  labs(x="trigrams",y="Count")
```

![](Middle_Report_files/figure-html/unnamed-chunk-23-1.png) 

```r
## Plot Worcloud with top 20 words
plot(all.trigrams.dfm, max.words = 20, random.order = FALSE, scale=c(2,0.5),
     colors = brewer.pal(6, "Dark2"))
```

![](Middle_Report_files/figure-html/unnamed-chunk-23-2.png) 

We can see that some of the trigrams should be cleaned, words like **happy mother's day** and **happy mothers day** are the same and should be considered as equal, also words like **please please please** and **love love love** must be handled. This will be take into consideration for next steps.

## 3. How to Identify and Clean Non-english Words (Work in progress)

One option to filter the Corpora and remove the **non-english words** will be using a english dictionary to do that. Is neccesary to find a english dictionary and work on it.

Another choice is a library named **textcat** that make text cathegorization based on n-grams thought the function *textcat()*.
Let's see how this works:


```r
library(textcat)
textcat(c("This is a english sentence",
          "Esto es una oracion en espanol",
          "This is esto es datos",
          "madre",
          "father",
          "bonjour",
          "merci"),
        p=ECIMCI_profiles)
```

```
## [1] "en" "es" "pt" "no" "en" "fr" "it"
```

Is working good but is not perfect. Let's see how this works with our data:


```r
all.tokens <- toLower(
  tokenize(all.corpus, what = "fasterword",
         removeNumbers = TRUE, 
         removePunct = TRUE, 
         removeSeparators = TRUE,
         removeTwitter = TRUE, 
         removeURL = TRUE))
all.tokens[1:8]     
```

```
## $text1
##  [1] "listening"   "to"          "the"         "socialmedia" "experts"    
##  [6] "from"        "at"          "s"           "cyberposium" "now"        
## [11] "they"        "are"         "awesome"    
## 
## $text2
## [1] "god"   "i"     "miss"  "those"
## 
## $text3
## [1] "kay"    "thanks"
## 
## $text4
##  [1] "halftime"   "show"       "strong"     "production" "she"       
##  [6] "can"        "still"      "move"       "like"       "jagger"    
## 
## $text5
##  [1] "this"       "week"       "will"       "forever"    "be"        
##  [6] "known"      "as"         "the"        "one"        "with"      
## [11] "all"        "the"        "unexpected" "meetings"   "whether"   
## [16] "ill"        "accomplish" "anything"   "i"          "planned"   
## [21] "is"         "yet"        "to"         "be"         "seen"      
## 
## $text6
##  [1] "the"      "cruel"    "irony"    "of"       "this"     "game"    
##  [7] "it"       "was"      "the"      "pitching" "and"      "3-4"     
## [13] "spot"     "that"     "let"      "us"       "down"     "and"     
## [19] "of"       "course"   "we"       "wouldnt"  "have"     "been"    
## [25] "here"     "without"  "em"      
## 
## $text7
##  [1] "thinking" "if"       "i"        "really"   "want"     "people"  
##  [7] "to"       "come"     "over"     "saturday"
## 
## $text8
## [1] "always"      "smile"       "bro"         "follow"      "me"         
## [6] "belieberboy"
```

```r
textcat(all.tokens[1:8], p=ECIMCI_profiles)
```

```
## text1 text2 text3 text4 text5 text6 text7 text8 
##  "en"  "en"  "en"  "en"  "en"  "en"  "en"  "en"
```

There still some problems that I need to fix regarding characters encoding (some characters that the function doesn't handle). This texts is a good example of this issue, the function doesn't work with it:

```r
all.corpus[9]
```

```
##                                                                                                                                           text9 
## "I think the reason I love #BoyMeetsWorld so much is because Cory and Topanga remind me of and me <f0><U+009F><U+0092><U+0097><f0><U+009F><U+0098><U+0098>"
```


## 4. Future Work - Prediction Algorithm

The next steps that I will perform to create a prediction algortihm will be:

* Considering how to fix the issues with the *textcat()* function to remove the non-english words.

* Considering how to reduce the amount of n-grams using steamming or addiotional filtering to improve the runtime of the prediction algorithm.

* Consider how to create the prediction algorithm using the n-grams and how to predict the words that can not be handle by the algorithm perse.

* Design the GUI of the Shiny App and star working on it, taking into consideration runtime and memory restrictions.
