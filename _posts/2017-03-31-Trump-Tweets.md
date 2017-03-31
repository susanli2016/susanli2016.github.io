---
layout: post
title: "Text Mining, - Trump's Latest Tweets"
excerpt: "Tweets"
tags: [rstats]
share: true
comments: true
---

Just finished a few hours text mining lesson, can't wait to put my new skill into practice, starting from Trump's tweets.

First, apply API keys from twitter. 

{% highlight r %}
library(twitteR)
consumer_key <- "Your_Consumer_Key"
consumer_secret <- "Your_Consumer_Secret"
access_token <- NULL
access_secret <- NULL
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
{% endhighlight %}

The maximize request is 3200 tweets, I got 791, which is not bad.

{% highlight r %}
tweets <- userTimeline("realDonaldTrump", n = 3200)
(n.tweet <- length(tweets))
{% endhighlight %}

{% highlight text %}
##[1] 791
{% endhighlight %}

Have a look the first three

{% highlight r %}
tweets[1:3]
{% endhighlight %}

{% highlight text %}
##[[1]]
##[1] "realDonaldTrump: Only by enlisting the full potential of women in our ##society will we be truly able to ###MakeAmericaGreatAgain\xed��\xed��\xed��\xed��… https://t.co/cxhgYaxiek"

##[[2]]
##[1] "realDonaldTrump: ...and job losses. American companies must be prepared to ##look at other alternatives."

##[[3]]
##[1] "realDonaldTrump: The meeting next week with China will be a very difficult ##one in that we can no longer have massive trade deficits..."
{% endhighlight %}

Text cleaning process, which includes convert all letters to lower case, remove URL, remove anything other than English letter and space, remove stopwords and extra white space. 

{% highlight r %}
library(tm) 
library(stringr)
myCorpus <- Corpus(VectorSource(tweets.df$text)) 
myCorpus <- tm_map(myCorpus, content_transformer(str_to_lower))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct)) 
myStopwords <- myStopwords <- c(stopwords('english'), "amp", "trump") 
myCorpus <- tm_map(myCorpus, removeWords, myStopwords) 
myCorpus <- tm_map(myCorpus, stripWhitespace)
{% endhighlight %}

Look at these three tweets again

{% highlight r %}
myCorpus <- tm_map(myCorpus, stemDocument)
inspect(myCorpus[1:3])
{% endhighlight %}

{% highlight text %}
##<<SimpleCorpus>>
##Metadata:  corpus specific: 1, document level (indexed): 0
##Content:  documents: 3

##[1] enlist full potenti women societi will truli abl makeamericagreatagain 
##[2] job loss american compani must prepar look altern                      
##[3] meet next week china will difficult one can longer massiv trade deficit
{% endhighlight %}

Need to replace a few words, such as 'potenti' to 'potential', 'societi' to 'society', 'countri' to 'country'.  

{% highlight r %}
replaceWord <- function(corpus, oldword, newword) { 
  tm_map(corpus, content_transformer(gsub), 
         pattern=oldword, replacement=newword) 
} 
myCorpus <- replaceWord(myCorpus, "potenti", "potential") 
myCorpus <- replaceWord(myCorpus, "societi", "society") 
myCorpus <- replaceWord(myCorpus, "countri", "country")
{% endhighlight %}

### Building term document matrix

{% highlight r %}
tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf))) 
tdm
{% endhighlight %}

{% highlight text %}
##<<TermDocumentMatrix (terms: 2189, documents: 791)>>
##Non-/sparse entries: 8172/1723327
##Sparsity           : 100%
##Maximal term length: 29
##Weighting          : term frequency (tf)
{% endhighlight %}

As you can see, the term-document matrix is composed of 2189 terms and 791 documents(tweets). It is very sparse, with 100% of the entries being zero. Let's have a look at the terms of 'clinton', 'bad' and 'great', and tweets numbered 21 to 30.

{% highlight r %}
idx <- which(dimnames(tdm)$Terms %in% c("clinton", "bad", "great"))
as.matrix(tdm[idx, 21:30])
{% endhighlight %}

{% highlight text %}
##         Docs
##Terms     21 22 23 24 25 26 27 28 29 30
##  great    0  0  0  1  0  1  0  1  0  0
##  clinton  0  0  0  0  0  0  0  0  0  0
##  bad      1  0  0  0  0  0  0  0  0  0
{% endhighlight %}

### What are the top frequent terms?

{% highlight r %}
(freq.terms <- findFreqTerms(tdm, lowfreq = 30))
{% endhighlight %}

{% highlight text %}
##[1] "will"          "american"      "job"           "look"          "obamacar" ##[6] "get"           "great"         "media"         "year"          "today"    ##[11] "watch"         "country"       "peopl"         "elect"         "now"     ##[16] "clinton"       "fake"          "news"          "new"           "thank"   ##[21] "big"           "us"            "make"          "bad"           "win"     ##[26] "hillari"       "go"            "even"          "america"       "pm"      ##[31] "just"          "join"          "state"         "presid"        "time"    ##[36] "vote"          "draintheswamp"
{% endhighlight %}

### A picture worth a thousand words.

{% highlight r %}
term.freq <- rowSums(as.matrix(tdm)) 
term.freq <- subset(term.freq, term.freq >= 40) 
df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip() + theme(axis.text=element_text(size=7))
{% endhighlight %}

![Trump-1](/figs/2017-03-31-Trump-Tweets/Trump-1.png)

### Word Cloud

{% highlight r %}
m <- as.matrix(tdm) 
word.freq <- sort(rowSums(m), decreasing = T) 
library(RColorBrewer)
pal <- brewer.pal(9, "BuGn")[-(1:4)]
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)
{% endhighlight %}

![Trump-2](/figs/2017-03-31-Trump-Tweets/Trump-2.png)

### Which word/words are associated with 'will'? There are three.

{% highlight r %}
findAssocs(tdm, "will", 0.2)
{% endhighlight %}

{% highlight text %}
##$will
## bring   back togeth 
##  0.29   0.21   0.21
{% endhighlight %}

### Which word/words are associated with 'great'? - This is obvious.

{% highlight r %}
findAssocs(tdm, "great", 0.2)
{% endhighlight %}

{% highlight text %}
##$great
##america    make  togeth 
##   0.41    0.34    0.27
{% endhighlight %}

### There are many words are associated with 'bad'.

{% highlight r %}
findAssocs(tdm, "bad", 0.2)
{% endhighlight %}

{% highlight text %}
##$bad
##         ban         argu    everybodi         dude       stupid        trial 
##        0.30         0.29         0.29         0.29         0.29         0.29 
##       worst       intent        thing         judg        notic      televis 
##        0.21         0.21         0.21         0.21         0.20         0.20 
##relationship            u 
##        0.20         0.20
{% endhighlight %}

### Clustering Words

{% highlight r %}
tdm2 <- removeSparseTerms(tdm, sparse=0.95)
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward.D")
plot(fit)
rect.hclust(fit, k=10)
(groups <- cutree(fit, k=10))
{% endhighlight %}

![Trump-3](/figs/2017-03-31-Trump-Tweets/Trump-3.png)

We can see the words in the tweets, words 'will', 'great', 'thank', 'join', 'state' are not clustered into any group, 'hillari', 'clinton' and 'draintheswamp' are clustered into one group, 'us' is not clustered in any group, 'go', 'make', 'america', 'time', 'today', 'get', 'watch', 'job' and 'country' are clustered into one group, 'elect' is not clustered into any group, 'people' and 'just' are clustered into one group. 

### Clustering Tweets

{% highlight r %}
m3 <- t(m2)
set.seed(100)
k <- 8
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits=3)
{% endhighlight %}

{% highlight text %}
## will   job   get great today watch country peopl elect clinton thank    us  ##make
##1 1.175 0.021 0.072 0.144 0.082 0.021   0.093 0.082 0.010   0.010 0.062 0.103 ##0.093
##2 0.000 0.000 0.035 0.000 0.051 0.030   0.055 0.072 0.069   0.079 0.000 0.058 ##0.023
##3 0.077 0.000 0.231 0.096 0.038 0.308   0.019 0.000 0.077   0.019 1.000 0.077 ##0.019
##4 0.000 0.019 0.057 1.094 0.132 0.038   0.075 0.151 0.019   0.000 0.132 0.019 ##0.000
##5 0.075 0.025 0.050 0.050 0.000 0.100   0.050 0.200 0.175   0.075 0.050 0.075 ##0.050
##6 0.171 1.229 0.029 0.229 0.086 0.029   0.086 0.086 0.029   0.000 0.171 0.229 ##0.029
##7 0.622 0.054 0.108 1.081 0.054 0.108   0.027 0.081 0.000   0.000 0.270 0.027 ##0.973
##8 0.250 0.068 0.068 0.023 0.023 0.045   0.045 0.000 0.023   0.068 0.114 0.045 ##0.023
##  hillari    go america  just  join state  time draintheswamp
##1   0.041 0.093   0.010 0.010 0.021 0.062 0.000         0.041
##2   0.088 0.037   0.012 0.000 0.085 0.074 0.000         0.058
##3   0.019 0.173   0.019 0.000 0.038 0.019 0.000         0.019
##4   0.000 0.094   0.000 0.038 0.000 0.170 0.038         0.000
##5   0.050 0.000   0.000 1.050 0.000 0.050 0.000         0.000
##6   0.029 0.000   0.114 0.057 0.000 0.000 0.000         0.000
##7   0.000 0.162   0.892 0.027 0.000 0.027 0.000         0.054
##8   0.045 0.114   0.045 0.068 0.045 0.045 1.114         0.205
{% endhighlight %}

### Check the top three words in every cluster

{% highlight r %}
for (i in 1:k) {
cat(paste("cluster ", i, ": ", sep=""))
s <- sort(kmeansResult$centers[i,], decreasing=T)
cat(names(s)[1:3], "\n")
}
{% endhighlight %}

Cluster 1 talks about US will be great, cluster 2 talks about Hillary Clinton, cluster 3 seems talk about thanking people voting for him, cluster 4 talks about great people, cluster 5 seems talk about he was elected by people, cluster 6 talks about jobs, everyone knows cluster 7, cluster 8 is about cleaning up government corruptions. 

As of now, I am bored with his tweets, or at least bored with analyzing his tweets.

Source code that create this post can be found [here](https://github.com/susanli2016/Data-Analysis-with-R/blob/master/Donald-Trump-Tweets.Rmd). I am happy to hear any feedback and question.