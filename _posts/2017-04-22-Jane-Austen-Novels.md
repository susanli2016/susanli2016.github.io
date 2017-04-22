---
layout: post
title: "Text Mining Jane Austen's Novels"
excerpt: "Sentiment Analysis, Text Mining"
tags: [rstats]
share: true
comments: true
---

Why are Jane Austen’s novels so popular? With this question in mind, I started analyze all of her six novels.I will need the following packages for this project.

{% highlight r %}
library(tidyverse)      
library(stringr)        
library(tidytext)       
library(janeaustenr)
library(dplyr)
library(ggplot2)
library(reshape2)
{% endhighlight %}


{% highlight r %}
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()
tidy_books <- original_books %>%
  unnest_tokens(word, text)
tidy_books
data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words)
{% endhighlight %}

{% highlight text %}
## A tibble: 725,054 × 4
##                  book linenumber chapter        word
##                <fctr>      <int>   <int>       <chr>
##1  Sense & Sensibility          1       0       sense
##2  Sense & Sensibility          1       0         and
##3  Sense & Sensibility          1       0 sensibility
##4  Sense & Sensibility          3       0          by
##5  Sense & Sensibility          3       0        jane
##6  Sense & Sensibility          3       0      austen
##7  Sense & Sensibility          5       0        1811
##8  Sense & Sensibility         10       1     chapter
##9  Sense & Sensibility         10       1           1
##10 Sense & Sensibility         13       1         the
## ... with 725,044 more rows
{% endhighlight %}

{% highlight r %}
tidy_books %>%
  count(word, sort = TRUE)
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) + ggtitle("The Most Common Words in Jane Austen's Novels") +
  coord_flip()
{% endhighlight %}

![novel-1](/figs/2017-04-22-Jane-Austen-Novels/novel-1.png)

### Sentiment of Jane Austen's novels

{% highlight r %}
tidy_books1 <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
{% endhighlight %}

{% highlight r %}
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)
janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) + ggtitle("Positive and Negative Sentiment of Jane Austen's Novels") +
  facet_wrap(~book, ncol = 2, scales = "free_x")
{% endhighlight %}

{% highlight text %}
## A tibble: 13,914 × 2
##     word     n
##    <chr> <int>
##1    miss  1855
##2    time  1337
##3   fanny   862
##4    dear   822
##5    lady   817
##6     sir   806
##7     day   797
##8    emma   787
##9  sister   727
##10  house   699
## ... with 13,904 more rows
{% endhighlight %}

![novel-2](/figs/2017-04-22-Jane-Austen-Novels/novel-2.png)

{% highlight r %}
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + ggtitle("Words Contribute to Positive and Negative Sentiment of Jane Austen's Novels ") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
{% endhighlight %}

![novel-3](/figs/2017-04-22-Jane-Austen-Novels/novel-3.png)

The most common words in Jane Austen’s novels

{% highlight r %}
library(wordcloud)

tidy_books1 %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
{% endhighlight %}

![novel-4](/figs/2017-04-22-Jane-Austen-Novels/novel-4.png)

The Most common positive and negative words in Jane Austen’s novels

{% highlight r %}
tidy_books1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)
{% endhighlight %}

![novel-5](/figs/2017-04-22-Jane-Austen-Novels/novel-5.png)


{% highlight r %}
book_words1 <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()
total_words <- book_words1 %>% 
  group_by(book) %>% 
  summarize(total = sum(n))
book_words1 <- left_join(book_words, total_words)
book_words1 <- book_words1 %>%
  bind_tf_idf(word, book, n)
book_words1 %>%
  select(-total) %>%
  arrange(desc(tf_idf))
plot_austen <- book_words1 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))
plot_austen %>% 
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() + ggtitle("Most Frequent words in Jane Austen’s Novels")
{% endhighlight %}

![novel-6](/figs/2017-04-22-Jane-Austen-Novels/novel-6.png)

{% highlight r %}
plot_austen %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip() + ggtitle("Most Frequent Words in each of Jane Austen's novels")
{% endhighlight %}

![novel-7](/figs/2017-04-22-Jane-Austen-Novels/novel-7.png)

To be continued.








