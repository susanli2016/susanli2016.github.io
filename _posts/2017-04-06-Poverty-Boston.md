---
layout: post
title: "Mapping Poverty in Boston"
excerpt: "Mapping, Census Data"
tags: [rstats]
share: true
comments: true
---

[2016 Canadian census data will be released later this year](http://www12.statcan.gc.ca/census-recensement/2016/ref/release-dates-diffusion-eng.cfm), according to [Statistics Canada](http://www.statcan.gc.ca/eng/start). I wasn't going to wait, I decided to play around with [U.S. Census Bureau 2010-2014 ACS (American Community Survey) data](https://www.census.gov/programs-surveys/acs/). 

For this project, I will use a number of packages as follows:

{% highlight r %}
library(sp)
library(rgdal)
library(tigris)
library(dplyr)
library(maptools) 
library(ggplot2)
library(ggmap) 
{% endhighlight %}

First, [apply API key from the US Census Bureau](http://api.census.gov/data/key_signup.html).

{% highlight r %}
library(acs14lite)
census_api_key <- "Your_Census_Key"
set_api_key(census_api_key)
{% endhighlight %}

### Fetch the census data. 

It took me sometime to understand the data and the methodology. I am interested in the poverty in Boston. So I should be looking for table B17021 - Poverty Status of Individuals in the Past 12 Months by Living Arrangement. Within this table, I should fetch the following variables:

* B17021_001E - count of people for whom poverty status has been determined (the sample estimate)
* B17021_001M: count of people for whom poverty status has been determined (the margin of error)
* B17021_002E: count of those people whose income in the past 12 months is below poverty (estimate)
* B17021_002M: count of those people whose income in the past 12 months is below poverty (margin of error)

Boston is the seat of Suffolk county. Cool! it works like a charm! 

{% highlight r %}
bos_poverty <- acs14(geography = 'tract', state = 'MA', county = 'Suffolk', 
                 variable = c('B17021_001E', 'B17021_001M', 'B17021_002E', 'B17021_002M'))
head(bos_poverty)
{% endhighlight %}

{% highlight text %}
##                                             NAME B17021_001E B17021_001M
##1    Census Tract 1, Suffolk County, Massachusetts        3255         427
##2 Census Tract 2.01, Suffolk County, Massachusetts        3073         352
##3 Census Tract 2.02, Suffolk County, Massachusetts        3482         445
##4 Census Tract 3.01, Suffolk County, Massachusetts        2330         317
##5 Census Tract 3.02, Suffolk County, Massachusetts        2597         323
##6 Census Tract 4.01, Suffolk County, Massachusetts        5074         389
##  B17021_002E B17021_002M state county  tract
##1         555         191    25    025 000100
##2         405         178    25    025 000201
##3         791         340    25    025 000202
##4         328         172    25    025 000301
##5         580         231    25    025 000302
##6        1282         293    25    025 000401
{% endhighlight %}

The related table can be found at [Census FactFinder](https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_14_5YR_B17021&prodType=table)

Next, I need to convert poverty from count to percentage, then calculate the margin of error(MOE)for each percentage. Because MOE is an indicator of the reliability of ACS estimates. Adding the MOE to the estimate provides an upper limit and subtracting the MOE from the estimate provides a lower limit of the range where the true value of the estimate most likely actually falls.

{% highlight r %}
bos_poverty_1 <- bos_poverty %>%
  mutate(geoid = paste0(state, county, tract),
         pct = round(100 * (B17021_002E / B17021_001E), 1),
         moe = round(100 * (moe_prop(B17021_002E, B17021_001E, B17021_002M, B17021_001M)), 1)) %>%
  select(geoid, pct, moe)
head(bos_poverty_1)
{% endhighlight %}

{% highlight text %}
##       geoid  pct moe
##1 25025000100 17.1 5.4
##2 25025000201 13.2 5.6
##3 25025000202 22.7 9.3
##4 25025000301 14.1 7.1
##5 25025000302 22.3 8.5
##6 25025000401 25.3 5.4
{% endhighlight %}

### Link ACS data to TIGER census tracts, then join the ACS data to the tracts spatial data.

Looks like I am doing it right. 

{% highlight r %}
bos_tract <- tracts('MA', 'Suffolk', cb=TRUE)
bos_tract_2 <- geo_join(bos_tract, bos_poverty_1, "GEOID", "geoid")
str(bos_tract_2@data)
bos_tract_2 <- bos_tract_2[!is.na(bos_tract_2$pct),]
{% endhighlight %}

{% highlight text %}
##'data.frame':	204 obs. of  12 variables:
## $ STATEFP : chr  "25" "25" "25" "25" ...
## $ COUNTYFP: chr  "025" "025" "025" "025" ...
## $ TRACTCE : chr  "000602" "010500" "030500" "050101" ...
## $ AFFGEOID: chr  "1400000US25025000602" "1400000US25025010500" ##"1400000US25025030500" "1400000US25025050101" ...
## $ GEOID   : chr  "25025000602" "25025010500" "25025030500" "25025050101" ...
## $ NAME    : chr  "6.02" "105" "305" "501.01" ...
## $ LSAD    : chr  "CT" "CT" "CT" "CT" ...
## $ ALAND   : chr  "599552" "189911" "193673" "299771" ...
## $ AWATER  : chr  "0" "10075" "292691" "184516" ...
## $ geoid   : chr  "25025000602" "25025010500" "25025030500" "25025050101" ...
## $ pct     : num  31.5 34.9 8.2 23 19.2 23.3 39.9 36.3 20.8 38.4 ...
## $ moe     : num  6.8 8.7 3.5 7.5 7.9 12.7 9 7.6 6.7 7.4 ...
{% endhighlight %}

### Now we have a simple map of Suffolk county. 

{% highlight r %}
plot(bos_tract_2)
{% endhighlight %}

![poverty-1](/figs/2017-04-06-Poverty-Boston/poverty-1.png)

Use `fortify` function to turn the map into a data frame so that it can easily be plotted with `ggplot2`, which produce this data frame:

{% highlight r %}
map_data <- fortify(bos_tract_2, data=bos_tract_2@data, region="geoid")
head(map_data)
{% endhighlight %}

{% highlight text %}
##   long   lat order  hole piece          id         group
##1 -71.16 42.36     1 FALSE     1 25025000100 25025000100.1
##2 -71.15 42.36     2 FALSE     1 25025000100 25025000100.1
##3 -71.15 42.36     3 FALSE     1 25025000100 25025000100.1
##4 -71.15 42.36     4 FALSE     1 25025000100 25025000100.1
##5 -71.14 42.36     5 FALSE     1 25025000100 25025000100.1
## -71.14 42.36     6 FALSE     1 25025000100 25025000100.1
{% endhighlight %}

### Merge our ACS data to the fortified data frame.

{% highlight r %}
map_data <- merge(map_data, bos_tract_2@data, by.x="id", by.y="geoid")
head(map_data)
{% endhighlight %}

### Now I have a perfect data frame for a map.

{% highlight text %}
##    id   long   lat order  hole piece     group STATEFP COUNTYFP ##TRACTCE
##1 25025000100 -71.16 42.36  1 FALSE   1 25025000100.1     25    025  000100
##2 25025000100 -71.15 42.36  2 FALSE   1 25025000100.1     25    025  000100
##3 25025000100 -71.15 42.36  3 FALSE   1 25025000100.1     25    025  000100
##4 25025000100 -71.15 42.36  4 FALSE   1 25025000100.1     25    025  000100
##5 25025000100 -71.14 42.36  5 FALSE   1 25025000100.1     25    025  000100
##6 25025000100 -71.14 42.36  6 FALSE   1 25025000100.1     25    025  000100
##              AFFGEOID       GEOID NAME LSAD   ALAND AWATER  pct moe
##1 1400000US25025000100 25025000100    1   CT 1795016      0 17.1 5.4
##2 1400000US25025000100 25025000100    1   CT 1795016      0 17.1 5.4
##3 1400000US25025000100 25025000100    1   CT 1795016      0 17.1 5.4
##4 1400000US25025000100 25025000100    1   CT 1795016      0 17.1 5.4
##5 1400000US25025000100 25025000100    1   CT 1795016      0 17.1 5.4
##6 1400000US25025000100 25025000100    1   CT 1795016      0 17.1 5.4
{% endhighlight %}

{% highlight r %}
ggplot() +
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group, fill = pct)) +
  scale_fill_gradient(name='Percent',limits=c(0, 80), low="#56B1F7", high="#132B43")+
  guides(fill = guide_legend()) +
  ggtitle("Percent of Individuals below Poverty Level") +
  theme_classic() +
  coord_map()
{% endhighlight %}

![poverty-2](/figs/2017-04-06-Poverty-Boston/poverty-2.png)

### Let's make the map fancier.

{% highlight r %} 
bos_basemap <-get_map('Boston', zoom=12)
ggmap(bos_basemap) +
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group, fill = pct)) +
  scale_fill_gradient(name='Percent',limits=c(0, 80), low="#56B1F7", high="#132B43") +
  guides(fill = guide_legend()) +
  ggtitle("Percent of Individuals below Poverty Level") +
  theme_nothing(legend=TRUE) +
  coord_map()
{% endhighlight %}

![poverty-3](/figs/2017-04-06-Poverty-Boston/poverty-3.png)

### The End 

I enjoyed learning US Census data, and learning about maps and how to make them is very rewarding. `tigris` and `acs14lite` packages developed by Kyle Walker (https://walkerke.github.io/) are essential for this small project. I look forward to mapping Canadian census data later this year. 

Source code that created this post can be found [here](https://github.com/susanli2016/Data-Analysis-with-R/blob/master/Mapping-Boston-Poverty.Rmd). I am happy to hear any feedback and questions.