###### ------ Applying my fav stm <3 --------------

library(stm)
library(quanteda)
library(tidyverse)
library(textreadr)
library(tidytext)
library(stringr)
library(stortingscrape)
library(SnowballC)
library(ggwordcloud)

## Loading my stm 

load("C:/Users/gardi/OneDrive/Documents/GardSTV2022H22Repo/data/topicmodelK_210.RData")

## Reading the new texts 

mps <- get_parlperiod_mps(periodid = "2021-2025", substitute = TRUE, good_manners = 0)


Sys.setlocale("LC_ALL", "")
newquestions <- data.frame(qnumber = c(1:20),
                           text = readLines("TolvteGang/questions.txt"),
                           qparty = str_extract(readLines("TolvteGang/meta.txt"), "\\([a-zA-Z]{1,3}\\)"),
                           qpers = str_remove(str_extract(readLines("TolvteGang/meta.txt"), "fra [:alpha:]+ [:alpha:]+ ?[:alpha:]+"), "fra "),
                           qansw = str_extract(readLines("TolvteGang/meta.txt"), "(?:[:alpha:]+\\-\\sog\\s)?[:alpha:]+ministeren")
                           )

## Tokenising and dfm'ing the new questions

tokens <- unnest_tokens(newquestions,
                        input = text,
                        output = unigrams,
                        token = "words")

futuredf <- tokens %>% 
  select(-qparty, -qansw, -qpers)

minestoppord <- data.frame(unigrams = stopwords(language = "no", source = "snowball"))



newdf <- futuredf %>% 
  anti_join(minestoppord, by = c("unigrams"))

newdf <- newdf %>% 
  mutate(stemgrams = quanteda::char_wordstem(unigrams, language = "no"))

newdfm <- newdf %>% 
  count(qnumber, stemgrams, name = "count") %>% 
  cast_dfm(qnumber, 
           stemgrams, 
           count)

## Fitting to the old stm

newstm <- convert(newdfm, to = "stm")

newstm2 <- alignCorpus(newstm, old.vocab = topicmodel1$vocab)

newdoctopics <- fitNewDocuments(model = topicmodel1,
                documents = newstm2$documents)

newdoctopics$theta

newdoctheta <- data.frame(newdoctopics$theta)

newdoctheta <- newdoctheta %>% 
  mutate(doc = c(1:20))

topicsstuff <- pivot_longer(newdoctheta, cols = c(1:210))

## Extracting top topics

topicsstuff2 <- topicsstuff %>% 
  group_by(doc) %>% 
  slice_max(order_by = value, n = 1)

## Making a vector of the top topics

thetopics <- str_remove(topicsstuff2$name, "X")

## Wordclouding the top topics, drawing them to pdf

topicmodel1_topics <- tidy(topicmodel1, 
                           matrix = "beta")

tokenexploringframe <- topicmodel1_topics %>%
  group_by(topic) %>% # Getting the top term per topic, thus using group_by
  slice_max(beta, n = 10) %>% # Fetching the 10 terms with the highest beta
  ungroup() # Ungrouping to get the dataframe back to normal


for(i in thetopics){
  tmp1 <- tokenexploringframe %>% filter(topic == i)
  
  tmp2 <- ggplot(tmp1, aes(label = term, size = beta)) + 
    geom_text_wordcloud() + theme_minimal() + scale_size_area(max_size=20)
  
  ggsave(tmp2, filename = (paste0("funimages/topic", i, "wordcloud.pdf")))
  
  Sys.sleep(2)
}

## Extracting top topics

Sys.setlocale("LC_ALL", "")

obj <- c("Da ", "Kan ", "Vi ", "Ha ", "Det ", "Gøy ", "i ", "R!")

obj2 <- paste(obj, collapse = "")

print(obj2)

## Mapping the beta values of the relevant words in a wordcloud