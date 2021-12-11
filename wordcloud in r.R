install.packages("wordcloud")
install.packages("tm")
install.packages("pdftools")
library(tm)
library(wordcloud)
library(pdftools)

txt <- pdf_text(file.choose())
str(txt)
class(txt)
cat(txt[1:2])
#create corpus - vcorpus, pcorpus
txt_corpus <- Corpus(VectorSource(txt))
txt_corpus
inspect(txt_corpus[[1]])
as.character(txt_corpus[1:2])
t = lapply(txt_corpus, as.character)
t

#data cleaning
txt_corpus = tm_map(txt_corpus,removeNumbers)
lapply(txt_corpus, as.character)

txt_corpus = tm_map(txt_corpus,to_lower)
lapply(txt_corpus,as.character)

txt_corpus = tm_map(txt_corpus,removePunctuation)
lapply(txt_corpus, as.character)

txt_corpus = tm_map(txt_corpus,stripWhitespace)
lapply(txt_corpus, as.character)

stopwords('en')
txt_corpus = tm_map(txt_corpus,removeWords,stopwords('en'))
lapply(txt_corpus, as.character)

#Tokenization using dtm
dtm <- DocumentTermMatrix(txt_corpus)
dtm <- as.matrix(dtm)
View(dtm)
dtm = t(dtm)
occu = rowSums(dtm)
no_occu = sort(occu,decreasing = T)
head(no_occu)

#plot wordcloud
wordcloud(txt_corpus,min.freq = 50,random.order = F,
          color = brewer.pal(8,"Dark2"),rot.per = 0.60)
