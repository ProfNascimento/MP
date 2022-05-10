library(tm)
library(wordcloud)
library(qdap)

review <- read.csv("https://raw.githubusercontent.com/ProfNascimento/MP/main/Womens%20Clothing%20E-Commerce%20Reviews.csv", stringsAsFactors=F)
colnames(review)

#Step1 : Creating a Corpus
corpus_review <- Corpus(VectorSource(review$Review.Text))

#Step 2 : Text preprocessing
corpus_review <- tm_map(corpus_review, tolower)

corpus_review <- tm_map(corpus_review, removeNumbers)

corpus_review <- tm_map(corpus_review , removePunctuation)

corpus_review <- tm_map(corpus_review,removeWords , stopwords("english"))

corpus_review <- tm_map(corpus_review, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress", "just", "i"))

corpus_review <- tm_map(corpus_review , stemDocument)
corpus_review [[8]][1]

# term_count <- freq_terms(corpus_review,20)
# plot(term_count)


#Step 3: Creating DTM or TDM
review_tdm <- TermDocumentMatrix(corpus_review)

review_m <- as.matrix(review_tdm)
review_term_freq <- rowSums(review_m)

review_term_freq <- sort(review_term_freq,T)
review_term_freq[1:10]

#Comparison of Corpus
pos_comments <- subset(review$Review.Text , review$Recommended.IND==1)
neg_comments <- subset(review$Review.Text , review$Recommended.IND==0)

pos_terms <- paste(pos_comments , collapse =" ")
neg_terms <- paste(neg_comments , collapse =" ")

#Combine both positive and negative terms
all_terms <- c(pos_terms, neg_terms)

#Creating corpus for all the terms
all_corpus <- VCorpus(VectorSource(all_terms))

all_tdm <- TermDocumentMatrix(
  # Use all_corpus
  all_corpus, 
  control = list(
    # Use TFIDF weighting
    #weighting = weightTfIdf, 
    # Remove the punctuation
    removePunctuation = TRUE,
    #Remove numbers
    removeNumbers =TRUE,
    #Stemming of Documents
    stemDocument = TRUE,
    #Convert to lowercase
    tolower = TRUE ,
    # Use English stopwords
    stopwords = stopwords("english")
    
  )
)

all_tdm_m <- as.matrix(all_tdm)

colnames(all_tdm_m) <- c("positive","negative")

all_term_freq <- rowSums(all_tdm_m)

all_term_freq <- sort(all_term_freq,TRUE)

#Step 4 : Exploratory Text Analysis
par(mfrow = c(1,2))
barplot(review_term_freq[1:20] , col ="gray" , las=2)

comparison.cloud(
  all_tdm_m, 
  max.words = 100,
  colors = c("steel blue", "darkred")
)

#####################################
require(mp)
require(umap)

set.seed(1641)

list.vis = list(PCA=NULL, MDS=NULL, ForceScheme=NULL, LAMP=NULL, tSNE=NULL, UMAP=NULL)

list.vis[["PCA"]] = prcomp(review_m)$x[, 1:2]  #stats

data.dist = dist(review_m)

list.vis[["MDS"]] = cmdscale(data.dist)    #stats

list.vis[["ForceScheme"]] = forceScheme(data.dist)

list.vis[["LAMP"]] = lamp(review_m)

list.vis[["tSNE"]] = tSNE(review_m)

list.vis[["UMAP"]] = umap(review_m)$layout  

## PLOTTING MPs
par(mfrow = c(2, 3), oma = c(0, 0, 0, 0), mar = c(0.5, 0.5, 2.0, 0.5))
labels = as.factor(review$Recommended.IND)

for(i in 1:length(list.vis))
  plot(list.vis[[i]], main = names(list.vis[i]), 
       bg = c("darkred","steel blue"), col = c("darkred","steel blue"),  pch = 21, 
       xlab = "", ylab = "", xaxt = "n", yaxt = "n")

legend("left",legend=c("Negative","Positive"),col=c("darkred","steel blue"),pch=19)
par(current.op)
