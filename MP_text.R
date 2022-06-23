library(tm)
library(qdap)

review <- read.csv("https://raw.githubusercontent.com/ProfNascimento/MP/main/Womens%20Clothing%20E-Commerce%20Reviews.csv", stringsAsFactors=F)

#Step1 : Creating a Corpus
corpus_review <- Corpus(VectorSource(review$Review.Text))

#Step 2 : Text preprocessing
corpus_review <- tm_map(corpus_review, tolower)
corpus_review <- tm_map(corpus_review, removeNumbers)
corpus_review <- tm_map(corpus_review , removePunctuation)
corpus_review <- tm_map(corpus_review,removeWords , stopwords("english"))
corpus_review <- tm_map(corpus_review, removeWords,c("also", "get","like", "company", "made", "can", "im", "dress", "just", "i"))
corpus_review <- tm_map(corpus_review , stemDocument)

#Step 3: Creating DTM or TDM
review_tdm <- TermDocumentMatrix(corpus_review)
review_m <- as.matrix(review_tdm)

#####################################
## SELECTING ONLY ITEM --Jackets-- ##
review$Class.Name=as.factor(review$Class.Name)
levels(review$Class.Name)
review_jackets=t(review_m)[review$Class.Name=="Jackets",]

## PCA
res.pca=prcomp(review_jackets)

## Interpreting PCA 2D projection
require(factoextra)
fviz_pca_ind(res.pca, 
             #select.ind = list(cos2 = 30),
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)+theme(text = element_text(size = 15))

review$Review.Text[8551]
review$Review.Text[12374]

###############################################################
require(mp)
require(umap)
set.seed(1641)

list.vis = list(PCA=NULL, MDS=NULL, ForceScheme=NULL, LAMP=NULL, tSNE=NULL, UMAP=NULL)

list.vis[["PCA"]] = res.pca$x[, 1:2]  #stats

data.dist = dist(review_jackets)

list.vis[["MDS"]] = cmdscale(data.dist)    #stats

list.vis[["ForceScheme"]] = forceScheme(data.dist)

list.vis[["LAMP"]] = lamp(review_jackets)

list.vis[["tSNE"]] = tSNE(review_jackets)

list.vis[["UMAP"]] = umap(review_jackets)$layout  

## PLOTTING MPs
par(mfrow = c(2, 3), oma = c(0, 0, 0, 0), mar = c(0.5, 0.5, 2.0, 0.5))
labels = ifelse(review$Recommended.IND[review$Class.Name=="Jackets"]==1,"steel blue","darkred")

for(i in 1:length(list.vis)){
  plot(list.vis[[i]], main = names(list.vis[i]), 
       col = labels,  pch = 19, 
       xlab = "", ylab = "", xaxt = "n", yaxt = "n")}

legend("topleft",
       legend=c("Negative","Positive"),
       col=c("darkred","steel blue"),pch=19)
