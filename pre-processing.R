# ~~~ package install if needed only ~~~

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

# Create your own base working directory
# working directory <- "/Users/"... Mac Path
# working directory <- "C:\\Users\\"... Windows Path

# setwd("/Users/"...Mac Path
# setwd("C:\\Users\\"....Windows Path

getwd()
# ~~~ READ THE FILES INTO A FRAME ~~~
# Each column has to be labeled

Andy.Weir.The.Martian<- read.delim("Andy-Weir-The-Martian.csv", header = FALSE)
colnames(Andy.Weir.The.Martian)<- c("Score", "URL", "Review Title", "review text")
Andy.Weir.The.Martian$URL<- NULL
EL.James.Fifty.Shades.of.Grey<- read.delim("EL-James-Fifty-Shades-of-Grey.csv", header = FALSE)
colnames(EL.James.Fifty.Shades.of.Grey)<- c("Score", "URL", "Review Title", "review text")
EL.James.Fifty.Shades.of.Grey$URL<- NULL
Laura.Hillenbrand.Unbroken<- read.delim("Laura-Hillenbrand-Unbroken.csv", header = FALSE)
colnames(Laura.Hillenbrand.Unbroken)<- c("Score", "URL", "Review Title", "review text")
Laura.Hillenbrand.Unbroken$URL<- NULL
Donna.Tartt.The.Goldfinch<- read.delim("Donna-Tartt-The-Goldfinch.csv", header = FALSE)
colnames(Donna.Tartt.The.Goldfinch)<- c("Score", "URL", "Review Title", "review text")
Donna.Tartt.The.Goldfinch$URL<-NULL
Fillian.Flynn.Gone.Girl<- read.delim("Fillian_Flynn-Gone_Girl.csv", header = FALSE)
colnames(Fillian.Flynn.Gone.Girl)<- c("Score", "URL", "Review Title", "review text")
Fillian.Flynn.Gone.Girl$URL<-NULL
John.Green.The.Fault.in.our.Stars<- read.delim("John-Green-The-Fault-in-our-Stars.csv", header = FALSE)
colnames(John.Green.The.Fault.in.our.Stars)<- c("Score", "URL", "Review Title", "review text")
John.Green.The.Fault.in.our.Stars$URL<-NULL
Paula.Hawkins.The.Girl.On.The.Train<- read.delim("Paula_Hawkins-The-Girl-On-The-Train.csv", header = FALSE)
colnames(Paula.Hawkins.The.Girl.On.The.Train)<- c("Score", "URL", "Review Title", "review text")
Paula.Hawkins.The.Girl.On.The.Train$URL<-NULL
Suzanne.Collins.The.Hunger.Games<- read.delim("Suzanne-Collins-The-Hunger-Games.csv", header = FALSE)
colnames(Suzanne.Collins.The.Hunger.Games)<- c("Score", "URL", "Review Title", "review text")
Suzanne.Collins.The.Hunger.Games$URL<-NULL

# VCorpus only accepts files from a directory so they will need to be exported to a new set of files and read into the function
# need to adjust for which OS you are on
# mac_path <- "/Users/"
# pc_path <- "C:\\Users\\"
# full path is working dirrectory plus whatever system type you are working on
# full_path <- paste0("/Users/")...Mac Os
# full_path <- paste0("C:\\Users")...Windows Path

write.csv(Andy.Weir.The.Martian, paste0(full_path, "Andy.Weir.The.Martian.csv"), row.names=FALSE)
write.csv(EL.James.Fifty.Shades.of.Grey,  paste0(full_path, "EL.James.Fifty.Shades.of.Grey.csv"), row.names=FALSE)
write.csv(Laura.Hillenbrand.Unbroken,paste0(full_path, "Laura.Hillenbrand.Unbroken.csv"), row.names=FALSE)
write.csv(Donna.Tartt.The.Goldfinch,paste0(full_path, "Donna.Tartt.The.Goldfinch.csv"), row.names=FALSE)
write.csv(Fillian.Flynn.Gone.Girl,paste0(full_path, "Fillian.Flynn.Gone.Girl.csv"), row.names=FALSE)
write.csv(John.Green.The.Fault.in.our.Stars,paste0(full_path, "John.Green.The.Fault.in.our.Stars.csv"), row.names=FALSE)
write.csv(Paula.Hawkins.The.Girl.On.The.Train,paste0(full_path, "Paula.Hawkins.The.Girl.On.The.Train.csv"), row.names=FALSE)
write.csv(Suzanne.Collins.The.Hunger.Games,paste0(full_path, "Suzanne.Collins.The.Hunger.Games.csv"), row.names=FALSE)

# change the directory to the labeled_datasets dir by using the full_path variable
# /Users/labeled_datasets...Mac full_path
# C:\\Users\\labeled_datasets...Windows full_path
cname<- file.path(full_path)
cname
dir(cname)
# load the required libraries
library(tm)
library(SnowballC)
# Create the Corpus
docs <- VCorpus(DirSource(cname))
# Get rid of erroneous characters
for (j in seq(docs)) {
  docs[[j]]<- gsub("<.*?>", " ",docs[[j]])
  docs[[j]]<- gsub(",\"", " ",docs[[j]])
  docs[[j]]<- iconv(docs[[j]],from="UTF-8",to="ASCII",sub="")
  }
# Add custom stopwords
default_stopwords <- stopwords("english")
custom_stopwords <- c("read", "reading", "book", "books")
all_stopwords <- c(default_stopwords, custom_stopwords)

docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, all_stopwords)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
ds <- DirSource(".")
names(docs) <- ds$filelist
dtm <- DocumentTermMatrix(docs)
dtm <- removeSparseTerms(dtm, .99)
inspect(dtm)



# Create wordcloud
library(wordcloud)
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing = T)
head(v,50)
words <- names(v)
d <- data.frame(word=words, freq=v)
# cloud <- wordcloud(d$word, d$freq, min.freq = 5000)
cloud <- wordcloud(d$word, d$freq, min.freq = 5000,
                   max.words=500,
                   random.order = FALSE,
                   rot.per = 0.35,
                   colors=brewer.pal(8,"Dark2")
                   )

# Bar plot of top words
barplot(d[1:20,]$freq, las=2, names.arg = d[1:20,]$word,
        col = "lightblue", main = "Most frequent words all Ratings",
        ylab = "Word frequencies")


# k-means clustering 
install.packages("data.table")
library(data.table)
setwd(full_path)
dir()
csv_files <- list.files(pattern = "*.csv")

# Read each .csv file and store them in single data frames

data_frames <- lapply(csv_files, function(file) {
  read.csv(file, header = TRUE) 
})
for (i in seq_along(data_frames)) {
  data_frames[[i]]$doc_number <- i
}
rm(combined_df)

# Combine all the data frames into one
combined_data <- do.call(rbind, data_frames)
combined_data$Review.Title
combined_text <- paste(combined_data$Review.Title, combined_data$review.text, sep = " ")
library(tm)
# Create a corpus from the combined text
corpus_star <- Corpus(VectorSource(combined_text))
# Preprocess the corpus
default_stopwords <- stopwords("english")
# add custom stop words as needed to filter
custom_stopwords <- c("read", "reading", "book", "books", "classasizebase", "reviewtexti", "span", "reviewtextthis", "stars")
all_stopwords <- c(default_stopwords, custom_stopwords)

corpus_star <- tm_map(corpus_star, tolower)
corpus_star <- tm_map(corpus_star, removePunctuation)
corpus_star <- tm_map(corpus_star, removeNumbers)
corpus_star <- tm_map(corpus_star, removeWords, all_stopwords)
corpus_star <- tm_map(corpus_star, stripWhitespace)

# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus_star)
head(dtm$dimnames$Terms, 10)
dtm <- removeSparseTerms(dtm, .99)
term_frequencies <- rowSums(as.matrix(dtm))
dtm2 <- dtm[, names(tail(sort(colSums(as.matrix(dtm))), 500))]
findFreqTerms(dtm2 )

# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)
set.seed(123)



# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(dtm, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot to use the elbow method
library(tibble)
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)
library(ggplot2)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot


kmeansres <- kmeans(dtm, centers = 4)
# Get the cluster assignments
cluster_assignments <- kmeansres$cluster

# Plot the cluster plot
# install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeansres, data = dtm)

dtm$cluster <- kmeansres$cluster
dtm$cluster
kmeansres$tot.withinss


# Create a csv file to explore the data 
dtm_matrix <- as.matrix(dtm)
head(dtm$dimnames$Docs, 20)
output_file <- "matrix.csv"
write.csv(dtm_matrix, file = output_file)

m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing = T)
head(v,50)
words <- names(v)

dtmmatrix <- as.matrix(dtm)
dtm_df <- as.data.frame(dtmmatrix)
row.names(dtm_df) <- dtm$dimnames$Docs
unique(dtm$dimnames$Docs)
dtm_df <- cbind(dtm_df, cluster = dtm$cluster)
cluster_count_table <- table(dtm_df$cluster, row.names(dtm_df))
most_frequent_cluster <- as.numeric(ave(dtm_df$cluster, row.names(dtm_df), FUN = function(x) {
  tab <- table(x)
  names(tab)[which.max(tab)]
}))
result_df <- data.frame(Doc = rownames(cluster_count_table), Most_Frequent_Cluster = most_frequent_cluster)
result_df
output_file <- "cluster_doc.csv"
write.csv(result_df, file = output_file)


dtm_list <- lapply(unique(dtm$cluster), function(cluster) {
  cluster_corpus <- Corpus(VectorSource(corpus_star[dtm$cluster == cluster]))
  dtm_fun <- DocumentTermMatrix(cluster_corpus)
  return(dtm_fun)
})
most_common_words_per_cluster <- lapply(dtm_list, function(dtm) {
  freq <- colSums(as.matrix(dtm))
  top_words <- head(sort(freq, decreasing = TRUE), 10)
  return(names(top_words))
})
for (i in seq_along(most_common_words_per_cluster)) {
  cat("Cluster", i, ":", paste(most_common_words_per_cluster[[i]], collapse = ", "), "\n")
}

most_common_words_per_cluster
# create an empty list to store the word counts and clusters
top_word_counts_list <- list()
for (i in seq_along(dtm_list)) {
  dtm <- dtm_list[[i]]
  
  # Calculate word counts for each word in the DTM
  word_counts <- colSums(as.matrix(dtm))
  
  # Sort the word counts in descending order and select the top 10 words
  top_words <- names(head(sort(word_counts, decreasing = TRUE), 10))
  
  # Store the top word counts in the list
  top_word_counts_list[[i]] <- data.frame(word = top_words, count = word_counts[top_words])
}
word_freq_df <- do.call(rbind, top_word_counts_list)
word_freq_df
output_file <- "word_Sums.csv"
write.csv(word_freq_df, file = output_file)

# create a data frame with the word counts by cluster
sorted_cluster <- read.csv("word_Sums.csv")

# plot the counts by cluster
ggplot(sorted_cluster, aes(x = word, y = count, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Words", y = "Counts", title = "Word Counts by Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# top words per csv
csv_file <- "Suzanne.Collins.The.Hunger.Games.csv"
data3 <- read.csv(csv_file)
combined_text <- paste(data3$Review.Title, data3$review.text, sep = " ")
library(tm)

# Create a corpus from the combined text
corpus_star <- Corpus(VectorSource(combined_text))

# Preprocess the corpus
default_stopwords <- stopwords("english")

# add custom stop words as needed to filter
custom_stopwords <- c("read", "reading", "book", "books", "classasizebase", "reviewtexti", "span", "reviewtextthis", "stars")
all_stopwords <- c(default_stopwords, custom_stopwords)

corpus_star <- tm_map(corpus_star, tolower)
corpus_star <- tm_map(corpus_star, removePunctuation)
corpus_star <- tm_map(corpus_star, removeNumbers)
corpus_star <- tm_map(corpus_star, removeWords, all_stopwords)
corpus_star <- tm_map(corpus_star, stripWhitespace)

# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus_star)
dtm <- removeSparseTerms(dtm, .99)
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing = T)
head(v,50)
words <- names(v)
d <- data.frame(word=words, freq=v)


# bar plot of top words
title <- "Most Freq Hunger Games"
barplot(d[1:20,]$freq, las=2, names.arg = d[1:20,]$word,
        col = "lightblue", main = title,
        ylab = "Word frequencies")




