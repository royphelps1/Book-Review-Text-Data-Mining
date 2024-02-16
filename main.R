# ~~~ Main ~~~

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", 
            "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

setwd("C:/Users/")


# ~~~ READ THE FILES INTO A FRAME ~~~
# Each column has to be labeled


#1
Andy.Weir.The.Martian<- read.delim("Andy-Weir-The-Martian.csv", header = FALSE)
colnames(Andy.Weir.The.Martian)<- c("Score", "URL", "Review Title", "review text")
Andy.Weir.The.Martian$URL<- NULL
Andy.Weir.The.Martian$Score<-factor(Andy.Weir.The.Martian$Score)


#2
EL.James.Fifty.Shades.of.Grey<- read.delim("EL-James-Fifty-Shades-of-Grey.csv", header = FALSE)
colnames(EL.James.Fifty.Shades.of.Grey)<- c("Score", "URL", "Review Title", "review text")
EL.James.Fifty.Shades.of.Grey$URL<- NULL
EL.James.Fifty.Shades.of.Grey$Score<-factor(EL.James.Fifty.Shades.of.Grey$Score)


#3
Laura.Hillenbrand.Unbroken<- read.delim("Laura-Hillenbrand-Unbroken.csv", header = FALSE)
colnames(Laura.Hillenbrand.Unbroken)<- c("Score", "URL", "Review Title", "review text")
Laura.Hillenbrand.Unbroken$URL<- NULL
Laura.Hillenbrand.Unbroken$Score<-factor(Laura.Hillenbrand.Unbroken$Score)


#4
Donna.Tartt.The.Goldfinch<- read.delim("Donna-Tartt-The-Goldfinch.csv", header = FALSE)
colnames(Donna.Tartt.The.Goldfinch)<- c("Score", "URL", "Review Title", "review text")
Donna.Tartt.The.Goldfinch$URL<-NULL
Donna.Tartt.The.Goldfinch$Score<-factor(Donna.Tartt.The.Goldfinch$Score)


#5
Fillian.Flynn.Gone.Girl<- read.delim("Fillian_Flynn-Gone_Girl.csv", header = FALSE)
colnames(Fillian.Flynn.Gone.Girl)<- c("Score", "URL", "Review Title", "review text")
Fillian.Flynn.Gone.Girl$URL<-NULL
Fillian.Flynn.Gone.Girl$Score<-factor(Fillian.Flynn.Gone.Girl$Score)


#6
John.Green.The.Fault.in.our.Stars<- read.delim("John-Green-The-Fault-in-our-Stars.csv", header = FALSE)
colnames(John.Green.The.Fault.in.our.Stars)<- c("Score", "URL", "Review Title", "review text")
John.Green.The.Fault.in.our.Stars$URL<-NULL
John.Green.The.Fault.in.our.Stars$Score<-factor(John.Green.The.Fault.in.our.Stars$Score)


#7
Paula.Hawkins.The.Girl.On.The.Train<- read.delim("Paula_Hawkins-The-Girl-On-The-Train.csv", header = FALSE)
colnames(Paula.Hawkins.The.Girl.On.The.Train)<- c("Score", "URL", "Review Title", "review text")
Paula.Hawkins.The.Girl.On.The.Train$URL<-NULL
Paula.Hawkins.The.Girl.On.The.Train$Score<-factor(Paula.Hawkins.The.Girl.On.The.Train$Score)


#8
Suzanne.Collins.The.Hunger.Games<- read.delim("Suzanne-Collins-The-Hunger-Games.csv", header = FALSE)
colnames(Suzanne.Collins.The.Hunger.Games)<- c("Score", "URL", "Review Title", "review text")
Suzanne.Collins.The.Hunger.Games$URL<-NULL
Suzanne.Collins.The.Hunger.Games$Score<-factor(Suzanne.Collins.The.Hunger.Games$Score)


# VCorpus only accepts files from a directory so they will need to be exported to a new set of files and read into the function

write.csv(Andy.Weir.The.Martian,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\Andy.Weir.The.Martian.csv", row.names=FALSE)
write.csv(EL.James.Fifty.Shades.of.Grey,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\EL.James.Fifty.Shades.of.Grey.csv", row.names=FALSE)
write.csv(Laura.Hillenbrand.Unbroken,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\Laura.Hillenbrand.Unbroken.csv", row.names=FALSE)
write.csv(Donna.Tartt.The.Goldfinch,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\Donna.Tartt.The.Goldfinch.csv", row.names=FALSE)
write.csv(Fillian.Flynn.Gone.Girl,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\Fillian.Flynn.Gone.Girl.csv", row.names=FALSE)
write.csv(John.Green.The.Fault.in.our.Stars,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\John.Green.The.Fault.in.our.Stars.csv", row.names=FALSE)
write.csv(Paula.Hawkins.The.Girl.On.The.Train,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\Paula.Hawkins.The.Girl.On.The.Train.csv", row.names=FALSE)
write.csv(Suzanne.Collins.The.Hunger.Games,"C:\\Users\\Admin-PC\\Desktop\\DATA630\\Group Project//labled datasets\\Suzanne.Collins.The.Hunger.Games.csv", row.names=FALSE)

# Plot the distribution of reviews
all_books <- bind_rows(Andy.Weir.The.Martian = Andy.Weir.The.Martian, 
                       Donna.Tartt.The.Goldfinch = Donna.Tartt.The.Goldfinch, 
                       EL.James.Fifty.Shades.of.Grey = EL.James.Fifty.Shades.of.Grey,
                       Fillian.Flynn.Gone.Girl = Fillian.Flynn.Gone.Girl, 
                       John.Green.The.Fault.in.our.Stars = John.Green.The.Fault.in.our.Stars, 
                       Laura.Hillenbrand.Unbroken = Laura.Hillenbrand.Unbroken,
                       Paula.Hawkins.The.Girl.On.The.Train = Paula.Hawkins.The.Girl.On.The.Train, 
                       Suzanne.Collins.The.Hunger.Games = Suzanne.Collins.The.Hunger.Games, 
                       .id = 'Book')

ggplot(all_books, aes(fill = Book, x = Score))+
  geom_bar(position = "dodge", stat = "count")+
  ylab("Number of Reviews")+
  xlab("Stars Given")+
  ggtitle("Distribution of Reviews")


# ~~~ Text Analysis ~~~
library(tm)
library(caTools)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(BiocManager)
library(Rgraphviz)
cname<- file.path("C:/Users/Admin-PC/Desktop/DATA630/Group Project/labled datasets")
cname
dir(cname)

# 1 Pre-processing
# Create stop words vectors
default_stopwords <- stopwords("english")
custom_stopwords <- c("read", "reading", "book", "books", "classasizebase", "reviewtexti", "span", "reviewtextthis")
all_stopwords <- c(default_stopwords, custom_stopwords)

# Remove stop words,punctuation and numbers
corpus_books<-Corpus(DirSource(cname))
corpus_books <- tm_map(corpus_books, tolower)
corpus_books <- tm_map(corpus_books, removePunctuation)
corpus_books <- tm_map(corpus_books, removeNumbers)
corpus_books <- tm_map(corpus_books, removeWords, all_stopwords)
corpus_books <- tm_map(corpus_books, stripWhitespace)
corpus_books <- tm_map(corpus_books, stemDocument)
dtm$cluster
dtm_books<-DocumentTermMatrix(corpus_books[2])

# remove sparse terms
dtm_books<-removeSparseTerms(dtm_books, 0.2)

wf <- data.frame(docs=Docs(dtm_books), as.matrix(dtm_books))

wf<- wf %>% gather(key = "word", value = "n", -docs)

wf2<- wf[order(wf$n,decreasing = TRUE),] %>%
  top_n(50)


  ggplot(wf2, aes( x = reorder(word, -n), y =n)) + 
  geom_bar(stat="identity") +
  facet_wrap(~ docs) + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

# Term document matrices with TF-IDF Weight
tdm_books<-TermDocumentMatrix(corpus_books, control = list(weighting = weightTfIdf))
inspect(tdm_books)
freq = rowSums(as.matrix(tdm_books))
head(freq,10)
tail(freq,10)
tail(sort(freq), n=20)

top20.freq = tail(sort(freq), n = 20)
top20.freq.df=as.data.frame(sort(top20.freq))
top20.freq.df$names <- rownames(top20.freq.df)

ggplot(top20.freq.df, aes(reorder(names,top20.freq), top20.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")

# Term document matrices with TF-IDF Weight
dtm_books2<-DocumentTermMatrix(corpus_books, control = list(weighting = weightTfIdf))
inspect(dtm_books2)
freq2 = rowSums(as.matrix(dtm_books2))
head(freq,10)
tail(freq,10)
tail(sort(freq), n=20)

top20.freq = tail(sort(freq), n = 20)
top20.freq.df=as.data.frame(sort(top20.freq))
top20.freq.df$names <- rownames(top20.freq.df)

ggplot(top20.freq.df, aes(reorder(names,top20.freq), top20.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")
library(dplyr)
library(readr)
library(tidyr)

# Directory for the original files based on OS
# full_path is from the pre-processing script. Make sure to run that first.
mac_path <- "/"
pc_path_sep <- "\\"

# Choose whichever OS you are on in the next line
directory <- paste0(full_path, mac_path)

# Get the list of CSV files in the directory

csv_files <- list.files(directory, pattern = ".csv$", full.names = TRUE)
# Check the list to ensure all are there
csv_files

# Create a list to store the dataframes for each score
star_ratings <- vector("list", 5)

# Iterate over each CSV file
for (file in csv_files) {
  # Read the CSV file into a dataframe
  df <- read_csv(file)
  
  # Group the dataframe by Score column
  grouped <- df %>% group_by(Score)
  
  # Split the data into separate dataframes based on star ratings
  split_dfs <- grouped %>% group_split()
  
  # Merge the split dataframes into the star_ratings list
  for (i in seq_along(split_dfs)) {
    if (is.null(star_ratings[[i]])) {
      star_ratings[[i]] <- split_dfs[[i]]
    } else {
      star_ratings[[i]] <- bind_rows(star_ratings[[i]], split_dfs[[i]])
    }
  }
}

# Write each star rating dataframe to a separate CSV file
for (i in seq_along(star_ratings)) {
  write_csv(star_ratings[[i]], paste0("star_rating_", i, ".csv"))
}

# evaluate each star rating
# This function can be modified to look at what ever star rating you want
# Just change the number in the quotes below. Make sure the csv files are in the working directory
star_rating <- "1"

file_name <- paste0("star_rating_", star_rating, ".csv")
# Load the CSV file
data <- read.csv(file_name, stringsAsFactors = FALSE)

# Combine the text from both text columns into a single document
combined_text <- paste(data$Review.Title, data$review.text, sep = " ")
head(combined_text)

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

# create wordcloud
library(wordcloud)
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing = T)
head(v,50)
words <- names(v)
words
d <- data.frame(word=words, freq=v)

# change the min.freq to adjust how many words in the cloud. Higher freq yields fewer words
cloud <- wordcloud(d$word, d$freq, min.freq = 2500)


plot_title <- paste0("Most frequent words ",star_rating," Star Rating" )
#bar plot of top words
barplot(d[1:20,]$freq, las=2, names.arg = d[1:20,]$word,
        col = "lightblue", main = plot_title,
        ylab = "Word frequencies")


# K-means clustering
library(dplyr)
library(ggplot2)
dtm2 <- dtm[, names(tail(sort(colSums(as.matrix(dtm))), 500))]
inspect(dtm2)

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
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot


kmeansres <- kmeans(dtm, centers = 3)
# Get the cluster assignments
cluster_assignments <- kmeansres$cluster

# plot the cluster plot
library(factoextra)
fviz_cluster(kmeansres, data = dtm)
kmeansres$size


