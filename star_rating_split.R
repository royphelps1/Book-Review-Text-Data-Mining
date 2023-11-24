library(dplyr)
library(readr)
library(tidyr)

# Directory for the original files based on OS
# full_path is from the pre-processing script. make sure to run that first
mac_path <- "/"
pc_path_sep <- "\\"
# choose whichever OS you are on in the next line
directory <- paste0(full_path, mac_path)

# Get the list of CSV files in the directory

csv_files <- list.files(directory, pattern = ".csv$", full.names = TRUE)
#check the list to ensure all are there
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
