#
# stacked_bar_by_title.R
# July 29, 2023
#
#
library(dplyr)
library(ggplot2)

# Blake's working directory "C:\\Users\\blake\\OneDrive\\Documents\\UMUC\\DATA630\\GroupProject\\DATA_630_GROUP\\datasets"
# Blake's mac working directory "/Users/blakenicholson/Documents/development/DATA_630_GROU/datasets"
# Robert Nightingale's working directory (Windows 11)
working_directory <- "C:/Users/Bob/OneDrive/Documents/UMGC/Data 630/Group Project/datasets"
# Roy's Working Directory "/Users/royphelps/Library/CloudStorage/OneDrive-UMGC/Classes/Data 630 9040/Group Project/Data"

# need to adjust for which OS you are on
mac_path <- "/labeled_datasets/"

# full path is working dirrectory plus whatever system type you are working on
full_path <- paste0(working_directory, mac_path)


setwd(full_path)
getwd()


#Stacked bar of top words and their books
csv_files <- list.files(full_path, pattern = ".csv$", full.names = TRUE)

d <- data.frame(word=NULL, freq=NULL, title=NULL)

for (i in seq_along(csv_files)) {
 # i<-1  # for testing
  
  # top words per csv
  # csv_file <- "Suzanne.Collins.The.Hunger.Games.csv"
  csv_file <-   csv_files[i]
  
  csv_parts <- strsplit(csv_file,"[.]")
  lparts <- length (csv_parts[[1]])
  csv_title_parts <- csv_parts[[1]][3:(lparts-1)]
  csv_title <- paste0(csv_title_parts,collapse = " ")
  
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
  # head(v,50)
  words <- names(v)
  d <- rbind( data.frame(word=words, freq=v, title=csv_title), d)
}

# Sum freq count by word
word_sums <- aggregate(d$freq,by=list(d$word),FUN=sum)

# Sort sum by count
sort_sums <-arrange(word_sums,desc(x))

# Top 20 words
d_filt <- sort_sums[1:20,1]

# Filter of combined dataframe
d1 <- filter(d,word %in% d_filt)

ggplot(d1,aes(fill=title,x=reorder(word,-freq),y=freq)) +
  geom_bar(position='stack',stat='identity') +
  xlab("word") +
  ylab("frequency") +
  theme(axis.text.x=element_text(angle = 90
                                 , hjust = 1
                                 )) +
  ggtitle("Most frequent words by title \nall ratings") +
  theme(plot.title = element_text(hjust = 0.5))

