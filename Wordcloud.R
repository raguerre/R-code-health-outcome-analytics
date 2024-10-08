## Word Cloud & Freq words ##
## Project: Health outcomen Analytics SPH ##
## August 2024 ##

# Install packages & lib -------------------------------------------------------
install.packages(c("dplyr","wordcloud","RColorBrewer","tm","SnowballC","reshape2","psych","readr"))
install.packages(c("NLP", "magrittr","tidytuesdayR","tidyverse"))

library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(reshape2)
library(psych)
library(readr)

library(NLP)
library(magrittr)
library(tidytuesdayR)
library(tidyverse)

#library(showtext)
#library(scales)
#library(MetBrewer)
#library(sp)
#library(sysfonts)

## Check system & version
#Sys.which("R")
#version

# Draft --------------------------------------------------------------------
path <- setwd("C:/Users/ncber/OneDrive/Documentos/R-code-health-outcome-analytics")
file <- "/source_data.csv"
data <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE)

text_data <-data$outcomes
head(data$outcomes)
text_data <- as.character(text_data)

# Convert text data to UTF-8 encoding
#text_data <- iconv(text_data, from = "unknown", to = "UTF-8")
text_data <- iconv(text_data, from = "LATIN2", to = "UTF-8")
head(text_data)

# Remove any non-printable characters
text_data <- gsub("[[:cntrl:]]", "", text_data)
text_data <- na.omit(text_data)

# Create a text corpus, define transformation functions, and apply them to corpus
corpus <- Corpus(VectorSource(text_data))

to_lower <- content_transformer(tolower)
remove_punct <- content_transformer(function(x) removePunctuation(x))
remove_nums <- content_transformer(function(x) removeNumbers(x))

corpus <- tm_map(corpus, to_lower) 
corpus <- tm_map(corpus, remove_punct)
corpus <- tm_map(corpus, remove_nums)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))

# Inspect corpus before stripWhitespace
inspect(corpus)

# Remove empty documents before applying stripWhitespace
#corpus <- corpus[sapply(corpus, function(doc) {
#  nchar(content(doc)) > 0
#})]

# Apply stripWhitespace
strip_ws <- content_transformer(stripWhitespace)
corpus <- tm_map(corpus, strip_ws)

# Check the cleaned corpus
inspect(corpus)

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Convert the term-document matrix to a matrix
matrix <- as.matrix(tdm)

# Calculate word frequencies
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)

# Convert to data frame for wordcloud
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs, stringsAsFactors = FALSE)

# Generate the word cloud with adjusted parameters
set.seed(123)
wordcloud(words = word_freqs_df$word, freq = word_freqs_df$freq,
          min.freq = 2,                      # Increase min.freq to filter out less frequent words
          max.words = 200,                   # Limit the number of words to display
          scale = c(3, 0.5),                 # Adjust scale for word sizes
          rot.per = 0.1,
          colors = brewer.pal(8,"Dark2"))  # Adjust colors as needed

png(filename = "wordcloud_large.png", width = 1200, height = 1000) 


brewer.pal.info
display.brewer.all()
display.brewer.pal()

# Check the distribution of word frequencies
summary(word_freqs_df$freq)

# Inspect the top few words
head(word_freqs_df[order(-word_freqs_df$freq), ])

# Plot a barplot of the top words
top_words <- head(word_freqs_df[order(-word_freqs_df$freq), ], 30)
barplot(top_words$freq, names.arg = top_words$word, las = 2,
        col = brewer.pal(8, "Dark2"), main = "Top 30 Words",
        xlab = "Words", ylab = "Frequency")
plot(barplot)

# Second try -------------------------------------------------------------------
# load libraries
install.packages("RColorBrewer")
install.packages("textTinyR")
install.packages("langdetect")
install.packages("text")

library(text)
library(tidyverse)
library(showtext)
library(janitor)
library(sysfonts)
library(RColorBrewer)
library(textTinyR)
library(langdetect)

# add font
font_add_google(name="Sigmar One", family="Sigmar One")
font_t<-"Sigmar One"

font_add_google(name="Hind", family="Hind")
font<-"Hind"

text_data <-g.ind$objectives
head(g.ind$objectives)
text_data <- as.character(text_data)

text_data <- na.omit(text_data)

# Create a text corpus, define transformation functions, and apply them to corpus
corpus <- Corpus(VectorSource(text_data))

to_lower <- content_transformer(tolower)
remove_punct <- content_transformer(function(x) removePunctuation(x))
remove_nums <- content_transformer(function(x) removeNumbers(x))

corpus <- tm_map(corpus, to_lower) 
corpus <- tm_map(corpus, remove_punct)
corpus <- tm_map(corpus, remove_nums)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))

inspect(corpus) # Check the cleaned corpus
tdm <- TermDocumentMatrix(corpus) # Create a term-document matrix
matrix <- as.matrix(tdm) # Convert the term-document matrix to a matrix
word_freqs <- sort(rowSums(matrix), decreasing = TRUE) # Calculate word frequencies
word_freqs_df <- data.frame(word = names(word_freqs), # Convert to data frame for wordcloud
                            freq = word_freqs, stringsAsFactors = FALSE) 

set.seed(42)
cloud <- ggplot(word_freqs_df, aes(label = word, size = freq)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 50) +
  theme_minimal(base_size = 30, base_family = "Montserrat") 

cloud + theme_light() 
plot(cloud)

ggsave(paste0("Cloud-all_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 10, height = 8)

#Spanish
install.packages("cld2")
library(cld2)

text_data_df<-data.frame(text_data)

detect_language <- function(text) {
  result <- cld2::detect_language(text)
  result$language
}

text_data$language <- sapply(text_data_df$text, detect_language)

  
language_detection <- text::textLanguage(text_data$objectives)
text_data$language <- language_detection$language

corpus <- tm_map(corpus, to_lower) 
corpus <- tm_map(corpus, remove_punct)
corpus <- tm_map(corpus, remove_nums)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))

inspect(corpus) # Check the cleaned corpus
tdm <- TermDocumentMatrix(corpus) # Create a term-document matrix
matrix <- as.matrix(tdm) # Convert the term-document matrix to a matrix
word_freqs <- sort(rowSums(matrix), decreasing = TRUE) # Calculate word frequencies
word_freqs_df <- data.frame(word = names(word_freqs), # Convert to data frame for wordcloud
                            freq = word_freqs, stringsAsFactors = FALSE) 

set.seed(42)
cloud <- ggplot(word_freqs_df, aes(label = word, size = freq)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 50) +
  theme_minimal(base_size = 30, base_family = "Montserrat") 

cloud + theme_dark() 
plot(cloud)




#scale_color_gradient(palette = "Set1")
scale_color_brewer(palette = "Dark2") 
#theme_minimal()

windowsFonts()

library(extrafont)
font_import()
loadfonts(device = "win")

font_add_google("Montserrat", "Montserrat")
font_add_google("Roboto", "Roboto")

myFont1 <- "Montserrat"
plot(x)
print(x)

png(filename = "wordcloud_large.png", width = 1200, height = 1000) 

#eccentricity = 1

# Apply stripWhitespace
strip_ws <- content_transformer(stripWhitespace)
corpus <- tm_map(corpus, strip_ws)

# Generate the word cloud with adjusted parameters
set.seed(123)
wordcloud(words = word_freqs_df$word, freq = word_freqs_df$freq,
          min.freq = 2,                      # Increase min.freq to filter out less frequent words
          max.words = 200,                   # Limit the number of words to display
          scale = c(3, 0.5),                 # Adjust scale for word sizes
          rot.per = 0.1,
          colors = brewer.pal(8,"Dark2"))  # Adjust colors as needed

summary(word_freqs_df$freq) # Check the distribution of word frequencies
head(word_freqs_df[order(-word_freqs_df$freq), ]) # Inspect the top few words

# Plot a barplot of the top words
top_words <- head(word_freqs_df[order(-word_freqs_df$freq), ], 30)
barplot(top_words$freq, names.arg = top_words$word, las = 2,
        col = brewer.pal(8, "Dark2"), main = "Top 30 Words",
        xlab = "Words", ylab = "Frequency")

# Plot by category
health <- data %>%
  filter(health %in% c("TRUE"))


