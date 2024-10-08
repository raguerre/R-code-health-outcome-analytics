## Project: Health Outcomes Analytics SPH ##
## Source: PMR Operational Main Dataset - PowerBi
## September 2024 ##

# Install packages & lib -------------------------------------------------------
install.packages(c("dplyr","wordcloud","RColorBrewer","tm","SnowballC","reshape2","psych","readr"))
install.packages(c("NLP", "magrittr","tidytuesdayR","tidyverse"))
install.packages("pastecs")
install.packages("plyr")
#devtools::install_github("lepennec/ggwordcloud")

install.packages("ggplot2")
install.packages("ggwordcloud")
install.packages("pastecs")
install.packages("vangogh")

library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(reshape2)
library(psych)
library(readr)
library(pastecs)

library(NLP)
library(magrittr)
library(tidytuesdayR)
library(tidyverse)
library(pastecs)
library(plyr)
library(ggplot2)
library(ggwordcloud)
library(vangogh)

#library(showtext)
#library(scales)
#library(MetBrewer)
#library(sp)
library(sysfonts)

## Check system & version
#Sys.which("R")
#version

### Keywords - Load data -------------------------------------------------------

# Path & load file
path <- setwd("C:/Users/ncber/OneDrive/Documentos/R-code-health-outcome-analytics")
file <- "/input/db_key_words2.xlsx" #Tab: Operation Profile
keywords <- readxl::read_excel(paste0(path,file,sep=""))

## Wordcloud verbs ##
text_verb <- as.character(keywords$verbs)
text_verb <- na.omit(text_verb)

### Create a text corpus, define transformation functions, and apply them to corpus
corpus <- Corpus(VectorSource(text_verb))

to_lower <- content_transformer(tolower)
remove_punct <- content_transformer(function(x) removePunctuation(x))
remove_nums <- content_transformer(function(x) removeNumbers(x))

corpus <- tm_map(corpus, to_lower) 
corpus <- tm_map(corpus, remove_punct)
corpus <- tm_map(corpus, remove_nums)
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
#corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
#corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))

inspect(corpus) # Check the cleaned corpus
tdm <- TermDocumentMatrix(corpus) # Create a term-document matrix
matrix <- as.matrix(tdm) # Convert the term-document matrix to a matrix
word_freqs <- sort(rowSums(matrix), decreasing = TRUE) # Calculate word frequencies
word_freqs_df <- data.frame(word = names(word_freqs), # Convert to data frame for wordcloud
                            freq = word_freqs, stringsAsFactors = FALSE) 

word_freqs_df <- word_freqs_df %>%
  filter(word!="character")  # Exclude the word

set.seed(42)
cloud_verbs <- ggplot(word_freqs_df, aes(label = word, size = freq, color=freq)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 60) +
  theme_minimal(base_size = 20, base_family = "Montserrat") +
  scale_color_gradient(low = "#0f3057", high = "#428C5C")  # Light blue to navy
 
plot(cloud_verbs)

## Start Bar plot 
font_add_google(name = "Roboto Mono", family = "Roboto")
font_add_google(name = "Bebas Neue", family = "Bebas")
font <- "Roboto"
font_t <- "Bebas"

plot_verbs <- ggplot(keywords) +
  aes(x = `verbs`) +
  geom_bar(stat = "count") +
  stat_count(geom = "text", colour="white", size=3.5,
             aes(label=..count..), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 30, hjust = 0, face = "bold", color = "#000000", margin = margin(b = 5)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#000000", lineheight = 1.2, margin = margin(b = 20)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 30)),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = font, color = "#000000"),
        axis.text.x = element_text(family = font, size = 9, color = "#000000", margin = margin(t = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Most frequent verbs",
       subtitle = "Verbs in both English and Spanish most used in SPH operations",
       caption = "Data: PMR Operation Main Dataset | Cohort: September 2024")

plot(plot_verbs)
ggsave(paste0("Operations Type_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 10, height = 8)

## Wordcloud Direct Object ##
text_verb <- as.character(keywords$direct_object)
text_verb <- na.omit(text_verb)

### Create a text corpus, define transformation functions, and apply them to corpus
corpus <- Corpus(VectorSource(text_verb))

to_lower <- content_transformer(tolower)
remove_punct <- content_transformer(function(x) removePunctuation(x))
remove_nums <- content_transformer(function(x) removeNumbers(x))

corpus <- tm_map(corpus, to_lower) 
corpus <- tm_map(corpus, remove_punct)
corpus <- tm_map(corpus, remove_nums)
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
#corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
#corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))

inspect(corpus) # Check the cleaned corpus
tdm <- TermDocumentMatrix(corpus) # Create a term-document matrix
matrix <- as.matrix(tdm) # Convert the term-document matrix to a matrix
word_freqs <- sort(rowSums(matrix), decreasing = TRUE) # Calculate word frequencies
word_freqs_df <- data.frame(word = names(word_freqs), # Convert to data frame for wordcloud
                            freq = word_freqs, stringsAsFactors = FALSE) 

word_freqs_df <- word_freqs_df %>%
  filter(word!="character")  # Exclude the word

set.seed(42)
cloud_verbs <- ggplot(word_freqs_df, aes(label = word, size = freq, color=freq)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 60) +
  theme_minimal(base_size = 20, base_family = "Montserrat") +
  scale_color_gradient(low = "#0f3057", high = "#428C5C")  # Light blue to navy

plot(cloud_verbs)

## Bar Plot Direct Object 
font_add_google(name = "Roboto Mono", family = "Roboto")
font_add_google(name = "Bebas Neue", family = "Bebas")
font <- "Roboto"
font_t <- "Bebas"

plot_dir_obj <- ggplot(keywords) +
  aes(x = `direct_object`) +
  geom_bar(stat = "count") +
  stat_count(geom = "text", colour="white", size=3.5,
             aes(label=..count..), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 30, hjust = 0, face = "bold", color = "#000000", margin = margin(b = 5)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#000000", lineheight = 1.2, margin = margin(b = 20)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 30)),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = font, color = "#000000"),
        axis.text.x = element_text(family = font, size = 9, color = "#000000", margin = margin(t = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Most frequent words",
       subtitle = "Direct objects in both English and Spanish most used in SPH operations",
       caption = "Data: PMR Operation Main Dataset | Cohort: September 2024")

plot(plot_dir_obj)


## Wordcloud Indirect Object ##
text_verb <- as.character(keywords$indirect_object)
text_verb <- na.omit(text_verb)

### Create a text corpus, define transformation functions, and apply them to corpus
corpus <- Corpus(VectorSource(text_verb))

to_lower <- content_transformer(tolower)
remove_punct <- content_transformer(function(x) removePunctuation(x))
remove_nums <- content_transformer(function(x) removeNumbers(x))

corpus <- tm_map(corpus, to_lower) 
corpus <- tm_map(corpus, remove_punct)
corpus <- tm_map(corpus, remove_nums)
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
#corpus <- tm_map(corpus, removeWords, stopwords("spanish"))

inspect(corpus) # Check the cleaned corpus
tdm <- TermDocumentMatrix(corpus) # Create a term-document matrix
matrix <- as.matrix(tdm) # Convert the term-document matrix to a matrix
word_freqs <- sort(rowSums(matrix), decreasing = TRUE) # Calculate word frequencies
word_freqs_df <- data.frame(word = names(word_freqs), # Convert to data frame for wordcloud
                            freq = word_freqs, stringsAsFactors = FALSE) 

word_freqs_df <- word_freqs_df %>%
  filter(word!="character")  # Exclude the word

set.seed(42)
cloud_verbs <- ggplot(word_freqs_df, aes(label = word, size = freq, color=freq)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 60) +
  theme_minimal(base_size = 20, base_family = "Montserrat") +
  scale_color_gradient(low = "#0f3057", high = "#428C5C")  # Light blue to navy

plot(cloud_verbs)

## Bar Plot Direct Object 
font_add_google(name = "Roboto Mono", family = "Roboto")
font_add_google(name = "Bebas Neue", family = "Bebas")
font <- "Roboto"
font_t <- "Bebas"

plot_dir_obj <- ggplot(keywords) +
  aes(x = `direct_object`) +
  geom_bar(stat = "count") +
  stat_count(geom = "text", colour="white", size=3.5,
             aes(label=..count..), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 30, hjust = 0, face = "bold", color = "#000000", margin = margin(b = 5)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#000000", lineheight = 1.2, margin = margin(b = 20)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 30)),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = font, color = "#000000"),
        axis.text.x = element_text(family = font, size = 9, color = "#000000", margin = margin(t = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Most frequent words",
       subtitle = "Direct objects in both English and Spanish most used in SPH operations",
       caption = "Data: PMR Operation Main Dataset | Cohort: September 2024")

plot(plot_dir_obj)


### Database with revised outcomes ### -----------------------------------------

# Path & load file
path <- setwd("C:/Users/ncber/OneDrive/Documentos/R-code-health-outcome-analytics")
file <- "/input/db_key_words3.xlsx" #Tab: Operation Profile
keywords <- readxl::read_excel(paste0(path,file,sep=""))

file <- "/input/profiles.csv" #Tab: Operation Profile
profiles <-readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) # From tableau
profiles <-profiles[with(profiles,order(`Project Number`,`Cycle ID`)),]

# Leave the record of the first Convergence (oldest) cycle only
profiles$`Cycle ID`<-as.integer(profiles$`Cycle ID`)
profiles$min.cycle <-ave(profiles$`Cycle ID`,profiles$`Project Number`,FUN = min)
summary(profiles$`Cycle ID`)
profiles <- profiles[profiles$`Cycle ID`==profiles$min.cycle,]
profiles$min.cycle  <-NULL

# Keep only health
profiles <- profiles[grepl("HEALTH | e-HEALTH",profiles$Sector,ignore.case = T),]

p<-g.ind[c(1,2,6,12,15,18,25:30)]


