

#most of the libraries needed
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations


prince_orig <- read.csv("data/prince_raw_data.csv", stringsAsFactors = FALSE)
names(prince_orig)

prince <- prince_orig %>% 
  select(lyrics = text, song, year, album, peak, 
         us_pop = US.Pop, us_rnb = US.R.B)

glimpse(prince[139,])

dim(prince)

str(prince[139, ]$lyrics, nchar.max = 300)

# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}



# fix (expand) contractions
print ("Fix Contractions\n\n")
prince$lyrics <- sapply(prince$lyrics, fix.contractions)


# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
prince$lyrics <- sapply(prince$lyrics, removeSpecialChars)


# convert everything to lower case
prince$lyrics <- sapply(prince$lyrics, tolower)

str(prince[139, ]$lyrics, nchar.max = 300)

#get facts about the full dataset
summary(prince)


#create the decade column
prince <- prince %>%
  mutate(decade = 
           ifelse(prince$year %in% 1978:1979, "1970s", 
           ifelse(prince$year %in% 1980:1989, "1980s", 
           ifelse(prince$year %in% 1990:1999, "1990s", 
           ifelse(prince$year %in% 2000:2009, "2000s", 
           ifelse(prince$year %in% 2010:2015, "2010s", 
                  "NA"))))))

#create the chart level column
prince <- prince %>%
  mutate(chart_level = 
           ifelse(prince$peak %in% 1:10, "Top 10", 
           ifelse(prince$peak %in% 11:100, "Top 100", "Uncharted")))


#create binary field called charted showing if a song hit the charts at all
prince <- prince %>%
  mutate(charted = 
           ifelse(prince$peak %in% 1:100, "Charted", "Uncharted"))

#save the new dataset to .csv for use in later tutorials
write.csv(prince, file = "data/prince_new.csv")



#define some colors to use throughout
print ("Plot it")

my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

prince %>%
  filter(decade != "NA") %>%
  group_by(decade, charted) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = charted), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Released Songs") +
  labs(x = NULL, y = "Song Count")
  ggsave("output/plot_charted1.png")


prince %>%
  filter(decade != "NA") %>%
  group_by(decade, charted) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = charted), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Released Songs") +
  labs(x = NULL, y = "Song Count")

  ggsave("output/plot_top1.png")








