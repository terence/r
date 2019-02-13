

#most of the libraries needed
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(xml2)
library(knitr)
#library(kableExtra)


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


# ---------------------
# Some Plots
# ---------------------

#define some colors to use throughout

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



#look at the full data set at your disposal
prince %>%
  group_by(decade, chart_level) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() +
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = chart_level), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("All Songs in Data")

  ggsave("output/plot_allsongs_timeline1.png")

# --------------------------
# HTML Table output
# --------------------------
library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function
prince %>%
  filter(peak == "1") %>%
  select(year, song, peak) %>%
  arrange(year) %>%
  mutate(year = color_tile("lightblue", "lightgreen")(year)) %>%
  mutate(peak = color_tile("lightgreen", "lightgreen")(peak)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Prince's No. 1 Songs") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                  full_width = FALSE) %>%
  cat(., file="output/table1.html")
#  write_html(kable(dt, "html"), "output/table1.html")



# --------------------------
# Text Mining
# --------------------------

undesirable_words <- c("prince", "chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")

head(sample(stop_words$word, 15), 15)


#unnest and remove stop, undesirable and short words
prince_words_filtered <- prince %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)


class(prince_words_filtered)
dim(prince_words_filtered)


 prince_words_filtered %>% 
  filter(word == "race") %>%
  select(word, song, year, peak, decade, chart_level, charted) %>%
  arrange() %>%
  top_n(10,song) %>%
  mutate(song = color_tile("lightblue","lightblue")(song)) %>%
  mutate(word = color_tile("lightgreen","lightgreen")(word)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Tokenized Format Example") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                  full_width = FALSE) %>%
  cat(., file="output/table2.html")

# -----------
# Word Counts
# -------------------

full_word_count <- prince %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song,chart_level) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

full_word_count[1:10,] %>%
  ungroup(num_words, song) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(song = color_tile("lightpink","lightpink")(song)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Songs With Highest Word Count") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                  full_width = FALSE) %>%
  cat(., file="output/word_count1.html")

full_word_count %>%
  ggplot() +
    geom_histogram(aes(x = num_words, fill = chart_level )) +
    ylab("Song Count") + 
    xlab("Word Count per Song") +
    ggtitle("Word Count Distribution") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          panel.grid.minor.y = element_blank())

  ggsave("output/plot_wordcount1.png")


full_word_count %>%
  filter(chart_level == 'Top 10' & num_words > 800) %>%
  left_join(prince_orig, by = "song") %>%
  select(Song = song, 
         "Word Count" = num_words, 
         "Peak Position" = peak, 
         "US Pop" = US.Pop, 
         "US R&B" = US.R.B, 
         Canada = CA, 
         Ireland = IR) %>%
  kable("html", escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"))

prince_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
    geom_col(aes(word, n), fill = my_colors[4]) +
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank()) +
    xlab("") + 
    ylab("Song Count") +
    ggtitle("Most Frequently Used Words in Prince Lyrics") +
    coord_flip()

  ggsave("output/plot_topwords1.png")


# -----------
# Word Cloud Generation
# -----------
#install.packages("webshot")
#webshot::install_phantomjs()
#prince_words_counts <- prince_words_filtered %>%
#  count(word, sort = TRUE) 
#pwc <- wordcloud2(prince_words_counts[1:300, ], size = .5)
#saveWidget(pwc,"output/wordcloud1.html",selfcontained = F)
#webshot::webshot("output/wordcloud1.html","1.png",vwidth = 1992, vheight = 1744, delay =10)


# ------
# Popular Words
# ------

popular_words <- prince_words_filtered %>% 
  group_by(chart_level) %>%
  count(word, chart_level, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(chart_level,n) %>%
  mutate(row = row_number()) 

popular_words %>%
  ggplot(aes(row, n, fill = chart_level)) +
    geom_col(show.legend = NULL) +
    labs(x = NULL, y = "Song Count") +
    ggtitle("Popular Words by Chart Level") + 
    theme_lyrics() +  
    facet_wrap(~chart_level, scales = "free") +
    scale_x_continuous(  # This handles replacement of row 
      breaks = popular_words$row, # notice need to reuse data frame
      labels = popular_words$word) +
    coord_flip()


  ggsave("output/plot_popularwords1.png")

# -------
# Timeless Words
# ------


timeless_words <- prince_words_filtered %>% 
  filter(decade != 'NA') %>%
  group_by(decade) %>%
  count(word, decade, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(decade,n) %>%
  mutate(row = row_number()) 

timeless_words %>%
  ggplot(aes(row, n, fill = decade)) +
    geom_col(show.legend = NULL) +
    labs(x = NULL, y = "Song Count") +
    ggtitle("Timeless Words") + 
    theme_lyrics() +  
    facet_wrap(~decade, scales = "free", ncol = 5) +
    scale_x_continuous(  # This handles replacement of row 
      breaks = timeless_words$row, # notice need to reuse data frame
      labels = timeless_words$word) +
    coord_flip()

  ggsave("output/plot_timelesswords1.png")


#unnest and remove undesirable words, but leave in stop and short words
prince_word_lengths <- prince %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song,decade) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  mutate(word_length = nchar(word)) 

prince_word_lengths %>%
  count(word_length, sort = TRUE) %>%
  ggplot(aes(word_length), 
         binwidth = 10) + 
    geom_histogram(aes(fill = ..count..),
                   breaks = seq(1,25, by = 2), 
                   show.legend = FALSE) + 
    xlab("Word Length") + 
    ylab("Word Count") +
    ggtitle("Word Length Distribution") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank())

  ggsave("output/plot_wordlength1.png")

# ---------
# Lex Diversity
# --------

lex_diversity_per_year <- prince %>%
  filter(decade != "NA") %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song,year) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) 

diversity_plot <- lex_diversity_per_year %>%
  ggplot(aes(year, lex_diversity)) +
    geom_point(color = my_colors[3],
               alpha = .4, 
               size = 4, 
               position = "jitter") + 
    stat_smooth(color = "black", se = FALSE, method = "lm") +
    geom_smooth(aes(x = year, y = lex_diversity), se = FALSE,
                color = "blue", lwd = 2) +
    ggtitle("Lexical Diversity") +
    xlab("") + 
    ylab("") +
    scale_color_manual(values = my_colors) +
    theme_classic() + 
    theme_lyrics()

#  ggsave("output/plot_wordlength1.png")
diversity_plot


# --------
# Lex Density
# ---------

lex_density_per_year <- prince %>%
  filter(decade != "NA") %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song,year) %>%
  summarise(lex_density = n_distinct(word)/n()) %>%
  arrange(desc(lex_density))

density_plot <- lex_density_per_year %>%
  ggplot(aes(year, lex_density)) + 
    geom_point(color = my_colors[4],
               alpha = .4, 
               size = 4, 
               position = "jitter") + 
    stat_smooth(color = "black", 
                se = FALSE, 
                method = "lm") +
    geom_smooth(aes(x = year, y = lex_density), 
                se = FALSE,
                color = "blue", 
                lwd = 2) +
    ggtitle("Lexical Density") + 
    xlab("") + 
    ylab("") +
    scale_color_manual(values = my_colors) +
    theme_classic() + 
    theme_lyrics()

density_plot








