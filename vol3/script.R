# Load libraries ---------------------------------------------------------------

library(dplyr)
library(tidytext)
library(showtext)
library(ggplot2)
library(topicmodels)
library(ggtext)

# Import data ------------------------------------------------------------------

public_tweets <- readr::read_csv(
  "https://raw.githubusercontent.com/aaronsfox/netball-numbers-challenge/master/datasets/vol3/somethingAboutRiddlesAndFruit.csv",
  col_types = c("cTccfnnnnfcccc"))

team_tweets <- readr::read_csv(
  "https://raw.githubusercontent.com/aaronsfox/netball-numbers-challenge/master/datasets/vol3/somethingAboutRiddlesAndFruit_teamTweets.csv",
  col_types = c("cTccfnnnnfcccc"))

# Tidying ----------------------------------------------------------------------

# Merge source data sets
public_tweets %>%
  mutate(source = "public_tweets") -> public_tweets_2

team_tweets %>%
  mutate(source = "team_tweets") -> team_tweets_2

bind_rows(public_tweets_2, team_tweets_2) %>%
  select(source, everything()) %>%
  # Remove duplicate tweets
  mutate(is_dup = duplicated(id)) %>%
  filter(is_dup == FALSE) %>%
  select(-is_dup) -> merged_tweets

# Identify most frequent posters
merged_tweets %>%
  group_by(username) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  slice_max(n, n = 6) -> top_posters

top_posters_vec <- top_posters$username

# Convert to tidytext format
data(stop_words)

merged_tweets %>%
  # Filter to top 6 posters
  filter(username %in% top_posters_vec) %>%
  # Tokenise
  group_by(username) %>%
  unnest_tokens(word, content) %>%
  ungroup() %>%
  select(username, word) %>%
  # Remove stop words
  anti_join(stop_words) %>%
  # Calculate term frequency
  group_by(username, word) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  # Remove hashtags
  filter(!word %in% c("ssntrade", "ssnsignings")) %>%
  # Remove URL parts
  filter(!word %in% c("https", "t.co")) %>%
  # Remove words we expect to be very frequent
  filter(!word %in% c("netball", "ssn", "supernetball")) %>%
  # Remove references to team / league Twitter handles
  filter(!word %in% c(
    "adelaidetbirds", "collingwoodsn", "firebirdsqld", "giants_netball",
    "melbournevixens", "nswswifts", "sc_lightning", "supernetball",
    "westcoastfever")) -> tidy_tweets

tidy_tweets %>%
  # Sentiment analysis with bing lexicon
  inner_join(get_sentiments("bing")) %>%
  # Top 3 terms per poster
  group_by(username) %>%
  slice_max(n, n = 3) %>%
  ungroup() %>%
  mutate(
    username = paste0("@", username),
    word = reorder(word, n),
    sentiment = factor(
      sentiment,
      levels = c("positive", "negative"))) -> tidy_tweets_with_sentiment
  
# Plot n-gram frequency and sentiment ------------------------------------------

# Import fonts from Google Fonts
font_add_google("Roboto Condensed", "roboto_con")
font_add_google("Nixie One", "nixie")

showtext_auto()

p <- ggplot(
  tidy_tweets_with_sentiment,
  aes(x = n, y = word, fill = sentiment, group = sentiment))
p <- p + facet_wrap(~username, ncol = 3, scales = "free_y")
p <- p + geom_bar(stat = "identity")
p <- p + scale_fill_manual(
  values = c("positive" = "#4a8bad",
             "negative" = "#ad4a8b"))
p <- p + labs(
  title = "Sentiment salad: Top 6 Twitter posters during the #SSNTrade period",
  subtitle = "Sentiment lexicon: bing",
  x = NULL, y = NULL,
  caption = "**Data source:** @aaron_s_fox // **Plot:** @jacquietran")
p <- p + theme_minimal()
p <- p + theme(
  text = element_text(family = "roboto_con", size = 24),
  plot.background = element_rect(fill = "#E3E9EC"),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_line(colour = "#82929A", linetype = "dotted"),
  panel.grid.major.y = element_blank(),
  plot.title = element_text(family = "nixie"),
  plot.subtitle = element_text(family = "nixie", margin=margin(0,0,15,0)),
  plot.caption = element_markdown(size = NULL),
  legend.position = "bottom")

ggsave(here::here("vol3/sentiment_plot.png"),
       last_plot(), width = 14, height = 10, units = "cm", dpi = 300)

showtext_auto(FALSE)

# Convert from tidytext to DTM format ------------------------------------------

tidy_tweets %>%
  cast_dtm(source, word, n) -> tidy_tweets_dtm

# Topic modelling --------------------------------------------------------------

tidy_tweets_lda <- LDA(tidy_tweets_dtm, k = 2, control = list(seed = 1234))

ssn_topics <- tidy(tidy_tweets_lda, matrix = "beta")

ssn_top_terms <- ssn_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ssn_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()