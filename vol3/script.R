# Load libraries ---------------------------------------------------------------

library(dplyr)
library(tidytext)
library(ggplot2)

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

# Convert to tidytext format
data(stop_words)

merged_tweets %>%
  # Tokenise
  group_by(source) %>%
  unnest_tokens(word, content) %>%
  ungroup() %>%
  # Remove stop words
  anti_join(stop_words) %>%
  # Calculate term frequency
  group_by(word) %>%
  count(word, sort = TRUE) %>%
  ungroup() %>%
  # Remove hashtags
  filter(!word %in% c("ssntrade", "ssnsignings")) %>%
  # Remove URL parts
  filter(!word %in% c("https", "t.co")) %>%
  # Remove words we expect to be very frequent
  filter(!word %in% c("netball", "ssn")) %>%
  # Sentiment analysis with bing lexicon
  inner_join(get_sentiments("bing")) %>%
  # Isolate to terms occurring at least 10 times
  filter(n >= 10) %>%
  mutate(word = reorder(word, n)) -> tidy_tweets
  # Remove references to team / league Twitter handles
  # filter(!word %in% c(
  #  "adelaidetbirds", "collingwoodsn", "firebirdsqld", "giants_netball",
  #  "melbournevixens", "nswswifts", "sc_lightning", "supernetball",
  #  "westcoastfever"))

# Plot n-gram frequency and sentiment ------------------------------------------

p <- ggplot(tidy_tweets, aes(x = n, y = word, fill = sentiment))
p <- p + geom_bar(stat = "identity")
p <- p + labs(
  x = NULL, y = NULL)
p <- p + theme_minimal()
p <- p + theme(
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_blank(),
  legend.position = "bottom")

# Convert from tidytext to DTM format ------------------------------------------

# cast_dtm()
