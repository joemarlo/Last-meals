
library(tidyverse)
library(httr)
library(stringdist)
library(rvest)    
library(stringr)
library(tidytext)
library(SnowballC)
library(igraph)
library(ggraph)
data(stop_words)

US.table <- read_csv("Data/US_last_meals.csv")



# gg theme ----------------------------------------------------------------

theme_custom <- function() {
  theme_gray() +
    theme(
      panel.grid.minor.y = element_line(color = NA),
      panel.grid.major.y = element_line(color = "gray95"),
      panel.grid.minor.x = element_line(color = NA),
      panel.grid.major.x = element_line(color = "gray95"),
      panel.background = element_rect(fill = NA),
      plot.background = element_rect(
        fill = NA,
        color = "gray95",
        size = 10
      ),
      plot.margin = unit(c(1, 1, 1, 1), "cm"),
      axis.title = element_text(color = "gray30"),
      axis.ticks = element_line(color = NA),
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(
        color = "gray30",
        size = 11,
        face = "bold"
      ),
      plot.title = element_text(color = "gray30",
                                face = "bold"),
      plot.subtitle = element_text(size = 10,
                                   color = "gray30"),
      text = element_text(family = "Helvetica")
    )
}

theme_set(theme_custom())




# create ngrams -----------------------------------------------------------

parsed.words <- US.table %>%
  select(Name, Requested.Meal) %>% 
  unnest_tokens(output = word, input = Requested.Meal) %>% 
  anti_join(stop_words, by = "word")

# top words
parsed.words %>% 
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>% 
  tail(n = 20)

# top words
parsed.words %>% 
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>% 
  top_n(n = 15, wt = n) %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = n)) +
  geom_col() +
  scale_fill_gradient(low = "#0b2919", high = "#2b7551") +
  labs(title = "Most common words within last meal requests",
       x = NULL,
       y = "Count") +
  coord_flip() +
  theme(legend.position = "none")

ggsave(filename = "Plots/unigrams.svg",
       plot = last_plot(),
       device = "svg",
       width = 8,
       height = 7)
```

# stemming ----------------------------------------------------------------

# tokenize the words skipping ngrams
#   this allows multiple words per token so we include
#   food like "fried chicken", not just "chicken"
parsed.ngrams <- US.table %>%
  select(Name, Requested.Meal) %>% 
  unnest_tokens(output = word, input = Requested.Meal, token = "ngrams", n = 3, n_min = 1) %>% 
  anti_join(stop_words, by = "word")

# EXAMPLE
enframe('butter pecan ice cream') %>%
  unnest_tokens(
    output = word,
    input = value,
    token = "ngrams",
    n = 3,
    n_min = 1
  )

# food base ---------------------------------------------------------------

# Download foodbase from here: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6827550/
# Pull out the ingredients which will act as comparison for our foods
food.words <- read_csv("Data/food_words.csv") %>%
  pull() %>%
  str_to_lower()

# remove condiments to the food words list
excl.words <- c("meal", "food", "snack", "drink", "double", "ketchup", "cups", "pecan",
                "mustard", "mayonnaise", "mayo", "sauce", "sour cream", "fried", "onion",
                "onions", "pepper", "ranch", "ranch dressing", "meat", "butter")
food.words <- food.words[!(food.words %in% excl.words)]

# add Coke
food.words <- append(food.words, c("coke", "pepsi"))

parsed.ngrams <- parsed.ngrams %>% 
  filter(word %in% food.words)
         # stem %in% wordStem(food.words, language = 'english'))
head(parsed.ngrams)

# EXAMPLE
enframe('butter pecan ice cream') %>%
  unnest_tokens(
    output = word,
    input = value,
    token = "ngrams",
    n = 3,
    n_min = 1
  ) %>% 
  filter(word %in% food.words)
  

# double counts -----------------------------------------------------------

# The issue now is that we're double counting some items. Ice, cream, and ice cream are the most obvious. We can remove this duplicates on an inmate basis by excluding words that exist in longer strings. E.g. exclude "ice" if there is another string like "ice cream" but keep "ice cream."

# split the $word with more than one word into a list
#  then split the dataframe by $Name
# remove unrelated words
name.groups <- parsed.ngrams %>% 
  mutate(word = str_split(word, pattern = " ")) %>% 
  group_by(Name) %>% 
  group_split()

# check to see if the $word is contained within 
#  another $word for that $Name
deduped.ngrams <- lapply(name.groups, function(group) {
  
  # first remove $words that are more than two
  #  individual words (e.g. "chocolate ice cream") b/c
  #  these will be captured in "ice cream"
  group <- group %>%
    rowwise() %>%
    filter(length(word) <= 2) %>% 
    ungroup()
  
  # if only one unique word then return that one word
  if (length(unique(group$word)) == 1) {
    non.duplicates <- group$word[1]
  } else{
    # for groups that have more than one row check to 
    #   see if a word is contained in another row
    duplicate.bool <-
      sapply(1:length(group$word), function(i) {
        x <- group$word[i]
        lst <- group$word
        lst <- lst[!(lst %in% x)]
        word.in.list <- sapply(lst, function(y) {
          x %in% y
        })
        return(sum(word.in.list) == 0)
      })
    
    non.duplicates <- group$word[duplicate.bool]
  }
  return(group %>% filter(word %in% non.duplicates))
}) %>% bind_rows()
rm(name.groups)

# unlist the word column
deduped.ngrams <- deduped.ngrams %>% 
  rowwise() %>% 
  mutate(word = paste0(word, collapse = " ")) %>% 
  ungroup()

# n removed rows
nrow(parsed.ngrams) - nrow(deduped.ngrams)


# stemming
deduped.ngrams <- deduped.ngrams %>% 
  mutate(stem = wordStem(word, language = 'english'))


# check to see if there are any similar foods that are duplicates and should be edited
get_top_matches <- function(current.word, words.to.match, n = 5){
  # function returns that top n matches of the current.name
  #   within the names.to.match list via fuzzy string matching
  
  scores <- stringsim(current.word, words.to.match, method = "osa")
  words.to.match[rev(order(scores))][1:(n + 1)]
}

# test the function
get_top_matches(deduped.ngrams$stem[1], unique(deduped.ngrams$stem))

# apply the function across the entire list to generate a data.frame
#  containing the current.name and it's top 5 best matches
lapply(deduped.ngrams$stem,
       get_top_matches,
       words.to.match = unique(deduped.ngrams$stem)) %>% 
  unlist() %>% 
  matrix(ncol = 6, byrow = TRUE) %>% 
  as_tibble() %>% 
  setNames(c("Current.word", paste0("Match.", 1:5))) %>% 
  head()

# get most common word for each stem
unique.stem.word.pairs <- deduped.ngrams %>% 
  select(stem, word) %>%
  group_by(stem, word) %>% 
  summarize(n = n()) %>% 
  group_by(stem) %>% 
  filter(n == max(n)) %>% 
  select(-n)

# plot the total counts of stems, but use the word as the label
final.freq.plot <- deduped.ngrams %>% 
  group_by(stem) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(n = 10, wt = n) %>% 
  left_join(unique.stem.word.pairs) %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = n)) +
  geom_col() +
  scale_fill_gradient(low = "#0b2919", high = "#2b7551") +
  labs(title = "Top 10 most common items in last meal requests",
       subtitle = "Data from 130 U.S. inmates since 1927",
       x = NULL,
       y = "Count") +
  coord_flip() +
  theme(legend.position = "none")

final.freq.plot

ggsave(filename = "Plots/Common_last_meals.svg",
       plot = last_plot(),
       device = "svg",
       width = 8,
       height = 7)


top.five <- deduped.ngrams %>% 
  group_by(stem) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(n = 5, wt = n) %>% 
  left_join(unique.stem.word.pairs) %>% 
  pull(word)


deduped.ngrams %>% 
  group_by(Name) %>% 
  summarize(match = max(word %in% top.five)) %>% 
  summarize(mean(match))

deduped.ngrams %>% 
  group_by(word) %>%
  summarize(n = n()) %>% 
  filter(word %in% top.five) %>% 
  summarize(sum(n))



# cosine ------------------------------------------------------------------

cosine_matrix <- function(tokenized_data, lower = 0, upper = 1, filt = 0) {
  # ruthlessly stolen from https://www.markhw.com/blog/word-similarity-graphs
  
  if (!all(c("word", "Name") %in% names(tokenized_data))) {
    stop("tokenized_data must contain variables named word and id")
  }
  
  if (lower < 0 | lower > 1 | upper < 0 | upper > 1 | filt < 0 | filt > 1) {
    stop("lower, upper, and filt must be 0 <= x <= 1")
  }
  
  docs <- length(unique(tokenized_data$Name))
  
  out <- tokenized_data %>%
    count(Name, stem) %>%
    group_by(stem) %>%
    mutate(n_docs = n()) %>%
    ungroup() %>%
    filter(n_docs < (docs * upper) & n_docs > (docs * lower)) %>%
    select(-n_docs) %>%
    mutate(n = 1) %>%
    spread(stem, n, fill = 0) %>%
    select(-Name) %>%
    as.matrix() %>%
    lsa::cosine()
  
  filt <- quantile(out[lower.tri(out)], filt)
  out[out < filt] <- diag(out) <- 0
  out <- out[rowSums(out) != 0, colSums(out) != 0]
  
  return(out)
}

cos_mat <- cosine_matrix(deduped.ngrams, lower = .035,
                         upper = .90, filt = .75)
head(cos_mat)


# There are two clear communities here. In the top left section, "home-style" food such as mashed potatoes, gravy, tea, peas, and rice all naturally go together. In the center section, hamburger, onion rings, fried chicken, and steakare also naturally grouped. Intiutively, cheese is stuck right in the middle of these two groups. I interpret this as it's the great equalizer, [almost everyone loves cheese](link). It's important to view this graph a few different times with various random seeds. There's many correct ways to visualize the same graph and sometimes you can draw the wrong conclusions from the visualization so it's important to take the above conclusions with some reservation.

set.seed(22)
cosine.graph <- graph_from_adjacency_matrix(cos_mat, 
                                            mode = "undirected", 
                                            weighted = TRUE) %>%
  ggraph(layout = 'nicely') +
  geom_edge_link(aes(alpha = weight),
                 show.legend = FALSE,
                 color = "#2b7551") + 
  geom_node_label(aes(label = name),
                  label.size = 0.1,
                  size = 3,
                  color = "#0b2919")
cosine.graph


ggsave(filename = "Plots/last_meals_graph.svg",
       plot = last_plot(),
       device = "svg",
       width = 8,
       height = 7)


# save 20  different graphs then convert to a single gif via imagemagick shell
setwd("Plots/gif")
set.seed(22)
for (i in 1:20L){
  graph_from_adjacency_matrix(cos_mat,
                              mode = "undirected",
                              weighted = TRUE) %>%
    ggraph(layout = 'nicely') +
    geom_edge_link(aes(alpha = weight),
                   show.legend = FALSE,
                   color = "#2b7551") +
    geom_node_label(aes(label = name),
                    label.size = 0.1,
                    size = 3,
                    color = "#0b2919")

  ggsave(filename = paste0(i, ".png"),
         plot = last_plot(),
         device = "png",
         width = 8,
         height = 7)
}
system("convert -delay 80 *.png last_meals_graph.gif") # this calls imagemagik via shell
file.remove(list.files(pattern = ".png"))
setwd(normalizePath('../..'))


## Fast food references
fast.food.names <- c('KFC', 'McDonald', 'Big Mac', 'whopper', 'Burger King', 'Pizza Hut', 'Domino', 'Coke', 'Pepsi', 'Coca-Cola',  'Dr. Pepper')


