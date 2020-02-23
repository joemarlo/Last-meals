library(tidyverse)
library(stringdist)
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

# create unigrams and remove stop words
parsed.words <- US.table %>%
  select(Name, Requested.Meal) %>% 
  unnest_tokens(output = word, input = Requested.Meal) %>% 
  anti_join(stop_words, by = "word")

# view the top words
parsed.words %>% 
  group_by(word) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>% 
  tail(n = 20)

# plot the top words
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

# ggsave(filename = "Plots/unigrams.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 8,
#        height = 7)


# tokenzing ----------------------------------------------------------------

# tokenize uni, bi, and tri-grams so can include
#   food like "fried chicken", not just "chicken"
parsed.ngrams <- US.table %>%
  select(Name, Requested.Meal) %>% 
  unnest_tokens(output = word, input = Requested.Meal, token = "ngrams", n = 3, n_min = 1) %>% 
  anti_join(stop_words, by = "word")

# EXAMPLE for blog plost
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
excl.words <- c("meal", "food", "snack", "drink", "double", "ketchup", "cups", "cigar",
                "mustard", "mayonnaise", "mayo", "sauce", "sour cream", "fried", "onion",
                "onions", "pepper", "ranch", "ranch dressing", "meat", "butter")
food.words <- food.words[!(food.words %in% excl.words)]

# add Coke
food.words <- append(food.words, c("coke", "pepsi"))

# filter the ngrams to food words
parsed.ngrams <- parsed.ngrams %>% 
  filter(word %in% food.words)

head(parsed.ngrams)

# EXAMPLE for blog plost
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

# The issue now is that we're double counting some items. Ice, cream, and 
#   ice cream are the most obvious. We can remove this duplicates on an 
#   inmate basis by excluding words that exist in longer strings. E.g. 
#   exclude "ice" if there is another string like "ice cream" but keep "ice cream."

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
    
    # remove $words that are more than two
    #  individual words (e.g. "chocolate ice cream") b/c
    #  these will be captured in "ice cream"
    non.duplicates <- non.duplicates[sapply(non.duplicates, length) <= 2]
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

# steming the words
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

# ggsave(filename = "Plots/Common_last_meals.svg",
#        plot = final.freq.plot,
#        device = "svg",
#        width = 8,
#        height = 7)

# find the top five most popular food items
top.five <- deduped.ngrams %>% 
  group_by(stem) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(n = 5, wt = n) %>% 
  left_join(unique.stem.word.pairs) %>% 
  pull(word)

# calculate the proportion of meals these five show up in
deduped.ngrams %>% 
  group_by(Name) %>% 
  summarize(match = max(word %in% top.five)) %>% 
  summarize(mean(match))

# count the number of meals these five show up in 
deduped.ngrams %>% 
  group_by(word) %>%
  summarize(n = n()) %>% 
  filter(word %in% top.five) %>% 
  summarize(sum(n))



# cosine ------------------------------------------------------------------

cosine_matrix <- function(tokenized_data, lower = 0, upper = 1, filt = 0) {
  # ruthlessly stolen from https://www.markhw.com/blog/word-similarity-graphs
  
  if (!all(c("stem", "Name") %in% names(tokenized_data))) {
    stop("tokenized_data must contain variables named stem and Name")
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

# calculate the cosine of our ngrams grouped by Name
cos_mat <- cosine_matrix(deduped.ngrams, lower = .035,
                         upper = .90, filt = .75)
cos_mat[1:5, 1:5]

# graph our cosine matrix
set.seed(44)
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

# ggsave(filename = "Plots/last_meals_graph.svg",
#        plot = cosine.graph,
#        device = "svg",
#        width = 8,
#        height = 7)


# repeat the previous graph 20 times 
#  save each graph and then convert to a single gif via imagemagick shell
# setwd("Plots/gif")
# set.seed(44)
# for (i in 1:20L){
#   graph_from_adjacency_matrix(cos_mat,
#                               mode = "undirected",
#                               weighted = TRUE) %>%
#     ggraph(layout = 'nicely') +
#     geom_edge_link(aes(alpha = weight),
#                    show.legend = FALSE,
#                    color = "#2b7551") +
#     geom_node_label(aes(label = name),
#                     label.size = 0.1,
#                     size = 3,
#                     color = "#0b2919")
# 
#   ggsave(filename = paste0(i, ".png"),
#          plot = last_plot(),
#          device = "png",
#          width = 8,
#          height = 7)
# }
# system("convert -delay 80 *.png last_meals_graph.gif") # this calls imagemagik via shell
# file.remove(list.files(pattern = ".png"))
# setwd(normalizePath('../..'))



# state analysis ----------------------------------------------------------

# count of observations by state
US.table %>% 
  count(State) %>% 
  ggplot(aes(x = reorder(State, -n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL,
       y = 'Count')

# Northeast
MidAtlantic <- c('New Jersey', 'New York', 'Pennsylvania')
NewEngland <- c('Connecticut', 'Maine', 'Massachusetts', 'New Hampshire',
                'Rhode Island', 'Vermont')
NE <- tibble(Division = 'MidAtlantic',
       State = MidAtlantic) %>% 
  bind_rows(tibble(Division =  'NewEngland',
                   State = NewEngland)) %>% 
  mutate(Region = 'Northeast')

# Midwest
ENCentral <- c('Indiana', 'Illinois', 'Michigan',
               'Ohio', 'Wisconsin')
WNCentral <- c('Iowa', 'Kansas', 'Minnesota', 'Missouri',
               'Nebraska', 'North Dakota', 'South Dakota')
MW <- tibble(Division = 'ENCentral',
             State = ENCentral) %>% 
  bind_rows(tibble(Division =  'WNCentral',
                   State = WNCentral)) %>% 
  mutate(Region = 'Midwest')

# South
SAtlantic <- c('Delaware',  'District of Columbia', 'Florida',
               'Georgia', 'Maryland', 'North Carolina',
               'South Carolina', 'Virginia', 'West Virginia')
ESCentral <- c('Alabama', 'Kentucky', 'Mississippi', 'Tennessee')
WSCentral <- c('Arkansas', 'Louisiana', 'Oklahoma', 'Texas')
South <- tibble(Division = 'SAtlantic',
             State = SAtlantic) %>% 
  bind_rows(tibble(Division =  'ESCentral',
                   State = ESCentral)) %>%
  bind_rows(tibble(Division = 'WSCentral',
                   State = WSCentral)) %>% 
  mutate(Region = 'South')

# West
Mountain <- c('Arizona', 'Colorado', 'Idaho', 'New Mexico',
              'Montana', 'Utah', 'Nevada', 'Wyoming')
Pacific <- c('Alaska', 'California', 'Hawaii', 'Oregon', 'Washington')
West <- tibble(Division = 'Mountain',
             State = Mountain) %>% 
  bind_rows(tibble(Division =  'Pacific',
                   State = Pacific)) %>% 
  mutate(Region = 'West')

# bind all the rows togther. final dataframe is list of all states with 
#  their division and region
regions <- bind_rows(NE, MW, South, West)

# add region to the last meals data frame
state.regions <- US.table %>% 
  mutate(State = case_when(
    State == 'Washington state' ~ 'Washington',
    State == 'Washington State' ~ 'Washington',
    TRUE ~ State)) %>% 
  left_join(regions, by = 'State')

# count of most common regions
state.regions %>% 
  count(Region) %>%
  mutate(Region = recode(Region,  'Other' = 'NA')) %>%
  ggplot(aes(x = reorder(Region, -n), y = n, fill = n)) +
  geom_col() +
  scale_fill_gradient(low = "#0b2919", high = "#2b7551") +
  coord_flip() +
  labs(x = NULL,
       y = 'Count') +
  theme(legend.position = "none")

# repeat the cosine process but group by region instead of Name
#  I also did this for state and division but it didn't work well
parsed.ngrams <- state.regions %>% 
  select(Region, Requested.Meal) %>% 
  unnest_tokens(output = word, input = Requested.Meal, token = "ngrams", n = 3, n_min = 1) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(word %in% food.words)

# split the $word with more than one word into a list
#  then split the dataframe by $Name
# remove unrelated words
state.groups <- parsed.ngrams %>% 
  mutate(word = str_split(word, pattern = " ")) %>% 
  group_by(Region) %>% 
  group_split()

# check to see if the $word is contained within 
#  another $word for that $Name
deduped.ngrams <- lapply(state.groups, function(group) {
  
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
    
    # remove $words that are more than two
    #  individual words (e.g. "chocolate ice cream") b/c
    #  these will be captured in "ice cream"
    non.duplicates <- non.duplicates[sapply(non.duplicates, length) <= 2]
  }
  return(group %>% filter(word %in% non.duplicates))
}) %>% bind_rows()
rm(state.groups)

# unlist the word column and stem
deduped.ngrams <- deduped.ngrams %>% 
  rowwise() %>% 
  mutate(word = paste0(word, collapse = " ")) %>% 
  ungroup() %>% 
  mutate(stem = wordStem(word, language = 'english'))

# get most common word for each stem
unique.stem.word.pairs <- deduped.ngrams %>% 
  select(stem, word) %>%
  group_by(stem, word) %>% 
  summarize(n = n()) %>% 
  group_by(stem) %>% 
  filter(n == max(n)) %>% 
  select(-n)

# plot the total counts of stems, but use the word as the label
#  faceted by region
region.freq.plot <- deduped.ngrams %>% 
  filter(!is.na(Region)) %>% 
  group_by(Region) %>% 
  mutate(region.n = n()) %>% 
  ungroup() %>% 
  mutate(Region = paste0(Region, ', n = ', region.n)) %>% 
  group_by(Region, stem) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(n = 5, wt = n) %>% 
  left_join(unique.stem.word.pairs) %>% 
  ggplot(aes(x = reorder_within(word, n, within = Region), y = n, fill = n)) +
  geom_col() +
  scale_x_reordered() +
  scale_fill_gradient(low = "#0b2919", high = "#2b7551") +
  labs(title = "Top five most common items in last meal requests",
       subtitle = "Data from 130 U.S. inmates since 1927",
       x = NULL,
       y = "Count") +
  coord_flip() +
  theme(legend.position = "none") +
  facet_wrap(~Region, scales = 'free_y')

region.freq.plot
# ggsave(filename = "Plots/last_meals_region.svg",
#        plot = region.freq.plot,
#        device = "svg",
#        width = 8,
#        height = 7)


# cosine graph
set.seed(22)
deduped.ngrams %>%
  rename(Name = Region) %>%
  cosine_matrix(lower = .12,
                upper = .80,
                filt = .6) %>%
  graph_from_adjacency_matrix(mode = "undirected",
                              weighted = TRUE) %>%
  ggraph(layout = 'nicely') +
  geom_edge_link(aes(alpha = weight),
                 show.legend = FALSE,
                 color = "#2b7551") +
  geom_node_label(aes(label = name),
                  label.size = 0.1,
                  size = 3,
                  color = "#0b2919"
  )


# fast food ---------------------------------------------------------------
fast.food.names <- c('KFC', "McDonald's", 'Big Mac', 'whopper', 'Burger King', 
                     'Pizza Hut', "Domino's", 'Coke', 'Pepsi', 'Coca Cola',  'Dr Pepper') %>% 
  str_to_lower()

# plot of top brands
US.table %>%
  select(Name, Requested.Meal) %>% 
  unnest_tokens(output = word, input = Requested.Meal, token = "ngrams", n = 3, n_min = 1) %>% 
  anti_join(stop_words, by = "word") %>%
  filter(word %in% fast.food.names) %>% 
  mutate(word = recode(word, 'coca cola' = 'coke')) %>% 
  count(word) %>% 
  ggplot(aes(x = reorder(word, n), y = n, fill = n)) +
  geom_col() +
  scale_fill_gradient(low = "#0b2919", high = "#2b7551") +
  labs(title = "Top brands in last meal requests",
       subtitle = "Data from 130 U.S. inmates since 1927",
       x = NULL,
       y = "Count") +
  coord_flip() +
  theme(legend.position = "none")

# ggsave(filename = "Plots/top_brands.svg",
#        plot = last_plot(),
#        device = "svg",
#        width = 8,
#        height = 7)

