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
    count(Name, word) %>%
    group_by(word) %>%
    mutate(n_docs = n()) %>%
    ungroup() %>%
    filter(n_docs < (docs * upper) & n_docs > (docs * lower)) %>%
    select(-n_docs) %>%
    mutate(n = 1) %>%
    spread(word, n, fill = 0) %>%
    select(-Name) %>%
    as.matrix() %>%
    lsa::cosine()
  
  filt <- quantile(out[lower.tri(out)], filt)
  out[out < filt] <- diag(out) <- 0
  out <- out[rowSums(out) != 0, colSums(out) != 0]
  
  return(out)
}


# new ---------------------------------------------------------------------

sentence.1 <- 'paper'
sentence.2 <- 'pizzas, ice cream, and coffee, apples'

parsed.ngrams <- tribble(~Name, ~Sentence,
                         'Sentence.1', sentence.1,
                         'Sentence.2', sentence.2) %>% 
  unnest_tokens(output = word, input = Sentence, token = "skip_ngrams") %>% 
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word, language = 'english')) %>%
  filter(word %in% food.words,
         stem %in% wordStem(food.words, language = 'english')) 

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

word.counts <- deduped.ngrams %>% 
  count(Name, stem) %>%
  mutate(n = min(1, n)) %>% 
  group_by(stem) %>% 
  pivot_wider(names_from = stem,
              values_from = n,
              values_fill = list(n = 0)) %>%
  select(-Name) %>% 
  as.matrix() %>% 
  t()

cos.measure <- lsa::cosine(word.counts[,1], word.counts[,2])
degree.angle <- REdaS::rad2deg(acos(cos.measure))
# cos(REdaS::deg2rad(degree.angle))

# plot the angle
tibble(x = cos(acos(cos.measure)), y = sin(acos(cos.measure))) %>% 
  ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = 'grey80') +
  geom_segment(aes(x = 0, y = 0, xend = x, yend = y),
               arrow = arrow(type = 'closed', length = unit(0.5, "cm")),
               alpha = 0.7) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0)) +
  geom_point(aes(x = 1, y = 0), size = 2) +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  coord_fixed(xlim = 0:1, ylim = 0:1) +
  labs(title = "Cosine similarity projected onto the unit circle",
       subtitle = paste0('Angle of ', round(degree.angle, 1), ' degrees'),
       x = NULL,
       y = NULL)


  