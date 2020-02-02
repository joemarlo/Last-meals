library(tidyverse)
library(httr)
library(rvest)    
library(stringr)


# scrape the table of last meals ------------------------------------------

## Sources
# https://pastebin.com/TiERhrpg  
# https://en.wikipedia.org/wiki/Last_meal  
# https://henryhargreaves.com/No-Seconds  


# scrape the table
tables <- read_html('https://en.wikipedia.org/wiki/Last_meal') %>% 
  html_nodes(xpath = '//table[contains(@class, "sortable")]') %>% 
  html_children()

# pull the US table
US.table <- tables[4] %>%
  html_children() %>%
  html_text() %>%
  matrix(ncol = 1, byrow = TRUE) %>%
  as_tibble() %>% 
  separate(
    V1,
    into = c("Name", "Crime", "State", "Year",
             "Method.of.Dispatch", "Requested.Meal",
             "tmp", "tmp2", "tmp3", "tmp4", "tmp5"),
    sep = "\n"
  )

# fix row 35
US.table[35, "Crime"] <- US.table[35, "State"]
US.table[35, "State"] <- US.table[35, "Method.of.Dispatch"]
US.table[35, "Year"] <- US.table[35, "tmp"]
US.table[35, "Method.of.Dispatch"] <- US.table[35, "tmp3"]
US.table[35, "Requested.Meal"] <- US.table[35, "tmp5"]
# US.table[35,]

# remove extra columns and first row
US.table <- select(US.table, -contains("tmp"))
US.table <- US.table[-1,]

# standardize the method of dispatch descriptions
US.table$Method.of.Dispatch <- str_to_sentence(US.table$Method.of.Dispatch)
# table(US.table$Method.of.Dispatch)

US.table$Method.of.Dispatch <- recode(
  US.table$Method.of.Dispatch,
  "Electric chair" = "Electrocution",
  "Electrocution, photographed in the electric chair" = "Electrocution",
  "Hanged, last person executed in public in the us" = "Hanging",
  "Lethal injection. Clemency appeal based on his being too overweight for lethal injection drugs to work on him properly" = "Lethal injection"
)

# convert Year to numeric
US.table$Year <- as.numeric(US.table$Year)

# We need to remove observations of inmates that didn't request a
#   meal or received a meal that was not requested. We're interested 
#   in what people *wanted* as their last meal.
excl.names <- c("David Mason", "Odell Barnes", "Philip Workman")
US.table <- US.table[!(US.table$Name %in% excl.names),]

# write the table out
write_csv(US.table, path = "Data/US_last_meals.csv")


# EDA ---------------------------------------------------------------------

# summary bar plots
# table(US.table$Crime) %>% barplot()
# table(US.table$State) %>% barplot()
# table(US.table$Year) %>% barplot()
# table(US.table$Method.of.Dispatch) %>% barplot()

# plot of methods over time
US.table %>% 
  group_by(Year, Method.of.Dispatch) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = Year, y = n, group = Method.of.Dispatch,
             fill = Method.of.Dispatch)) +
  geom_col(position = 'dodge') +
  labs(y = 'Count')


# food words --------------------------------------------------------------

foodbase <- read_xml('Data/Foodbase/FoodBase_uncurated.xml')
food.words <- xml_find_all(foodbase,
                           xpath = "/collection/document/annotation/text") %>%
  xml_text() %>% 
  enframe(value = "word") %>% 
  select(-name)

write_csv(food.words, path = "Data/food_words.csv")
