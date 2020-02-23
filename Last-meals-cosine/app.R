library(shiny)
library(tidyverse)
library(stringr)
library(tidytext)
library(SnowballC)
library(REdaS)
library(lsa)
data(stop_words)

# ggplot theme ------------------------------------------------------------

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

# data --------------------------------------------------------------------

# read in the food words
food.words <- read_csv("food_words.csv") %>%
    pull() %>%
    str_to_lower()

# remove condiments to the food words list
excl.words <- c("meal", "food", "snack", "drink", "double", "ketchup", "cups", "pecan",
                "mustard", "mayonnaise", "mayo", "sauce", "sour cream", "fried", "onion",
                "onions", "pepper", "ranch", "ranch dressing", "meat", "butter")
food.words <- food.words[!(food.words %in% excl.words)]

# add Coke
food.words <- append(food.words, c("coke", "pepsi"))


# function to return cosine -----------------------------------------------

calc_cosine <- function(sentence.1, sentence.2) {
    # returns the cosine similarity between two sentences
    #  and the document-term matrix
    
    # create the ngrams
    parsed.ngrams <- tribble(~Name, ~Sentence,
                             'Sentence.1', sentence.1,
                             'Sentence.2', sentence.2) %>% 
        unnest_tokens(output = word, input = Sentence, token = "ngrams", n = 3, n_min = 1) %>% 
        anti_join(stop_words, by = "word") %>%
        filter(word %in% food.words)
    
    if (!("Sentence.1" %in% parsed.ngrams$Name & "Sentence.2" %in% parsed.ngrams$Name)) stop("Please enter at least one food item")
    
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
    
    # create document term matrix from stems 
    word.counts <- deduped.ngrams %>% 
        mutate(stem = wordStem(word, language = 'english')) %>%
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
    final.table <- as_tibble(word.counts, rownames = 'Ngram') %>% 
        mutate(V1 = as.integer(V1),
               V2 = as.integer(V2)) %>% 
        rename("Meal one" = V1,
               "Meal two" = V2)
    return(list(cos.measure, final.table))
}

# app ---------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(

    fluidRow(
            column(12,
                # sentence input
                textInput(
                    inputId = "sentence.1",
                    label = "Enter two meals to compare",
                    value = "Celery, olives, coffee, pizza, milk, and a slice of cake",
                    width = '100%',
                    placeholder = NULL
                ),
                textInput(
                    inputId = "sentence.2",
                    label = NULL,
                    value = "Pizza, apple pie, coffee, and milk",
                    width = '100%',
                    placeholder = NULL
                )
            ),
            
            # Show a plot of the generated distribution
            column(5,
                   tableOutput("table")
                ),
            column(7,
                   plotOutput("anglePlot",
                              height = '450px')
            )
        )
)


# server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {

    # calculate the cosine measure
    cosine.objects <- reactive({
        validate(
            need(input$sentence.1 != "", "Please enter a string contain food items")
        )
        calc_cosine(input$sentence.1, input$sentence.2)
    })
    
    # render the table
    output$table <- renderTable({
        cosine.objects()[[2]]
    })

    # render the plot
    output$anglePlot <- renderPlot({
        
        # retrieve the cosine measure
        cos.measure <- cosine.objects()[[1]]
        
        # calculate the cosine angle in degrees
        degree.angle <- REdaS::rad2deg(acos(cos.measure))
        
        # draw a plot containing the angle of the cosine measure
        tibble(x = cos(acos(cos.measure)), y = sin(acos(cos.measure))) %>%
            ggplot() +
            ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = 'grey80') +
            geom_segment(aes(x = 0, y = 0, xend = x, yend = y),
                         arrow = arrow(type = 'closed', length = unit(0.5, "cm")),
                         alpha = 0.7) +
            geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0)) +
            geom_point(aes(x = 1, y = 0), size = 2) +
            geom_point(aes(x = 0, y = 0), size = 2) +
            scale_x_continuous(labels = NULL) +
            scale_y_continuous(labels = NULL) +
            coord_fixed(xlim = 0:1, ylim = 0:1) +
            labs(title = "Cosine similarity projected onto the unit circle",
                 subtitle = paste0('Cosine similarity of ', round(cos.measure, 2), ' equal to ', round(degree.angle, 1), ' degrees'),
                 x = NULL,
                 y = NULL)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
