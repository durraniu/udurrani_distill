library(tidyverse) 
library(pdftools)
library(quanteda) 
library(wesanderson)
library(tidytext)
library(reactable)


pdf_text <- pdf_text(pdf = "discover-large.pdf")

mycorpus <- corpus(pdf_text)

# docvars(mycorpus)
# 
# summary(mycorpus)
# 
# head(mycorpus, 5)


# # Assigns a unique identifier to each text
# docvars(mycorpus, "Textno") <-
#   sprintf("%02d", 1:ndoc(mycorpus))


head(docvars(mycorpus))




# Corpus to dataframe
df <- data.frame(text = sapply(mycorpus, as.character), 
           stringsAsFactors = FALSE, row.names = NULL)


# View(df)




## divide into sentences
dff <- df %>% 
  unnest_tokens(output = sentence, input = text, token = "sentences")
  

View(dff)



## Find which sentence has a 4 digit number
dff <- dff %>% 
  as_tibble() %>% 
  mutate(
    has_a_num = str_detect(string = sentence, pattern = "[[:digit:]]{4}")
  )


## Filter out all sentences which do not have numbers
dff_num <- dff %>% 
  filter(has_a_num == TRUE)

nrow(dff_num)

View(dff_num)





dff_num <- dff_num %>% 
  mutate(
    year = str_extract(string = sentence, pattern = "[[:digit:]]{4}") %>% 
      as.numeric()
  ) %>% 
  select(year, sentence) %>% 
  arrange(year) 


dff_num$sentence[1]

View(dff_num)




reactable(
  dff_num,
  groupBy = "year",
  searchable = TRUE,
  filterable = TRUE,
  resizable = TRUE,
  onClick = "expand",
  showPageSizeOptions = TRUE,
  striped = TRUE,
  highlight = TRUE,
  columns = list(
    year = colDef(name = "Year", maxWidth = 250),
    sentence = colDef( name = "What happened?")
   )
)

















# Text pre-processing
token <- tokens(
    # Takes the corpus
    mycorpus,
    # Remove punctuation
    remove_punct = TRUE,
    # Remove symbols
    remove_symbols = TRUE,
    # Remove URL
    remove_url = TRUE,
    # Split up hyphenated words
    split_hyphens = TRUE
  )


token






# Calculate a document-feature matrix (DFM)

mydfm <- dfm(
  # Take the token object
  token,
  # Lower the words
  tolower = TRUE,
  # Get the stem of the words
  stem = TRUE,
  # Remove stop words
  remove = stopwords("english")
)




# Trim data: remove all the words that appear 
# less than 7.5% of the time and more than 90% of the time

mydfm.trim <-
  dfm_trim(
    mydfm,
    min_docfreq = 0.075,
    # min 7.5%
    max_docfreq = 0.90,
    #  max 90%
    docfreq_type = "prop"
  ) 


quanteda::textplot_wordcloud(
  # Load the DFM object
  mydfm,
  # Define the minimum number the words have to occur
  min_count = 3,
  # Define the maximum number the words can occur
  max_words = 500,
  # Define a color
  color = wes_palette("Darjeeling1")
)







# To visualize the frequency of the top 30 features, we use a lollipop plot:
  
 
# Get the 30 top features from the DFM
freq_feature <- topfeatures(mydfm, 30)

# Create a data.frame for ggplot
data <- data.frame(list(
  term = names(freq_feature),
  frequency = unname(freq_feature)
))

# Plot the plot
data %>%
  # Call ggplot
  ggplot() +
  # Add geom_segment (this will give us the lines of the lollipops)
  geom_segment(aes(
    x = reorder(term, frequency),
    xend = reorder(term, frequency),
    y = 0,
    yend = frequency
  ), color = "grey") +
  # Call a point plot with the terms on the x-axis and the frequency on the y-axis
  geom_point(aes(x = reorder(term, frequency), y = frequency)) +
  # Flip the plot
  coord_flip() +
  # Add labels for the axes
  xlab("") +
  ylab("Absolute frequency of the features")
