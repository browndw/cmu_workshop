library(tidytext)
library(tidyverse)

# Let's begin by creating an object consisting of a character string.
# In this case, the first sentence from Sense and Sensibility.
text <- "It is a truth universally acknowledged, that a single man in possession of a good fortune, must be in want of a wife."

# We can check the value of a variable like `text` by running it by itself
text

# Before continuing, we're going to put this text into a "data frame", which is
# a table where we can associate additional variables with the text and perform
# operations on it. This may seem like overkill for just one text, but we're
# going to get used to this pattern over the next few workshops.
text_df <- tibble(
  id = 1, # The first column, which we'll label "id", will serve as an ID number for this text
  txt = text # The second column will contain that original text. We'll label it "txt"
)

# This gives us a table with 2 columns and one row.
text_df

# Doing things like counting words requires us to split a long string of text into individual constituents or "words".
# The unnest_tokens command takes a data frame with a column of text, and splits that column into one row per word.
tokenized_df <- unnest_tokens(text_df, output = "word", input = "txt", token = "words")

# This gives us back a new data frame with 2 columns (that original "id" column, now repeated, and "word")
tokenized_df

# By modifying the arguments in unnest_tokens, we can get different results. 
bigram_df <- unnest_tokens(text_df, output = bigram, input = txt, token = "ngrams", n = 2)
bigram_df

# TO DO ----
# To see what options a function has, use a ? and wrute the function (but without using any parentheses)
# Consult the help page for unnest_tokens() and modify the command to split this sentence into characters
?unnest_tokens

character_df <- unnest_tokens(text_df, output = character, input = txt, ...)
character_df

# Now set this so that it creates a list of characters in both upper and lower case
case_sensitive_character_df <- unnest_tokens(text_df, output = character, input = txt, ...)
case_sensitive_character_df

# To count the total number of words in the sentence, we need to count the
# number of rows in the tokenized table. We can do this using nrow()
nrow(tokenized_df)

# Now that we have a tidied table of all the words in our original text, we can
# start to do more analysis. For exmaple, we can get all the distinct words from
# our sentence by calling the distinct() function and telling it which column to
# get unqiue values from.
distinct_words <- distinct(tokenized_df, word)

# How many distinct words are there?

# If we take the total number of distinct words (or "types") and divide by the
# total number of words ("tokens"), we can get the type-token ratio, a measure
# of how linguistically diverse the text is

# 1. calculate the total number of words and save it to a variable

### YOUR CODE HERE

# 2. Calculate the distinct number of words and save it to a variable

### YOUR CODE HERE

# 3. Divide the distinct number of words by the total number of words (hint: divide by using / )

### YOUR CODE HERE

# Counting word ocurrence

# We've used distinct() on our table to get the unique set of values for one of
# its columns. count() gets the uique set but also adds a new column "n" with
# the number of ocurrences of that value
token_counts <- count(tokenized_df, word)

# By default, count() returns a table sorted by the value its counting. Consult
# ?count to figure out how to return a table ordered by "n"
?count

# The sum of the "n" column should equal the total number of words we calculated above. To access the values from a table column, use the $ notation like so
token_counts$n

# Now use sum() to add all those numbers together
sum(token_counts$n)

# To normalize our counts, we need to divide the raw counts by the total number of words we calculated earlier.
# mutate() will create a new column on our table based on the result of a new calculation we give it. Put the variable containing the total word count into here
normalized_frequency <- mutate(tokenized_df, norm_freq = n / ???)
normalized_frequency

# We can rearrange this table by using the arrange() function.
# To sort based on the absolute word count, tell it to sort based on n
arrange(token_counts, n)

# To reverse the ordering, wrap the ordering column name in the desc() function
arrange(token_counts, desc(n))

# Now create a table in reverse alphabetical order


# Chaining functions together

# We had ro run several operations on our original data frame to get to htis end result. The tidyverse introduces the operator: %>% 
# This is called a "pipe", and it sends the output from one function into the input of the next function. Here's our chain of commands needed to get to the word count table we ended with

token_counts <- text_df %>% 
  unnest_tokens(output = word, input = txt, token = "words") %>% 
  count(word) %>% 
  arrange(desc(n))

# Practice using the mutate function to create a new column that contains the number of characters in each word using the nchar() function
token_stats <- token_counts %>% 
  mutate(nc = nchar(word))

# Get the average word length from our original text. Which column do you need to pass into the mean() function?

# What's the median word length?

# Quick intro to plotting ----

# We'll be doing more plotting in the next workshop, but we'll finish this intro with the most basic plot: a bar plot.

# ggplot() takes a data frame, and then uses aes() (for "aesthetics") to specify which columns should be mapped to which visual variables. Once you specify this base, you then use the + sign to add at least one geom_ layer (for "geometry") which specifies which way you want to render the aesthethics you specified.

ggplot(token_counts, aes(x = word, y = n)) + # Our graph will be very basic - we just want to plot the frequency of each word along the x-axis
  geom_col() # And we will use a bar plot to do this.

# This looks a bit ugly to start out with, because our x axis values are whole words. ggplot allows a LOT of visual customization, but for now we'll just use coord_flip() to flip the axes and display the labels more comfortably

ggplot(token_counts, aes(x = word, y = n)) +
  geom_col() +
  coord_flip()

# In this caese, we may want to rearrange our data so that the most frequently ocurring word comes first. We could do this on the underlying data, but since right now we just want to do this in the context of our plot, we'll do the reordering right inside the ggplot(aes()) call.
ggplot(token_counts, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip()

# Earlier you counted all the letter ocurrences in this text. Create a plot showing them in order:

### YOUR CODE HERE
