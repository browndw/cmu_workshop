library(quanteda)
library(ggplot2)

# Let's begin by creating an object consisting of a character string.
# In this case, the first sentence from Sense and Sensibility.
text <- ("It is a truth universally acknowledged, that a single man in possession of a good fortune, must be in want of a wife.")

# Using "summary" we can access the object's ("text") values.
summary(text)

# We can also call up the object itself.
text

# From our text we can create our corpus object by using the "corpus" function.
small_corpus <- corpus(text)

# Let's look at a summary of our corpus.
summary(small_corpus)

# The first column shows the ids of all the texts in our coprus.
# Here, we only have 1, so our text has been assigned "text1" as an id.
# Ids are very important as they link any meta data we may have about our texts with tokens counts; we see more of how this works in our next workshop.
# The next column show the number of unique tokens we have in our corpus and the next the total number of tokens.
# As an experiment, count the total number of works in our corpus... Why does this show 26 instead of 23?
# So as a next step, we need to "tokenize" the corpus; we need to define what we want to count.
# In this case, we tell it to remove punctuation.
small_tokens <- small_corpus %>% tokens(remove_punct = TRUE, what = "word")

# Now check the summary.
summary(small_tokens)

# To learn more about the "tokens" function, we can put a question mark in front of the function name and run the line.
?tokens

# Note all of the choices that are available to us.
# Would we want to count a stirng like "fast-paced" as one token or two?
# If we wanted the latter, we would set "remove_hypens" to "TRUE": tokens(remove_punct = TRUE, remove_hypens = TRUE, what = "word")
# Note, too, that we can tokenize for sequences or "ngrams".
# Let's make an object containing 2-word sequences, or bigrams
bi_grams <- small_corpus %>% tokens(remove_punct = TRUE, what = "word", ngrams = 2)

# View the object
bi_grams

# Now try to create an object containing 3-word sequences or trigrams on your own.

### YOUR CODE HERE

# With our tokens object we can now create a document-feature-matrix using the "dfm" function.
# A dfm is a data structure from which we can build "bag-of-words" statistical analyses.
small_dfm <- dfm(small_tokens)

# Let's look at the 10 most frequent words in "small_dfm" by calling "textstat_frequency".
textstat_frequency(small_dfm, n=10)

# Now try to create a dfm of the bigrams on your own and check the 10 most frequent bigrams.

### YOUR CODE HERE

# Quick intro to plotting ----

# We'll be doing more plotting in the next workshop, but we'll finish this intro with the most basic plot: a bar plot.
# But first we need to create data to plot and put it in a format that our plotting package (ggplot2) likes.
# To do that, we'll put our word frquencies into a "data.frame"

token_counts <- as.data.frame(textstat_frequency(small_dfm))

# Let's look at what we've created.
View(token_counts)

# ggplot() takes a data frame, and then uses aes() (for "aesthetics") to specify which columns should be mapped to which visual variables. Once you specify this base, you then use the + sign to add at least one geom_ layer (for "geometry") which specifies which way you want to render the aesthethics you specified.

ggplot(token_counts, aes(x = feature, y = frequency)) + # Our graph will be very basic - we just want to plot the frequency of each word along the x-axis
  geom_col() # And we will use a bar plot to do this.

# This looks a bit ugly to start out with, because our x axis values are whole words. ggplot allows a LOT of visual customization, but for now we'll just use coord_flip() to flip the axes and display the labels more comfortably

ggplot(token_counts, aes(x = feature, y = frequency)) +
  geom_col() +
  coord_flip()

# In this caese, we may want to rearrange our data so that the most frequently ocurring word comes first. We could do this on the underlying data, but since right now we just want to do this in the context of our plot, we'll do the reordering right inside the ggplot(aes()) call.
ggplot(token_counts, aes(x = reorder(feature, frequency), y = frequency)) +
  geom_col() +
  coord_flip()

# Earlier you counted all the bigrams in this text. Create a plot showing them in order:

### YOUR CODE HERE
