
# Let's begin by creating an object consisting of a character string.
# In this case, the first sentence from Sense and Sensibility.
text <- ("It is a truth universally acknowledged, that a single man in possession of a good fortune, must be in want of a wife.")

# Using "summary" we can access the object's ("text") values.
summary(text)

# We can also call up the object itself.
text

# Doing things like counting words requires us to split a long string of text into individual constituents or "words".
# To do this, we can use the function "strsplit", which requires us to specify the object we want to spit ("text") and where to split it (at spaces).
text_split <- strsplit(text, " ")

# If we call up the object, you'll notice that the result of this is a list, with elements that include punctuation (like "acknowledged,").
text_split

# If we want to specify a different word boundary (say at anything that isn't a letter), we can use a regular expression.
text_split <- strsplit(text, "\\W")

# Now you'll note we've gotten rid of the punctuation, but we have created a couple of blank elements where we had sequences of punctuation marks followed by spaces.
text_split

# One way to create a cleaner list is to prep our original text before splitting it.
# First we can eliminate punctuation using the gsub function.
# This asks us to specify  what we want to replace (punctuation), what we want to replace it with (nothing), and the object we want the function to operate on ("text").
# And we'll create a new object ("text_edited")
text_edited <- gsub("[[:punct:]]", "", text)

# Now we can convert that output into all lower case using "tolower".
text_edited <- tolower(text_edited)

# Let's take a look at the result.
text_edited

# And now let's split that at each space.
text_split <- strsplit(text_edited, " ")

# The result is a list with 23 elements.
text_split

# Note in the data space in the upper right of your workspace or in the summary output that we have a "List of 1".
summary(text_split)

# To easily manipulate and extract values, we can change our data into a different type or class of object.
# Here we use the function "unlist".
# After unlisting it, note how it's position has changed in the "Global Environment".
text_split <- unlist(text_split)

# In the summary, you'll see that our object now has a length of 23.
# This is a useful value to access, as it tells us the number of constituent elements in our object.
summary(text_split)

# Let's calculate the total number of words in our sentence using the function "length".
total_words <- length(text_split)

# We can also identify each distinct word using the "unique" function.
# This is data we'd want for word list, for example.
word_types <- unique(text_split)

# How many different types to we have?
# Again, we can use "length".
total_types <- length(word_types)

# The number should be 18.
total_types

# For a type-to-token ratio, we can now perform a basic mathematical operation, dividing our "total_types" by our "total_words".
total_types/total_words

# Now let's work with a different kind of data object.
# Data frames are like tables, allowing us to sort and calculate across rows and down columns.
# We can coerce one from our list of unique words using "as.data.frame".
corpus_data <- as.data.frame(word_types)

# Next let's look at the output from the "match" function.
# The function sequences along the first argument ("text_split"), which is our original sentence broken up into its component words.
# Here we're looking for matches to the 3rd row from the first column in our data frame ("a").
match(text_split, corpus_data[3,1])

# The output says: no, no, yes!...
# You can see how we can use this to count words.
# We simply want to sum all of the matches (which for "a" would be 4)
# To do that, let's write a simple function.
# Our function (which we'll call "counts") will take one argument ("x").
# First, we want to find matches between our text ("text_split") and "x".
# Then, we want to take that result ("word_matches") and find its sum, ignoring or removing all NAs ("na.rm = TRUE").
# Finally, we want to return that sum ("matches_total").
# The whole function needs to be enclosed in brackets "{}".
counts <- function(x){
  word_matches <- match(text_split, x)
  matches_total <- sum(word_matches, na.rm = TRUE)
  return(matches_total)
}

# Now we want to iterate through our data row by row. We want to "apply" the function.
# And R has a variety methods for doing this: among them "apply", "sapply", and "mapply".
# Here, we use "mapply", which is a version of "sapply" for multiple variables.
# We simply tell it to apply the "counts" function to the "word_types" column in our "corpus_data" data frame.
# We can access any column by name using the "$" operator.
word_count <- mapply(counts, corpus_data$word_types)

# To attach the result to our data frame we can use function "cbind", which joins data by column.
# As you might guess, "rbind" joins data by row.
corpus_data <- cbind(corpus_data, word_count)

# To check our result, we can use the "$" operator again, this time to sum our column of word counts.
# It should match the "total_words", which we calculated earlier using "length".
sum(corpus_data$word_count)

# Let's create another function to normalize our counts.
# Again, we'll assign only one argument to our function, "x".
# We want to divide "x" by the total number of words in our text, which we've already stored as "total_words".
# Then, we'll multiply that by a normalizing factor of 100, giving us the percent (or frequency per 100 words).
# Finally we'll use the "round" function to round the result to 2 decimal places.
normalize <- function (x) { 
  normal <- (x/total_words)*100
  normal <- round(normal, 2)
  return(normal)
}

# Again, we use "mapply" to iterate down a column (the "word_count" column) in or data frame.
frequency_norm <- mapply(normalize, corpus_data$word_count)

# We'll attach the result using a different technique this time.
# Rather than "cbind", we can use the column operator ("$") to add a column with the same name as our result ("frequency_norm")
corpus_data$frequency_norm <- frequency_norm

# We can sum the column to check our calculations.
# Looks good with a small rounding error.
sum(corpus_data$frequency_norm)

# To conclude our brief introduction, let's take a look at how we can order our data frame.
# You might have noted that our data frame remains in the order that it was originally created.
# You can open it by clicking on the grid icon to the right in the "Global Environment".
# And once open, there are arrows that enable you to sort the various columns.
# However, sorting in this way is only temporary.
# So let's learn one way of sorting our data frame to prepare it for output.
# We're going to use the "order" function.
# If we execute the "order" function on the "word_types" column, you'll see that it produces just a sequence of numbers.
order(corpus_data$word_types)

# These are our row indexes.
# If you open the data frame by clicking on it, you'll see the numbers on the left.
# Row 3 is "a" in the "word_types" column; 6 is "acknowledged"; and so on.
# The number sequence is simply the indexes arranged by "word_types" (in alphabetical order).
# So what if we want to see not just the indexes, but the entire data frame?
# To understand how this works, you need to know how to specify rows and columns in a data frame.
# Square brackets "[ ]" after a data frame indicate rows and columns, which are separated by a comma.
# So to see row 3, column 2, we can do this, which shows the 2nd column ("word_count") for the 3rd row.
corpus_data[3,2]

# If we don't put any number before the comma, we return all rows of the second column.
corpus_data[,2]

# This is identical to using the column operator ("$") to specify a column by name.
corpus_data$word_count

# Likewise, we could put nothing after the comma to return all columns of the 3rd row.
corpus_data[3,]

# Knowing that syntax, we can understand what we're doing here.
# We want to order the rows by "word_types" and return all columns of our data frame.
# Thus, the order function goes before the comma, and nothing goes after it.
corpus_data[order(corpus_data$word_types),]

# We can use the same principle to order the data frame by "word_count".
corpus_data[order(corpus_data$word_count),]

# Note, however, that this only produces but does not preserve the reordering.
# It also orders from low-to-high.
# To preserve the order, we assign the ordering to our "corpus_data" object.
# To reverse the order, we use the minus sign.
corpus_data <- corpus_data[order(-corpus_data$word_count),]

# Let's check the result.
View(corpus_data)

# Finally, we can save our data frame as a table using "write.csv".
# Because the names of the rows are simply numbers, we don't want to include them.
# We can specify this with "row.names".
write.csv(corpus_data, file="output/practice_data.csv", row.names = FALSE)

# Now you see how text processing works with R's basic functions.
# You can also see how this could get rather tedious and labor-intensive to create from scratch.
# Thankfully, the little we've done here (and much, much more) are built in to packages like quanteda and tm.
# That is where we go in the next script: "corpus_intro.R".
