library(tidyverse)
library(tidytext)

# First, we need to read in the text.
frankenstein <- read_lines("data/literature/Shelley_Frankenstein.txt") %>% 
  paste(collapse = " ") %>% 
  enframe(name = "doc_id", value = "text")

# Next we need to parse the text into sentences.
frankenstein_v <- unnest_tokens(frankenstein, output = sentence, input = text, token = "sentences") %>% 
  mutate(sentence_index = row_number()) %>% 
  unnest_tokens(output = word, input = sentence, token = "words") %>% 
  mutate(
    word_index = row_number(),
    progress = word_index / n()) %>% 
  left_join(get_sentiments("bing"), by = "word") %>% 
  left_join(get_sentiments("afinn"), by = "word")

ggplot(frankenstein_v, aes(x = progress, y = score)) +
  geom_smooth(method = "loess", span = 0.4)

# Then we calculate a sentiment value for each sentence using the "get_sentiment" function.
frankenstein_sentiment <- get_sentiment(frankenstein_v)

# The plot we generated above shows a trajectory that has been smoothed to eliminate some of the noise in the raw data.
# To access those values, we need to use the "get_dct_transform" function -- a discrete cosine transformation.
# In the arguments, we've specified we want 100 time units and to scale or sentiment range (from 1 to -1)
frankenstein_dct <- get_dct_transform(frankenstein_sentiment, low_pass_size = 5, 
  x_reverse_len = 100, scale_vals = FALSE, scale_range = TRUE)

# We can put that into a data frame, and call the column "dct".
frankenstein_df <- data.frame(dct = frankenstein_dct)

# And we can add a column called "narrative_time", which simply contains the numbers 1-100 present in our row names.
frankenstein_df$narrative_time <- as.numeric(row.names(frankenstein_df))

# Check our data frame.
View(frankenstein_df)

# Using that data frame, we can now create a plot just like the bottom one created by "simple_plot".
# This is useful because we now have access to the full functionality of ggplot2.
# We can customize our plot in whatever way we want.
# Moreover, we can more easily compare the sentiment from multiple works, which we will do shortly.
sentimentplot_frankenstein <- ggplot(data=frankenstein_df, aes(x=narrative_time, y=dct, group=1)) +
  geom_line(colour= "tomato") +
  xlab("Normalized Narrative Time") + ylab("Scaled Sentiment") +
  theme_minimal()

# Let's look at our plot.
sentimentplot_frankenstein

# Now, let's save it to our "output" folder.
ggsave("output/frankenstein_sentiment_dct.png", plot=sentimentplot_frankenstein, width=8, height=3, dpi=300)

# We can use a similar set of steps to create a plot comparing 2 novels.
# For our comparison, we'll use Pride & Prejudice and Wuthering Heights.
# We'll read in P & P, parse the text by sentence and get our sentiment measurements.
# Just run all 3 lines.
pride_prejudice <- get_text_as_string("data/literature/Austen_PrideAndPrejudice.txt")
pride_prejudice_v <- get_sentences(pride_prejudice)
pride_prejudice_sentiment <- get_sentiment(pride_prejudice_v)

# Do the same for Wuthering Heights.
wuthering_heights <- get_text_as_string("data/literature/Bronte_WutheringHeights.txt")
wuthering_heights_v <- get_sentences(wuthering_heights)
wuthering_heights_sentiment <- get_sentiment(wuthering_heights_v)

# Calculate the transformed values and normalize the time scale.
pride_prejudice_dct <- get_dct_transform(pride_prejudice_sentiment, low_pass_size = 5, 
  x_reverse_len = 100, scale_vals = FALSE, scale_range = TRUE)

wuthering_heights_dct <- get_dct_transform(wuthering_heights_sentiment, low_pass_size = 5, 
  x_reverse_len = 100, scale_vals = FALSE, scale_range = TRUE)

# Create a data frame for P & P.
# Note the 3rd line of code here.
# That simply creates a 3rd column called "novel", which repeats the same value "pp".
# The purpose of that column is to create a variable that we can use as category differentiating one novel from the other.
pride_prejudice_df <- data.frame(dct = pride_prejudice_dct)
pride_prejudice_df$narrative_time <- as.numeric(row.names(pride_prejudice_df))
pride_prejudice_df$novel <- rep("pp",len=100)

# Make a similar data frame for WH.
wuthering_heights_df <- data.frame(dct = wuthering_heights_dct)
wuthering_heights_df$narrative_time <- as.numeric(row.names(wuthering_heights_df))
wuthering_heights_df$novel <- rep("wh",len=100)

# Now let's put those together in a single data frame.
# The function "rbind" combines the data frames row-wise.
compare_df <- rbind(pride_prejudice_df, wuthering_heights_df)

# Check the result.
View(compare_df)

# Now we can use that data frame to generate our plot.
# Note that in the aesthetic ("aes") arguments, we need to specify our categorical variable or "group".
# We can also specify the categorical variable that we want to assign to "colour".
# In this case, the "group" and "colour" are the same.
sentimentplot_compare <- ggplot(data=compare_df, aes(x=narrative_time, y=dct, group=novel, colour = novel)) +
    geom_line() +
    xlab("Normalized Narrative Time") + ylab("Scaled Sentiment") +
    scale_color_manual(values=c("tomato", "steelblue"), name="Novel", labels=c("Pride and Prejudice", "Wuthering Heights")) +
    theme_minimal()

# Check the plot.
sentimentplot_compare

# And save it to our "output" folder.
ggsave("output/compare_sentiment_dct.png", plot=sentimentplot_compare, width=8, height=3, dpi=300)

  
# In addition to looking at fluctuations in overall sentiment, we can measure specific emotional categories like anger and joy.
# This requires us to apply the "get_nrc_sentiment" function to our parsed text.
# The function relies on the NRC (National Research Council) Sentiment Lexicon or EmoLex.
# This processes takes a couple of minutes.
frankenstein_nrc_full <- get_nrc_sentiment(frankenstein_v)

# From that result, we'll calculate the percentage of each category to the total sentiment.
# For this, we take the sums of the first 8 columns and use the "prop.table" function to calculate their percentages.
# We coerce the result into a data frame, calling column "percentage".
frankenstein_nrc_prop <- data.frame(percentage=colSums(prop.table(frankenstein_nrc_full[, 1:8])))

# Check the result.
View(frankenstein_nrc_prop)

# Let's make the row names a column called "emotion" for plotting.
frankenstein_nrc_prop$emotion <- row.names(frankenstein_nrc_prop)

# Reorder the data by the "percentage" column.
frankenstein_nrc_prop$emotion <- factor(frankenstein_nrc_prop$emotion, levels = frankenstein_nrc_prop$emotion[order(frankenstein_nrc_prop$percentage)])

# From the resulting data frame, we can generate a bar plot.
# Note that we're using "coord_flip" to rotate the plot sideways.
nrc_plot <- ggplot(data=frankenstein_nrc_prop, aes(x=emotion, y=percentage)) +
  geom_bar(stat= "identity") +
  xlab("") + ylab("Percentage") +
  coord_flip() +
  theme_minimal()

# View the plot
nrc_plot

# And save it to our "output" folder.
ggsave("output/frankenstein_sentiment_nrc.png", plot=nrc_plot, width=8, height=5, dpi=300)

# Let's make one final plot from this data.
# The most common emotions in Frankenstein, according to the NRC Lexicon, are "trust" and "fear".
# So let's attract those two measurements in the 8th and 4th columns.
frankenstein_trust <- frankenstein_nrc_full[, 8:8]
frankenstein_fear <- frankenstein_nrc_full[, 4:4]

# As we did with the basic sentiment plot, we can use the "get_dct_transform" function.
# This will smooth, normalize and scale our 2 measurements.
frankenstein_trust_dct <- get_dct_transform(frankenstein_trust, low_pass_size = 5, 
  x_reverse_len = 100, scale_vals = FALSE, scale_range = TRUE)

frankenstein_fear_dct <- get_dct_transform(frankenstein_fear, low_pass_size = 5, 
  x_reverse_len = 100, scale_vals = FALSE, scale_range = TRUE)

# Just as we've done previously, we can coerce the results into data frames.
# And we add a categorical variable column we label "emotion".
trust_df <- data.frame(dct = frankenstein_trust_dct)
trust_df$narrative_time <- as.numeric(row.names(trust_df))
trust_df$emotion <- rep("trust",len=100)

fear_df <- data.frame(dct = frankenstein_fear_dct)
fear_df$narrative_time <- as.numeric(row.names(fear_df))
fear_df$emotion <- rep("fear",len=100)

# Join the 2 data frames together.
compare_emotions <- rbind(trust_df, fear_df)

# From that data frame we can generate our plot.
emotions_compare <- ggplot(data=compare_emotions, aes(x=narrative_time, y=dct, group=emotion, colour = emotion)) +
  geom_line() +
  xlab("Normalized Narrative Time") + ylab("Scaled Sentiment") +
  scale_color_manual(values=c("tomato", "steelblue"), name="Emotion", labels=c("Fear", "Trust")) +
  theme_minimal()

# Check the plot.
emotions_compare

# And save it to our "output" folder.
ggsave("output/frankenstein_compare_nrc.png", plot=emotions_compare, width=8, height=4, dpi=300)


text_paths <- dir("data/literature", pattern = "*.txt", recursive = TRUE, full.names = TRUE)
full_texts <- vapply(text_paths, function(x) paste(readLines(x), collapse = " "), FUN.VALUE = character(1))
text_names <- basename(text_paths)

literature <- tibble(doc_id = text_names, text = full_texts)

lit_tokens <- literature %>% 
  unnest_tokens(input = text, output = sentence, token = "sentences") %>% 
  group_by(doc_id) %>% 
  mutate(sentence_index = row_number() / n()) %>% 
  ungroup() %>% 
  unnest_tokens(input = sentence, output = word, token = "words") %>% 
  group_by(doc_id) %>% 
  mutate(word_index = row_number() / n()) %>% 
  ungroup() %>% 
  inner_join(get_sentiments("nrc"), by = "word")

lit_tokens %>% 
  filter(!is.na(sentiment)) %>% 
  ggplot(aes(x = doc_id, fill = sentiment)) + 
  geom_bar(position = "fill")
