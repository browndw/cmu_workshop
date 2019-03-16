# Load the libraries we need
library(tidyverse)
library(readtext)
library(quanteda)

# The first thing we are going to do is gather a list of files that we want to load into our corpus.
# We could point the "readtext" function to a our "academic" directory, so why do it this way?
# In or current file structure they are no subfolders, which is easy for "readtext."
# However, if we did have subfolders, recursively iterating through those folders becomes increasing complicated.
# Starting from a files list is a simple solution, no matter the underlying file structure of our corpus.
# And note that the "list.files" function allows us to specify the type of file we want to load, as well as whether we want to locate files in subfolders ("recursive").

text_files <- dir("data/academic/", pattern = "*.txt", recursive = TRUE, full.names = TRUE)

# From the files list we can create our corpus object by combining the "corpus" and "readtext" functions.
# There are some advantages in separating these steps.
micusp_corpus <- as_tibble(readtext(text_files))
micusp_corpus

# In this collection, the file name encodes the discipline of the document in the first three characters of the doc_id. str_sub() lets us extract a portion of this string. Add a column called discipline to the table using mutate()
micusp_corpus <- micusp_corpus %>% 
  mutate(discipline = str_sub(doc_id, start = 1, end = 3))

# Checking the summary again, we see the new "Discipline" column.
summary(micusp_corpus)

# Use count to see what the disciplines and their document counts are:
count(micusp_corpus, discipline)

# Our data comes from multiple disciplines, which are indicated in the file names.
# What if want to compare the type-to-token ratios from the disciplines?
# First we need to tokenize this whole corpus using the unnest_tokens function
tokenized_micusp_corpus <- micusp_corpus %>% 
  unnest_tokens(output = word, input = text, token = "words")

# This corpus still contains numbers. To filter thouse out of our set, we will use the filter() function. Filter takes a table and then a function or expresison that returns either TRUE or FALSE for each row. The function we'll pass to filter is str_detect(), which we'll ask to look for any digits
tokenized_micusp_corpus <- tokenized_micusp_corpus %>% 
  filter(!str_detect(word, "\\d"))

# Right now, our data is a "long" format, with one row per word per document. We would like to create a mtrix that has one row per document, and one column per "word"
micusp_document_token_counts <- tokenized_micusp_corpus %>% 
  count(doc_id, word)


# Let's look at the 10 most frequent words across this corpus by coutning up all the word appearances.
micusp_counts <- tokenized_micusp_corpus %>% 
  count(word) %>% 
  arrange(desc(n))

# We can also calculate a normalized frequency just as we did in "text_intro.R".
# First we calculate and store the total number of words in our corpus.
total_words <- sum(micusp_counts$n)

micusp_counts <- micusp_counts %>% 
  mutate(norm_freq = n / total_words)

# Let's begin with the some of things we did using basic functions in "text_intro.R".
# With quanteda, however, we can use built-in functions to make our work much easier.
# We'll start by creating a "document-feature matrix".
# This function creates a data object that stores information we can then extract in various ways.
# Notice the various arguments we can use.
# These give us control over what exactly we want to count.
# We'll call our data object "micusp_dfm".
micusp_dfm <- as.dfm(cast_dfm(micusp_document_token_counts, document = doc_id, term = word, value = n))

# We might be interested in knowing what words distinguish one group from another.
# In our corpus, groups are defined by discipline.
# Using the "textstat_keyness" function, we can, for example, comparing English to the rest of the corpus.
# Here we can generate the top 10 keywords.
# We can select a variety of measures, but "lr" is log-likelihood (with a Williams correction as a default).

# First, we identify which documents from our original corpus data belong to the "ENG" discipline
eng <- micusp_corpus$discipline == "ENG"
textstat_keyness(micusp_dfm, target = eng, measure = "lr")

# Or we could use biology as our target.
bio <- micusp_corpus$discipline == "BIO"
textstat_keyness(micusp_dfm, target = bio, measure = "lr")

# What if we wanted to compare English and biology specifically?
# To do that, we would create a separate sub-corpus and then repeat our same tokenization and word coutning process before. Noe that we've run all tehse commands separately above, but here we chain them together using %>%
micusp_compare_corpus <- micusp_corpus %>% 
  filter(discipline %in% c("BIO", "ENG"))

micusp_compare_dfm <- micusp_compare_corpus %>% 
  unnest_tokens(output = word, input = text, token = "words") %>% 
  filter(!str_detect(word, "\\d")) %>% 
  count(doc_id, word) %>% 
  cast_dfm(document = doc_id, term = word, value = n)

# Let's look at the top 10 keywords.
# Note the count in the reference corpus ("n_reference") is much smaller than when we did this the first time.
# That's because our reference corpus consists only of biology texts, rather than texts from all other disciplines.
# Interesting, though, the list is quite similar.
compare_eng <- micusp_compare_corpus$discipline == "ENG"
micusp_keyness <- textstat_keyness(micusp_compare_dfm, target = compare_eng, measure = "lr")

# And for biology.
head(textstat_keyness(compare_dfm, target = "BIO", measure = "lr"), 10)

# If we want to save our keyness data, we can create a data frame.
micusp_keyness <- as.data.frame(textstat_keyness(compare_dfm, target = "ENG", measure = "lr"))

# Check the result.
View(micusp_keyness)

## TO DOs

# Get a token count by discipline. You'll need to start from the tokenized_micusp_corpus data and count() - but now you will need to count based on discipline rather than based on doc_id


# Similarly, we could calculate the average number of tokens per sentence.
# To do this, we will need to tokenize twice - first by splitting the text into sentences, and then by splitting those setnences into tokens

micusp_sentences <- micusp_corpus %>% 
  unnest_tokens(output = sentence, input = text, token = "sentences") %>% 
  # We'll create an intermediate "sentence_id" just by adding a row_number column
  mutate(sentence_id = row_number())

micusp_sentences

micusp_sentences_word_counts <- micusp_sentences %>% 
  unnest_tokens(output = word, input = sentence, token = "words") %>% 
  count(doc_id, discipline, sentence_id)

# To get the average words per sentence, we'll use dplyr's most powerful features, group_by() and summarize(), which are almost always used hand in hand. 
# group_by specifies how you will group the data frame, and then summarize will compress the data frame to one row per unique combination of all the variables in group_by, and then add the results of whatever summary functions you pass to it.
micusp_document_averages <- micusp_sentences_word_counts %>% 
  # First we choose which variables we want to group by (in this case, doc_id and discipline)...
  group_by(doc_id, discipline) %>% 
  # And then we state how we want to summarize (or collapse) the other data. Calling mean() will return 1 number based on all the values per group in n
  summarize(avg_words_per_sentence = mean(n))
micusp_document_averages

# IF we wanted to get the averge words per sentence based on discipline, we'd group only by discipline isntead
micusp_discipline_averages <- micusp_sentences_word_counts %>% 
  group_by(discipline) %>% 
  summarize(avg_words_per_sentence = mean(n)) %>% 
  arrange(desc(avg_words_per_sentence))

micusp_discipline_averages

# CHALLENGE Calculate a type-token ratio for each document in the corpus. hint:
# if you have a column of the unique token counts for each document, you can
# divide them by the sum of those counts by calling n() / sum(n) inside
# summarize (this will get the size of the group using n(), then divide by the
# total sum of the word counts)

discipline_ttr <- tokenized_micusp_corpus %>% 
  count(doc_id, discipline, word) %>% 
  group_by(doc_id, discipline) %>% 
  summarize(ttr = n() / sum(n)) %>% 
  ungroup() %>% 
  mutate_at(vars(discipline), as.factor)

ranked_discipline_ttr <- discipline_ttr %>% 
  mutate(ranked_discipline = fct_reorder(discipline, .x = ttr, .fun = median))

ggplot(ranked_discipline_ttr, aes(x = ranked_discipline, y = ttr)) + geom_boxplot()

# Let's create a basic boxplot.
# We'll specify the data frame we're using ("micusp_summary").
# We'll also specify the x-axis (the "Discipline" column) and the y-axis (the "TypeToken" column).
# Finally, we'll tell ggplot2 which type of plot to generate (a box plot) and the theme to use (the minimal theme).
# Note the plus signs at the end of the lines.
# This tells R that more functions are to come, even though there is a closed parenthesis
# The plot will appear in your "Plots" space on the bottom right.
ggplot(micusp_summary, aes(x=Discipline, y=TypeToken)) + 
  geom_boxplot()

# Note the ordering along the x-axis is by alphabetical order in our first plot.
# Let's recreate the plot, but this time using "reorder" to arrange the x-axis.
# We're telling ggplot2 to reorder "Discipline" by "TypeToken" using the function ("FUN") median.
ggplot(micusp_summary, aes(x=reorder(Discipline, TypeToken, FUN = median), y=TypeToken)) + 
  geom_boxplot() +
  theme_minimal()

# This is better.
# But we can also change the axis labels.
# And we will store this one as a object (called "micusp_boxplot") for output.
micusp_boxplot <- ggplot(micusp_summary, aes(x=reorder(Discipline, TypeToken, FUN = median), y=TypeToken)) + 
  geom_boxplot() +
  xlab("Discipline") + ylab("Type-to-Token Ratio") +
  theme_minimal()

# Check the plot.
micusp_boxplot

# Save the plot to the "output" folder.
ggsave("output/micusp_boxplot.png", plot = micusp_boxplot, width=8, height=5, dpi=300)

# Now we can calculate the mean type-to-token ratios for each discipline.
# And we'll save those to an object we'll call "micusp_tt".
micusp_tt <- aggregate(TypeToken ~ Discipline, micusp_summary, mean)

# Check the result.
micusp_tt

# Let's use our ratio function again.
# This time we'll use it to calculate sentence length.
SentenceLength <- mapply(simple_ratio, micusp_summary$Tokens, micusp_summary$Sentences)

# Again the resulting vector can be appended to our "micusp_summary" data frame.
micusp_summary$SentenceLength <- SentenceLength

# Check the result.
View(micusp_summary)

# Using the "aggregate" function, we can calculate the mean sentence length by discipline.
micusp_sl <- aggregate(SentenceLength ~ Discipline, micusp_summary, mean)

# Another very useful function for combining data is "merge".
# "Merge" takes arguments telling it the objects you want to put together and a "by" argument.
# The result is a new data frame we're calling "micusp_means".
micusp_means <- merge(micusp_sl, micusp_tt, by = "Discipline")

# Check the result.
View(micusp_means)

# To create a barplot, we use the "ggplot" function.
# For aesthetics ("aes"), we specify the x and y axes.
# Here, again, we use "reorder" for the x-axis.
# This time to arrange the "Discipline" categories by "SentenceLength".
# We also specify the colors of the bar outline and fill.
micusp_barplot <- ggplot(micusp_means, aes(x = reorder(Discipline, SentenceLength), y = SentenceLength)) +
  geom_bar(colour="black", fill = "steelblue", stat="identity") +
  xlab("Discipline") + ylab("Mean Sentence Length") +
  theme_minimal()

# Check the plot.
micusp_barplot

# Save the plot to the "output" folder.
ggsave("output/micusp_barplot.png", plot = micusp_barplot, width=8, height=4, dpi=300)

# Let's make one more plot using our "micusp_means" data frame.
# This one will be a scatter plot.
# We will plot mean sentence length along the x-axis and mean type-to-token ratio along the y-axis.
# Each point on the plot will represent a discipline.
# For those will create a label (from the "Discipline" column).
micusp_scatterplot <- ggplot(micusp_means, aes(x = SentenceLength, y = TypeToken, label = Discipline)) +
  geom_point(color="tomato") +
  geom_text(aes(label=Discipline),hjust=.5, vjust=-1) +
  xlab("Mean Sentence Length") + ylab("Mean Type-to-Token Ratio") +
  theme_minimal()
  
# Check the plot.
micusp_scatterplot

# Save the plot to the "output" folder.
ggsave("output/micusp_scatterplot.png", plot = micusp_scatterplot, width=8, height=6, dpi=300)


# For our last plot, we're going to create a dendrogram.
# The plot will show similarities among disciplines based on word frequencies.
# First we need to normalize the counts using the "dfm_weight" function.
# And based on those, we can get distances using "textstat_dist".
micusp_dist <- textstat_dist(dfm_weight(micusp_dfm, "prop"))

# From that distance object, we can use the basic function "hclust" to create a heirarchical cluster.
micusp_cluster <- hclust(micusp_dist)

# Now we can generate the dendrogram.
plot(micusp_cluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")

# To save the plot to the "output" folder, run these 3 lines together.
png(filename = "output/micusp_dendrogram.png", width = 8, height = 5, units = "in", res = 300)
plot(micusp_cluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")
dev.off()
