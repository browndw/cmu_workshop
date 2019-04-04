# For the second workshop, we'll be following many of the same procedures
# that we practiced in the first workshop. However, today we'll be working with
# many texts rather than a single sentence (or string).
#
# In this lesson you will learn:
#
# New functions for manipulating data.frames
# - select()
# - mutate()
# - filter()
# - arrange()
# - min_rank()
# - slice()
#
# New functions for text tokenization and analytics
# - tokens_compound()
# - textstat_keyness()
#
# Methods for multivariable data visualization
# - grouping by color
# - grouping by facets / small-multiples
# - 2D scatterplots
# - Text labels
# - boxplot
#
# We'll begin, just as we did in the the first workshop.
#
# library() loads an R library - sets of functions that we'll want to use during
# this session.
#
# quanteda is a large package with text analysis functions (FYI it can be a bit
# slow to load the first time!)
#
# tidyverse contains functions for manipulating data frames, and also includes
# the entire ggplot2 library for visualizing data frames

library(quanteda)
library(tidyverse)

# Reading in large corpora ----

# If you are working locally on your own maching, rather than through the web
# interface, you can also install the readtext package, which assists in loading
# all of our texts

# install.packages("readtext")
# library(readtext)

# Like quanteda, readtext is large and contains many functions we won't need to
# use. So we created our own helper function that does what we need it to do a
# little more efficiently. We can load this an another function that we will use
# later by running the line below.
source("R/functions/workshop_functions.R")

# In your Environment on the upper-left, you will see two functions: effect_size
# and readtext_lite.
#
# Now we are going to load file containing all of the metadata for our corpus.
# For this, we use one of R's many read functions. This one is a specific
# implementation for comma-separated-value (or csv) files.
micusp_meta <- read_csv("data/academic/MICUSP_meta.csv")

# We can view the file to see what kinds of information it contains. The data
# comes from the Michigan Corpus of Upper-Level Student Papers (or MICUSP). For
# the workshop, we've sampled 10 papers from each of the 17 disciplines
# represented in the corpus.
#
# You can find out more about MICUSP at http://elicorpora.info/
View(micusp_meta)

# If you ever need to quickly peek at the column names of a data frame, use colnames
colnames(micusp_meta)

# Next we are going to select a subset of categorical variables from the
# metadata, which we will want to attach to and use with our corpus. The
# select() function takes a data.frame and then one or more column names to
# keep. We'll name our new data frame "doc_categories".
doc_categories <- micusp_meta %>% 
  select(discipline_cat, level_cat, student_gender, speaker_status, paper_type, paper_features)

# In our new data frame, you find the following information:
# a three letter code for each discipline (discipline_cat),
# a code for the grade-level of the student writer (level_cat),
# a code for the gender of the student writer (student_gender),
# and a code for whether student writer identefies as a native or non-native speaker of English (speaker_status).
View(doc_categories)

# Because we have 170 text files to load into our corpus, we need to first read
# them into a data frame. To do this, you can use the readtext package or the
# lite function that we've already loaded into our environment.
doc_df <- readtext_lite(micusp_meta$file_path)

# Our data frame (doc_df) has just two columns: doc_id and text.
# Using the corpus function, we can now create a corpus from that data frame.
micusp_corpus <- corpus(doc_df)

# Let's take a look at it.
summary(micusp_corpus)

# Notice that none of our metadata has yet been attached to our corpus.
# Remember our doc_categories? To attach those to our new corpus,
# we can assign them as "document variables" using docvars().
?docvars

# The following command might look backwards, with the function on the left hand
# side of the <- operator. That is because it's an accessor function, which lets
# us add or modify data in an object. You can tell when a function is an
# accessor function like this because its help file will show that you can use
# it with <-, for example as we saw in ?docvars:
#
# docvars(x, field = NULL) <- value
docvars(micusp_corpus) <- doc_categories

# Now let's check our summary again.
summary(micusp_corpus)

# So as a next step, we need to customize exactly how we want quanteda to count
# tokens by using the tokens() function.
#
# tokens() takes our corpus, and then takes additional arguments that customize
# what it does. We'll tell it to tokenize by individual word, and to remove
# punctuation, numbers, and symbols.
micusp_tokens <- tokens(micusp_corpus, include_docvars=TRUE, remove_punct = TRUE,
                        remove_numbers = TRUE, remove_symbols = TRUE, what = "word")

# An issue that we run into frequently with corpus analysis is what to do with
# multi-word expressions. For example, consider a common English quantifier: "a
# lot". Typical tokenization rules will split this into two tokens: "a" and
# "lot". But counting "a lot" as a single unit would really be more accurate. We
# have a way of telling quanteda to account for these tokens.
#
# First, we need to load in a list of our expressions. Using readLines(), we
# load in a text file containing a multi-word expression on each line.
multiword_expressions <- readLines("data/stoplists/mwe_short.txt")

# This creates a character vector; in other words, multiple string values.
multiword_expressions

# The tokens_compound() functions looks for token sequences that match our list
# and combines them using an underscore.
micusp_tokens <- tokens_compound(micusp_tokens, pattern = phrase(multiword_expressions))

# In the first workshop, we learned about the pipe %>% operator that chains
# together multiple functions. Starting from the micusp_corpus, create the
# micusp_tokens object using both the tokens() and tokens_compound() command

### YOUR CODE HERE 

# (hint:)
# micusp_corpus %>% 
#    tokens(...) %>% ...

# With our tokens object we can now create a document-feature-matrix using the
# "dfm" function. A dfm is grid with one row per document in the corpus, and one
# column per unique "token" in the corpus. Each cell contains a count of how
# many times a token shows up in that document.
#
# A DFM is a commonly-used data structure for statistical analyses that look at
# word/ngram counts.
micusp_dfm <- dfm(micusp_tokens)

# One way to inspect our dfm is to use the topfeatures() function.
topfeatures(micusp_dfm)

# We can also group our counts according to any of the docvars we assigned to
# our corpus. For example, we can inspect the top words in each discipline.
# Remember you can check the docvars of this dfm by running:
# docvars(micusp_dfm)

topfeatures(micusp_dfm, groups = "discipline_cat")

# Now try to examine the top features grouped by paper type.

### YOUR CODE HERE

# Do the same but grouped by student level.

### YOUR CODE HERE

# We can also store full frequencies for all words using textstat_frequency()
# either for the whole corpus -- textstat_frequency(micusp_dfm)
# or with a grouping variable -- textstat_frequency(micusp_dfm, groups = "discipline_cat").
word_freq <- textstat_frequency(micusp_dfm)

# Let's see what we've created.
View(word_freq)

# Now collect the word frequencies grouped by discipline
word_freq_discipline <- textstat_frequency(micusp_dfm, groups = "discipline_cat")

# Now we have one row per word/group combination.

# Visualizing with groups

# Often we need to filter our data down a bit in order to plot it. A bar chart
# with 7000 bars isn't very legible. So let's keep just the top 10 words
#
# filter() takes a data frame and then a series of rules based on values in that
# data frame's columns. To get only the rows where the "rank" of a word is
# greater than or equal to 10:
top_word_freqs <- word_freq_discipline %>% 
  filter(rank <= 10)

# filter() statements can be as complex as you need them to be. Keep only the
# top 10 words in "English":
top_english_freqs <- word_freq_discipline %>% 
  filter(rank <= 10 & group == "English") # Note that we use == to mean "is this equal?"

## Visualizing with groups ----

# If we only give it the feature and frequency, ggplot will add everything up for us.
ggplot(top_word_freqs, aes(x = feature, y = frequency)) +
  geom_col()

# If we want to subdivide our plot by discipline, we have a few options. One is
# to partition by color:
ggplot(top_word_freqs, aes(x = feature, y = frequency, fill = group)) +
  geom_col()

# the default way to position columns subdivided with fill colors isn't always
# very useful. Experiment by using geom_col(position = "dodge") and
# geom_col(position = "fill"). How does position = "fill" transform what the
# y-axis measures?

### YOUR CODE HERE

# Another is to make "small multiples" by what ggplot calls "faceting". This is
# most useful when you'd like to make one graph per facet group:
ggplot(top_word_freqs, aes(x = feature, y = frequency)) +
  geom_col() +
  facet_wrap(~ group)

# We can still stack on additional modifiers that we learned last time, like
# flipping coordinates with coord_flip()

### YOUR CODE HERE

# Based on these plots, what are the drawbacks of trying to compare these
# categories by looking at raw feature frequencies?

# Overview so far ----

# 1. read in files with readtext_lite() or readtext()
# 2. corpus()
# 3. tokens()
# 4. dfm()
# 5. analysis (with e.g. textstat_frequency())

# Keyness ----

# If you've done any corpus analysis previously, you'll know
# that word frequencies by themselves don't usually get us very far.
# To lacate and make sense out of patterns, we often need to carry out
# various kinds of statistical tests.

# One of the most common is a keyness comparison.
# Keyness compares word frequencies in a target corpus and compares them to
# frequencies in a reference corpus. For more detail,
# see here http://ucrel.lancs.ac.uk/llwizard.html
# and here http://www.thegrammarlab.com/?nor-portfolio=understanding-keyness
# In quanteda, we can calculate keyness using textstat_keyness()
# As with textstat_frequency(), we use the function on our document feature matrix (dfm)
# In addition we need to specify our target using one of our categorical variables.

# First, let's see how we can specify which variable we want.
# Earlier, we assigned document variables using docvars().
# We can also use docvars() to look up those same variables.
# For example, we can find which texts in our corpus were written
# by female-identifying sudents using two equal signs indicating
# that we want the student_gender variable where it is "F".
student_f_index <- docvars(micusp_dfm, "student_gender") == "F"

# This creates a "logical" or "boolean" vector of TRUE/FALSE values. R often
# uses these kinds of indices when you are trying to tell it to look at a subset
# of some data
student_f_index

# We we use it with textstat_keyness we are indicating that we want the papers
# with student_gender equal to "F" to be our target corpus. All other papers
# will be the reference corpus.
#
# The specific method we're using is log-likelihood, which is designated by
# "lr". In this case MICUSP only provides one other category for student_gender:
# "M". Thus keyness will show the tokens that are more frequent in papers by
# female-identifying writers vs. male-identifying writers.
female_keywords <- textstat_keyness(micusp_dfm, student_f_index, measure = "lr")

# Check the results.
View(female_keywords)

# It is important to that the keyness statistic is a measure of significance. It
# tells us how much evidence we have for a difference in token frequencies. But
# it tells us nothing about the magnitude of the effect. For that we need to
# calculate an effect size.

# The quanteda package does not have a function for effect sizes. So we have
# created a simple function effect_size() that calculates Hardie's Log Ratio:
# http://cass.lancs.ac.uk/log-ratio-an-informal-introduction/
#
# This function takes two lists of numbers:
# - n_target: The number of times that term shows up in the target category
# - n_reference: The number of times that term shows up in all other categories

# You can access any column of a data.frame by using $
female_keywords$n_target

# And this vector of numbers is what we pass into the effect_size() function
effect_size(female_keywords$n_target, female_keywords$n_reference)

# A common way to compute new columns for a data frame based on values in existing columns is to use the mutate() function:
# (We'll also use the %>% operator for readability)
female_keywords <- female_keywords %>% 
  # While we're inside muatate(), we don't have to repeat the data frame name or
  # use $ - mutate() knows that we're trying to refer to other columns. "effect"
  # will be the name of the new column, and its contents will be the output of
  # effect_size()
  mutate(effect = effect_size(n_target, n_reference)) 

# Now see that we have a new effect column
female_keywords

# To reorder the data frame, we use arrange()
key_words %>% 
  arrange(effect)

# To sort in descending order, wrap the variable name in desc()
key_words %>% 
  arrange(desc(effect))

# What if we want to get the largest effect sizes, no matter whether positive or negative? We'd need to calculate the absolute value
?abs

# Add a column to key_words called abs_effect. You'll want to use mutate() and abs() in combination

### YOUR CODE HERE

# Here we can see the top ten keywords when the paper_type "Report" is the target.
report_keywords <- textstat_keyness(micusp_dfm, docvars(micusp_dfm, "paper_type") == "Report", 
                      measure = "lr")

report_keywords %>% 
  mutate(effect = effect_size(n_target, n_reference)) %>% 
  arrange(desc(effect))

# Now try to examine the top 10 keywords when the paper_type is a Proposal.

### YOUR CODE HERE

english_keywords <- textstat_keyness(micusp_dfm, docvars(micusp_dfm, "discipline_cat") == "English", 
                                    measure = "lr") %>% 
  mutate(effect = effect_size(n_target, n_reference))

biology_keywords <- textstat_keyness(micusp_dfm, docvars(micusp_dfm, "discipline_cat") == "Biology", 
                                     measure = "lr") %>% 
  mutate(effect = effect_size(n_target, n_reference)) 

# To visualize the difference between these two datasets, it could help to make
# a scatterplot of words arranged based on their relative effect size as
# keywords for either set. To do this we'll need to make one table that has:
  # - one row per word
  # - columns for the English effect sizes and other stats
  # - columns for hte Biology effect sizes and other stats
  
# left_join() is a function for combining two different data frames based on a
# column with shared values. Both of these data frames will have rows in their
# "feature" columns that are the same, so we will join on that column. The
# "suffix" argument will add extra names to the shared, non-joining columns like
# "effect_size" and "p" so that
  
comparison_stats <- english_keywords %>% 
  left_join(biology_keywords, by = "feature", suffix = c("_eng", "_bio"))

# Check the new column names of this table and we see that we have one "feature"
# column and then the other table columns with added suffixes
colnames(comparison_stats)

significant_comparison_stats <- comparison_stats %>% 
  # We'll want to do some filtering first - let's make sure each term we try to display shows up at least once in both of our categories
  filter(n_target_eng > 1 & n_target_eng > 1) %>% 
  # And we will create two summary variables - largest_effect, giving us a sense of the most powerful effect whether positive or negative, and smallest_p - which features have the smallest p for either 
mutate(
    average_effect = (abs(effect_eng) + abs(effect_bio)) / 2,
    average_p = (p_eng + p_bio) / 2)

ggplot(significant_comparison_stats, aes(x = effect_eng, y = effect_bio)) +
  geom_point()

# OK, this lays out things clearly enough. But points aren't that useful of a
# geometry for us. We can use the words themselves! Notice we need to add an
# extra argument to our aes() - geom_text takes x, y, as well as a label
# argument
ggplot(significant_comparison_stats, aes(x = effect_eng, y = effect_bio, label = feature)) +
  geom_text()

# That is... illegible. So let's go back up to our significant_comparison_stats
# and filter more strictly. Revise the filter so that, in addition to filtering
# on n_target, it also only keeps those where the p value for both English and
# Biology is less than or equal to 0.05. Now run the plot again.

### YOUR CODE HERE

# We can take advantage of other aesthethics to refine this plot more. We can
# size words based on their average effect size, and adjust their opacity based
# on their p-values
#
# n.b. when a single line of code gets really long, R doesn't mind if you add in
# line breaks between commas. This makes it more readable for human beings.
ggplot(significant_comparison_stats, 
       aes(x = effect_eng, y = effect_bio, label = feature, 
           size = average_effect, alpha = average_p)) +
  geom_text() +
  # Note that smaller p-values are better, so we want to transform the alpha
  # scale by reversing its usual defaults, so that smaller numbers are more
  # opaque and larger numbers are more transparent.
  scale_alpha(trans = "reverse") 

# Another good addition would be horizontal and vertical lines centered on the 0-axes so we know when something goes from having e.g. a positive biology effect size to a negative one.
# Add geom_hline(yintercept = 0)
# Add geom_vline(xintercept = 0)

### YOUR CODE HERE

# In addition to conducting different kinds of statistical tests,
# we may also want to see some of the context in which tokens appear.
# These contextual presentations of tokens are called
# key words in contexts (KWIC).
# To generate them in quanteda, we can use the kwic() function.
# We can operate this function on either a tokens object or a corpus object.
# Remember, that when we tokenized our corpus, we removed
# numbers, symbols and punctuation. So if we want to see those,
# We should use our corpus object with the function.
# Also note that we can specify a window size.
# The window is the span of tokens to left and right of our
# search token that we want to see.

# In these examples, note the difference btween a "fixed" search
# and a "glob" search. We can also use regular expressions with "regex".

kwic(micusp_corpus, "data", window = 3, valuetype = "fixed")

kwic(micusp_corpus, "suggest*", window = 3, valuetype = "glob")

# We can use kwic() to look at the context of the significant keywords we
# identified in the previous section. IF we want to just look at the English
# docs from the corpus, then we will first use corpus_subset() to only pull
# those docs.

english_corpus <- corpus_subset(micusp_corpus, discipline_cat == "English")
english_corpus

kwic(english_corpus, "blood*", window = 4, valuetype = "glob")

# Now look at blood in the context of biology

### YOUR CODE HERE

# What about the use of "novel"? "webster"? Try looking at terms that are strong
# positive keywords for biology but negative for english, too.

# Finally, we're going to see how we can use a simple dictionary to group our
# tokens into classes that we can then count.
#
# The first step is to load in our dictionary. The dictionary that we're using
# is in what is called YAML -- short for "YAML Ain't Markup Language". YAML is
# very friendly to human readers and very easy to format.
#
# You can look at the example if you wish. This one has list of words and
# phrases grouped under "hedges" and another under "boosters". To load it, we
# use the dictionary() function.
hb_dict <- dictionary(file = "data/stoplists/hedges_boosters.yml")

# Next we create a new tokens object from our original one.
# By using tokens_lookup() with our dicitonary, we will create
# groupings based on our dictionary.
# Note that our dictionary has only 1 level.
# But if we can a more complex taxonomy, we can specify
# which level of the taxonomy we'd like to group our tokens under.
hb_toks <- tokens_lookup(micusp_tokens, dictionary = hb_dict, levels = 1)

# Now we create a new document features matrix.
# And we're going to convert it to a data frame that we can use later.
hb_dfm <- dfm(hb_toks)
hb_dataframe <- convert(hb_dfm, to = "data.frame")


# Check the frequencies.
textstat_frequency(hb_dfm)

# If we wanted to plot hedges vs. boosters, however,
# we would first need to normalize our counts -- 
# by total counts of tokens or sentences in each text.
# This information can be retrieved easily from our tokens and corpus.
ntoken(micusp_tokens)
nsentence(micusp_corpus)

# Let's store both of those first.
tokens_total <- ntoken(micusp_tokens)
sentences_total <- nsentence(micusp_corpus)

# Next, we can  create our own, very simple function, which we name "simple_ratio".
# Our function requires two arguments -- x and y.
# The fuction itself is contain betwen the curly brackets.
# It divides x by y, rounds it to 5 decimal places, and returns it.
simple_ratio <- function(x, y) {
  ratio <- x / y
  return(ratio)
}



# Now we're going to apply the function to the hedges column
# in our hb_dfm and to our tokens_total.

hb_dataframe %>% 
  mutate()

hedges_norm <- mapply(simple_ratio, hb_dataframe$hedges, tokens_total)
boosters_norm <- mapply(simple_ratio, hb_dataframe$boosters, tokens_total)

# How would you normalize by the total number of sentences instead?

### YOUR CODE HERE

# We now append these results to our hb_dfm in columns called "hedges_norm" and "boosters_norm".
hb_dfm$hedges_norm <- hedges_norm
hb_dfm$boosters_norm <- boosters_norm

hb_ratios <- data.frame(
  
)

ggplot(hb_dfm, aes(x = hedges_norm, y = boosters_norm)) +
  geom_point()
