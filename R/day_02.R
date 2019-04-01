# For the second workshop, we'll be following many of the same procedures
# that we practiced in the first workshop. However, today we'll be working with
# many texts rather than a single sentence (or string).
#
# We're also going to add to our toolbox by learning how to lists and dictionaries,
# which can help us control what we want to define as a "token" and what we analyze
# by group tokens into categories that we can count.
#
# We'll begin, just as we did in the the first workshop.
#
# library() loads an R library - sets of functions that we'll want to use during
# this session.
#
# quanteda is a large package with text analysis functions (FYI it can be a bit
# slow to load the first time!)
#
# ggplot2 is another popular package with data visualization functions

library(quanteda)
library(ggplot2)

#library(readtext)

# If you are working locally on your own maching, rather than through the web interface,
# you can also install the readtext package, which assists in loading all of our texts
# before creating our corpus. That package, like quanteda, is also somewhat large.
# So we created our own helper function that does what we need it to do a little more efficiently.
# We can load this an another function that we will use later by running the line below.
source("/functions/workshop_functions.R")

# In your Environment on the upper-left, you will see two functions: effect_size and readtext_lite.
#
# Now we are going to load file containing all of the metadata for our corpus.
# For this, we use one of R's many read functions.
# This one is a specific implementation for comma-separated-value (or csv) files.
micusp_meta <- read.csv("/metadata/MICUSP_meta.csv", stringsAsFactors = FALSE)

# We can view the file to see what kinds of information it contains.
# The data comes from the Michigan Corpus of Upper-Level Student Papers (or MICUSP).
# For the workshop, we've sampled 10 papers from each of the 17 disciplines represented in the corpus.
# You can find out more about MICUSP at http://elicorpora.info/
View(micusp_meta)

# Next we are going to select a subset of categorical variables from the metadata,
# which we will want to attach to and use with our corpus.
# The variables we want are in the 6th through the 10th columns.
# To subset those columns, we use square brackets after the name of our data frame ("micusp_meta").
# Between the brackets, there is a comma. To the left of the comma we can define what rows we want.
# To the right of the column we can define the columns.
# For example, micusp_meta[1,1] would return the first row and the first column: BIO.G0.02.1
# As we want all rows, we leave the left side of the comma blank.
# To the right, we put 6:10, which returns columns 6 through 10.
# We'll name our new data frame "doc_categories".
doc_categories <- micusp_meta[,6:10]

# In our new data frame, you find the following information:
# a three letter code for each discipline (discipline_cat),
# a code for the grade-level of the student writer (level_cat),
# a code for the gender of the student writer (student_gender),
# and a code for whether student writer identefies as a native or non-native speaker of English (speaker_status).
View(doc_categories)

# Because we have 170 text files to load into our corpus, we need to first read them into a data frame.
# To do this, you can use the readtext package or the lite function that we've already loaded into our environment.
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

# So the first element of our argument will be the oject we want to assign the variable to: micusp_corpus.
# The second will be the names of the fields we want to assign.
# We can call them whatever we want. But we already have convenient names in our column headings.
colnames(doc_categories)

# To assign the names and the values, therefore, we simply execute the following.
docvars(micusp_corpus, colnames(doc_categories)) <- doc_categories

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

# An issue that we run into frequently with corpus analysis is what to do with multi-word expressions.
# For example, consider a common English quantifier: "a lot".
# Typical tokenization rules will split this into two tokens: "a" and "lot".
# But counting "a lot" as a single unit would really be more accurate.
# We have a way of telling quanteda to account for these tokens.
# First, we need to load in a list of our expressions.
# Using readLines(), we load in a text file containing a multi-word expression on each line.
multiword <- readLines("/lists/mwe_short.txt")

# The tokens_compound() functions looks for token sequences that match our list
# and combines them using an underscore.
micusp_tokens <- tokens_compound(micusp_tokens, phrase(multiword))

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

# We can also group our counts according to any of the categories we assigned to our corpus.
# For example, we can inspect the top words in each discipline.
topfeatures(micusp_dfm, groups = "discipline_cat")

# Now try to examine the top features grouped by paper type.

### YOUR CODE HERE

# Do the same but grouped by student level.

### YOUR CODE HERE

# We can also store word frequencies using textstat_frequency()
# either for the whole corpus -- textstat_frequency(micusp_dfm)
# or with a grouping variable -- textstat_frequency(micusp_dfm, groups = "discipline_cat").
word_freq <- textstat_frequency(micusp_dfm)

# Let's see what we've created.
View(word_freq)

# Overview so far ----

# 1. read in files with readtext_lite() or readtext()
# 2. corpus()
# 3. tokens()
# 4. dfm()
# 5. analysis (with e.g. textstat_frequency())

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
docvars(micusp_dfm, "student_gender") == "F"

# We we use it with textstat_keyness we are indicating that we want
# the papers with student_gender equal to "F" to be our target corpus.
# All other papers will be the reference corpus.
# The specific method we're using is log-likelihood, which is designated by "lr".
# In this case MICUSP only provides one other category for student_gender: "M".
# Thus keyness will show the tokens that are more frequent in papers by
# female-identifying writers vs. male-identifying writers.
key_words <- textstat_keyness(micusp_dfm, docvars(micusp_dfm, "student_gender") == "F", measure = "lr")

# Check the results.
View(key_words)

# It is important to that the keyness statistic is a measure of significance.
# It tells us how much evidence we have for a difference in token frequencies.
# But it tells us nothing about the magnitude of the effect.
# For that we need to calculate an effect size.

# The quanteda package does not have a function for effect sizes.
# So we have created a simple function effect_size()
# that calculates Hardie's Log Ratio: http://cass.lancs.ac.uk/log-ratio-an-informal-introduction/
# This line adds a column called log_ratio to our key_words data table.
key_words$log_ratio <- effect_size(key_words)

# If we simply want to view a limited number of results we can use head().
# Here we can see the top ten keywords when the paper_type "Report" is the target.
head(textstat_keyness(micusp_dfm, docvars(micusp_dfm, "paper_type") == "Report", 
                      measure = "lr"), 10)

# Now try to examine the top 10 keywords when the paper_type is a Proposal.

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

head(kwic(micusp_corpus, "data", window = 3, valuetype = "fixed"), 10)

head(kwic(micusp_corpus, "suggest*", window = 3, valuetype = "glob"), 10)

# Finally, we're going to see how we can use a simple dictionary to
# group our tokens into classes that we can then count.
# The first step is to load in our dictionary.
# The dictionary that we're using is in what is called YAML --
# short for "YAML Ain't Markup Language".
# YAML is very friendly to human readers and very easy to format.
# You can look at the example if you wish.
# This one has list of words and phrases grouped under "hedges"
# and another under "boosters".
# To load it, we use the dictionary() function.
hb_dict <- dictionary(file = "/Users/davidwestbrown/SpiderOak Hive/CMU/CMU Workshop/lists/hedges_boosters.yml")

# Next we create a new tokens object from our original one.
# By using tokens_lookup() with our dicitonary, we will create
# groupings based on our dictionary.
# Note that our dictionary has only 1 level.
# But if we can a more complex taxonomy, we can specify
# which level of the taxonomy we'd like to group our tokens under.
hb_toks <- tokens_lookup(micusp_tokens, hb_dict, levels = 1)

# Now we create a new document features matrix.
# And we're going to convert it to a data frame that we can use later.
hb_dfm <- dfm(hb_toks)
hb_dfm <- convert(hb_dfm, to = "data.frame")


# Check the frequencies.
textstat_frequency(hb_dfm)

# Try out keyness using "Proposals" as our target.
textstat_keyness(hb_dfm, docvars(hb_dfm, "paper_type") == "Proposal", measure = "lr")

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
simple_ratio <- function(x,y){
  ratio <- x/y
  ratio <- round(ratio, 5)
  return(ratio)}

# Now we're going to apply the function to the hedges column
# in our hb_dfm and to our tokens_total.
hedges_norm <- mapply(simple_ratio, hb_dfm$hedges, tokens_total)
boosters_norm <- mapply(simple_ratio, hb_dfm$boosters, tokens_total)

# How would you normalize by the total number of sentences instead?

### YOUR CODE HERE

# We now append these results to our hb_dfm in columns called "hedges_norm" and "boosters_norm".
hb_dfm$hedges_norm <- hedges_norm
hb_dfm$boosters_norm <- boosters_norm

ggplot(hb_dfm, aes(x = hedges_norm, y = boosters_norm)) +
  geom_point()
