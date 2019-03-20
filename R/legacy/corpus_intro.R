
# To load the packages we'll be using, simply run all 3 lines.
library(readtext)
library(quanteda)
library(ggplot2)


# The first thing we are going to do is gather a list of files that we want to load into our corpus.
# We could point the "readtext" function to a our "academic" directory, so why do it this way?
# In or current file structure they are no subfolders, which is easy for "readtext."
# However, if we did have subfolders, recursively iterating through those folders becomes increasing complicated.
# Starting from a files list is a simple solution, no matter the underlying file structure of our corpus.
# And note that the "list.files" function allows us to specify the type of file we want to load, as well as whether we want to locate files in subfolders ("recursive").

data_dir <- list.files("data/academic/", pattern="*.txt", recursive = TRUE, full.names = TRUE)

# From the files list we can create our corpus object by combining the "corpus" and "readtext" functions.
# There are some advantages in separating these steps.
micusp_corpus <- corpus(readtext(data_dir))

# Now check the summary of our corpus object.
summary(micusp_corpus)

# Our data comes from multiple disciplines, which are indicated in the file names.
# What if want to compare the type-to-token ratios from the disciplines?
# There are a variety of ways to subset data in R. For columns containing numeric and categorical variables, "subset" is easy and useful.
# Here, however, we are going to work on subsetting within our corpus object using quanteda's framework.
# First, we can assign new metadata to our corpus using the "docvars" (document variables) function.
# In this case we tell it to add the field "Discipline" to "micusp_corpus".
# The next part is based on regular expressions. The expression says find the first three letters in the "doc_id" and copy those into the new column.
docvars(micusp_corpus, "Discipline") <- gsub("(\\w{3})\\..*?$", "\\1", rownames(micusp_corpus$documents))

# Checking the summary again, we see the new "Discipline" column.
summary(micusp_corpus)

# Let's begin with the some of things we did using basic functions in "text_intro.R".
# With quanteda, however, we can use built-in functions to make our work much easier.
# We'll start by creating a "document-feature matrix".
# This function creates a data object that stores information we can then extract in various ways.
# Notice the various arguments we can use.
# These give us control over what exactly we want to count.
# We'll call our data object "micusp_dfm".
micusp_dfm <- dfm(micusp_corpus, groups = "Discipline", remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)

# Let's look at the 10 most frequent words in "micusp_dfm" by calling "textstat_frequency".
textstat_frequency(micusp_dfm, n=10)

# We can also coerce those frequency counts into a data frame.
micusp_counts <- as.data.frame(textstat_frequency(micusp_dfm))

# Check the result.
View(micusp_counts)

# We might be interested in knowing what words distinguish one group from another.
# In our corpus, groups are defined by discipline.
# Using the "textstat_keyness" function, we can, for example, comparing English to the rest of the corpus.
# Here we can generate the top 10 keywords.
# We can select a variety of measures, but "lr" is log-likelihood (with a Williams correction as a default).
head(textstat_keyness(micusp_dfm, target = "ENG", measure = "lr"), 10)

# Or we could use biology as our target.
head(textstat_keyness(micusp_dfm, target = "BIO", measure = "lr"), 10)

# What if we wanted to compare English and biology specifically?
# To do that, we would create a separate sub-corpus using the "corpus_subset" function.
micusp_compare <- corpus_subset(micusp_corpus, Discipline %in% c("BIO", "ENG"))

# Again, we need to create a document-feature matrix.
compare_dfm <- dfm(micusp_compare, groups = "Discipline", remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)

# Let's look at the top 10 keywords.
# Note the count in the reference corpus ("n_reference") is much smaller than when we did this the first time.
# That's because our reference corpus consists only of biology texts, rather than texts from all other disciplines.
# Interesting, though, the list is quite similar.
head(textstat_keyness(compare_dfm, target = "ENG", measure = "lr"), 10)

# And for biology.
head(textstat_keyness(compare_dfm, target = "BIO", measure = "lr"), 10)

# If we want to save our keyness data, we can create a data frame.
micusp_keyness <- as.data.frame(textstat_keyness(compare_dfm, target = "ENG", measure = "lr"))

# Check the result.
View(micusp_keyness)

# Note the scientific notion in our p-values column and the many decimal places in the keyness ("G2") column.
# We going to convert these columns.
# The first to 5 decimal places and the other to 2.
micusp_keyness[,"p"] <- round(micusp_keyness[,"p"],5)
micusp_keyness[,"G2"] <- round(micusp_keyness[,"G2"],2)

# Check the result
View(micusp_keyness)

# Save the table to the "output" folder.
write.csv(micusp_keyness, file="output/micusp_keyness.csv", row.names = FALSE)

# Now let's go back to our corpus summary.
summary(micusp_corpus, n=10)

# As we've done with other data, we can coerce the summary into a data frame.
# As we have more than 100 texts, we set "n" (number) greater than what we have (170).
# Otherwise, the data frame will only include the first 100 texts.
micusp_summary <- as.data.frame(summary(micusp_corpus, n=200))

# Check the result.
View(micusp_summary)

# As did in "text_intro.R", we can return the total word count using "sum".
sum(micusp_summary$Tokens)

# We can also get a token count by discipline.
# There are a number of ways of doing this, but one is simply to specify an additional attribute.
# Here we are specifying values in the "Discipline" column.
sum(micusp_summary$Tokens[micusp_summary$Discipline == "BIO"])

# Or we can use the aggregate function to sum "Tokens" by the "Discipline" variable.
aggregate(Tokens ~ Discipline, micusp_summary, sum)

# Similarly, we could calculate the average number of tokens per sentence.
# Here we divide the sum of the "Tokens" column by the sum of the "Sentences" column.
sum(micusp_summary$Tokens)/sum(micusp_summary$Sentences)

# What if we wanted to calculate other information, like the type-to-token ratios for each document?
# For that, we can just create our own, very simple function, which we name "simple_ratio".
# This is almost identical to the functions we wrote in "text_intro.R".
# However, note that we're requiring 2 arguments -- "x" and "y".
simple_ratio <- function(x,y){
  ratio <- x/y
  ratio <- round(ratio, 2)
  return(ratio)}

# Now we want to iterate through our data row by row. We want to "apply" the function.
# Again, just as we did in "text_intro.R", we'll use "mapply".
TypeToken <- mapply(simple_ratio, micusp_summary$Types, micusp_summary$Tokens)

# The resulting vector we can easily append to our "micusp_summary" data frame.
micusp_summary$TypeToken <- TypeToken

# Check the result.
View(micusp_summary)

# Let's create a basic boxplot.
# We'll specify the data frame we're using ("micusp_summary").
# We'll also specify the x-axis (the "Discipline" column) and the y-axis (the "TypeToken" column).
# Finally, we'll tell ggplot2 which type of plot to generate (a box plot) and the theme to use (the minimal theme).
# Note the plus signs at the end of the lines.
# This tells R that more functions are to come, even though there is a closed parenthesis
# The plot will appear in your "Plots" space on the bottom right.
ggplot(micusp_summary, aes(x=Discipline, y=TypeToken)) + 
  geom_boxplot() +
  theme_minimal()

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
