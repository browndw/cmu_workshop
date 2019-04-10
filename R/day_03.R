library(tidyverse)

# Now we are going to load file containing all of the metadata for our corpus.
# For this, we use one of R's many read functions. This one is a specific
# implementation for comma-separated-value (or csv) files.
micusp_meta <- read_csv("data/academic/MICUSP_meta.csv")

# Let's start by making a histogram, which is a common visualization for
# checking how data is distributed. Our data has a column for tokens_total.
# Those totals were calculated in the previous workshop using uanteda's ntoken
# function.
ggplot(micusp_meta, aes(tokens_total)) +
  geom_histogram()

# Note that ggplot gives us a warning about choosing 'binwidth'.
#
# This is the option for telling ggplot the size the buckets to group our values
# into. One common way to determine binwidth is with the Freedman–Diaconis rule:
# a rule based on the interquartile range, which we can calculate using IQR()
# and the number of observations, which we can calculate using length()

IQR(micusp_meta$tokens_total)
length(micusp_meta$tokens_total)

# We'll store our value as "bw_tokens".
bw_tokens <- 2 * IQR(micusp_meta$tokens_total) / length(micusp_meta$tokens_total)^(1 / 3)

# Now we can include that information in our plot 
ggplot(micusp_meta, aes(tokens_total)) +
  geom_histogram(binwidth = bw_tokens)

# We can also alter the look of our plot using another theme.
ggplot(micusp_meta, aes(tokens_total)) +
  geom_histogram(binwidth = bw_tokens) +
  theme_classic()

# If we want to change the appearance of the bars, we can change
# their fill and line color.
ggplot(micusp_meta, aes(tokens_total)) +
  geom_histogram(fill = "white", color = "black", binwidth = bw_tokens) +
  theme_classic()

# The axis labels can be changed using labs().
ggplot(micusp_meta, aes(tokens_total)) +
  geom_histogram(fill = "white", color = "black", binwidth = bw_tokens) +
  labs(x = "Tokens", y = "Count") +
  theme_classic()

# Try to create a histogram for the number of sentences in each file: sentences_total.
# Remeber that you'll need to calculate a new "bandwidth" for sentence counts.

### YOUR CODE HERE

# Now let's try let's return to the hedges and boosters data we generated
# in the previous workshop.
# First, we can read in the counts.
micusp_hb <- read_csv("data/tables/micusp_hb.csv")

# It would be useful to attach these statistics to our
# original corpus metadata so we can visualize how hedges and boosters differ
# across different categories. We'll use left_join() to connect them by the
# shared "text_id" column

hb_joined <- micusp_hb %>% 
  left_join(micusp_meta, by = "text_id")

hb_joined

# gather() does a bit of table rearranging that will let us compare
# hedges/boosters as different groups of the same kind of measure. Don't worry
# about understanding that quite yet - but do take a look at how it changes
# the table!
hb_ratios <- hb_joined %>% 
  gather(token_class, norm_freq, hedges_norm:boosters_norm)

# Whenever we want to regroup our graph by something like color or facets, we
# need to make sure that the group id (int his case, "token_class") is in its
# own column.

# Now all our normalized values are in the column "norm_freq" and the CATEGORY
# of that frequency (either "hedges_norm" or "boosters_norm") is in the
# "token_class" column

View(hb_ratios)

# To compare the "spread" of a numeric variable, a boxplot can be very useful to
# show distributions.
ggplot(hb_ratios, aes(x = paper_type, y = norm_freq, fill = token_class)) +
  geom_boxplot()

# Try varying the x axis to compare student gender

### YOUR CODE HERE

# Now vary it to compare the disciplines

### YOUR CODE HERE

# We're using the x and fill aesthetics to divide our data along 2 categories -
# you could add a third division by using facet_wrap() as well

ggplot(hb_ratios, aes(x = paper_discipline, y = norm_freq, fill = token_class)) +
  geom_boxplot() +
  facet_wrap(~ student_gender)

# This looks heinous. Flip the coordinates, and try to reorder() the x axis to
# order disciplines by the frequency of these tokens

### YOUR CODE HERE

# Now let's try plotting some time-series data.
# The data in this table comes from Google Books: https://books.google.com/ngrams#
# It includes counts for the phrase "witch hunt" and "witch hunts" from 1900-2009.
witch_hunt <- read_csv("data/tables/witch_hunt.csv")

# Let's begin with a basic scatter plot.
ggplot(witch_hunt, aes(x = year, y = counts_permil)) +
  geom_point()

# With time-series data, we often want to add a regression line.
# In ggplot2, this is done with geom_smooth().
# A linear model can be applied by specifying "lm" as the method.
ggplot(witch_hunt, aes(x = year, y = counts_permil)) +
  geom_point() +
  geom_smooth(method = "lm")

# Note that ggplot defaults to adding a 95% confidence interval, as well.
# If we wanted to add a smothing function to our regression
# like a generalized additive model or "gam".
ggplot(witch_hunt, aes(x = year, y = counts_permil)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

# We can also specify the color of our regression line...
ggplot(witch_hunt, aes(x = year, y = counts_permil)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              color = "tomato")

# ... the size of the line...
ggplot(witch_hunt, aes(x = year, y = counts_permil)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), 
              color = "tomato", size = 0.5)

# ... and the type of line...
ggplot(witch_hunt, aes(x = year, y = counts_permil)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), 
              color = "tomato", size = 0.5, linetype = "dashed")

# Now we'll try something similar with personal pronouns.
# We'll read in a table for the first person singular.
person_1st_sing <- read_csv("data/tables/person_1st_sing.csv")

# Let's see what that looks like.
ggplot(person_1st_sing, aes(x = year, y = counts_permil)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), 
              size = 0.5)

# We can read in the tables for the 1st person plural & the 2nd person
person_1st_pl <- read_csv("data/tables/person_1st_pl.csv")
person_2nd <- read_csv("data/tables/person_2nd.csv")

# Next, we can combine our counts into a single data frame.
# To do this, we use bind_rows(), which stacks data frames top to bottom,
# as long as they have the same columns!!!!
# Also, we'll use .id to add a column that idenfies where the data originated --
# creating a grouping variable that we can use in our aesthetics.
pronouns_joined <- bind_rows(person_1st_sing, person_1st_pl, person_2nd, .id = "id")

pronouns_joined

# First plot the points without grouping them by color, and add a smooth line

ggplot(pronouns_joined, aes(x = year, y = counts_permil)) +
  geom_point() +
  geom_smooth()

# When we plot the results, note the addition of the color aesthetic.
ggplot(pronouns_joined, aes(x = year, y = counts_permil, color = id)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

# The data looks quite messy in the early nineteenth century --
# reflecting far less robust data than for later years.
# To truncate our plot, we can filter it using filter().
# For example, if we want the plot to bein with the twentieth century we can
# specify that the year column must be greater than 1899.
ggplot(filter(pronouns_joined, year > 1899), aes(x = year, y = counts_permil, color = id)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))

# We might also note that the y-axis in not starting at 0, which is not always
# good practice. To set it to something specific, we can use ylim().
ggplot(subset(pronouns_joined, year > 1899), aes(x = year, y = counts_permil, color = id)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  ylim(0, 12000)

# Plotting factors ----
# Load in a file containing the results from a DocuScope analysis of MISUCP.
# DocuScope is large and detailed dictionary that groups words and phrases
# by their rhetorical functions.
# Here are the counts of the largest groupings (so the least amount of detail).
micusp_ds <- read_csv("data/tables/micusp_ds.csv")

# We only want the the columns with numerical data.
# So we're going to drop the text_id and discipline_cat columns.
micusp_sub <- micusp_ds %>% select(-text_id, -discipline_cat)

# Now we're going to run factor analysis.
# Factor analysis looks for variables that correlate either
# positively or negatatively with each other and groups them into factors.
# It is a common method for reducing complexity in data.
# Here, we're going to generate 3 factors and use a promax rotation.
# A short introduction to these techniques is here:
# https://www.statmethods.net/advstats/factor.html
micusp_factors <- factanal(micusp_sub, 3, rotation = "promax")

# From that analysis, we want to extract the loadings,
# which can plot in various ways.
f_loadings <- as.data.frame(unclass(micusp_factors$loadings))

# Before we plot, we want to move the names of the rows
# to a new column that we can call "cluster".
f_loadings <- rownames_to_column(f_loadings, "cluster")

# Now let's look at a quick plot of the first factor.
ggplot(f_loadings, aes(x = cluster, y = Factor1)) +
    geom_col()

# Flip the axes so the cluster labels are legible

### YOUR CODE HERE

# Now reorder cluster based on the size of Factor1

### YOUR CODE HERE

ggplot(f_loadings, aes(x = Factor1, y = Factor2)) +
  geom_point()

ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_point() +
  geom_text()

ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_label()

# There are many packages that extend ggplot2 by adding new geometries. One of
# them is 'ggrepel', which will try to keep text labels from overlapping.

library(ggrepel)
ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_label_repel()

# Check the help file for geom_label_repel() to see how we can tweak the boxes
# to have more padding around them

?geom_label_repel

ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_label_repel(box.padding = 0.5)


ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_text()

ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_label_repel()
