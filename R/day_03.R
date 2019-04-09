
library(tidyverse)

# Now we are going to load file containing all of the metadata for our corpus.
# For this, we use one of R's many read functions. This one is a specific
# implementation for comma-separated-value (or csv) files.
micusp_meta <- read_csv("data/academic/MICUSP_meta.csv")

# Let's start by making a histogram, which is a common visualization
# for checking how data is distributed.
# Our data has a column for tokens_total.
# Those totals were calculated in the previous workshop using uanteda's ntoken function.
ggplot(micusp_meta, aes(tokens_total)) +
  geom_histogram()

# Note that ggplot gives us a warning about choosing 'binwidth'.
# This is the option for telling ggplot the size the buckets to group our values into.
# One common way to determine binwidth is with the Freedman–Diaconis rule:
# a rule based on the interquartile range, which we can calculate using IQR()
# and the number of observations, which we can calculate using length()

IQR(micusp_meta$tokens_total)
length(micusp_meta$tokens_total)

# We'll store our value as "bw_tokens".
bw_tokens <- 2 * IQR(micusp_meta$tokens_total) / length(micusp_meta$tokens_total)^(1/3)

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
  labs(x="Tokens", y = "Count") +
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

# Now all our normalized values are in the column "norm_freq" and the CATEGORY
# of that frequency (either "hedges_norm" or "boosters_norm") is in the
# "token_class" column

View(hb_ratios)

# To compare the "spread" of a numeric variable, a boxplot can be very useful to
# show distributions.
ggplot(hb_ratios, aes(x = paper_type, y = norm_freq, fill = token_class)) +
  geom_boxplot()

# Try varying the x axis to compare student gender or discipline category

### YOUR CODE HERE

# We're using the x and fill aesthetics to divide our data along 2 categories -
# you could add a third division by using facet_wrap() as well

ggplot(hb_ratios, aes(x = discipline_cat, y = norm_freq, fill = token_class)) +
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

# When we plot the results, note the addition of the color aesthetic.
ggplot(pronouns_joined, aes(x = year, y = counts_permil, color = id)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), 
              size = 0.5)

# The data looks quite messy in the early nineteenth century --
# reflecting far less robust data than for later years.
# To truncate our plot, we can subset it using subset().
# For example, if we want the plot to bein with the twentieth century we can
# specify that the year column must be greater than 1899.
ggplot(subset(pronouns_joined, year > 1899), aes(x = year, y = counts_permil, color = id)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), 
              size = 0.5)

# We might also note that the y-axis in not starting at 0,
# which is not usually good practice.
# To set it to something specific, we can use ylim().
ggplot(subset(pronouns_joined, year > 1899), aes(x = year, y = counts_permil, color = id)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), 
              size = 0.5) +
  ylim(0,12000)


# Plotting factors...
micusp_ds <- read_csv("data/tables/micusp_ds.csv")

micusp_sub <- micusp_ds %>% select(-text_id, -discipline_cat)
micusp_factors <- factanal(micusp_sub, 3, rotation="promax") 
f_loadings <- as.data.frame(unclass(micusp_factors$loadings))
f_loadings <- rownames_to_column(f_loadings, "cluster")


ggplot(f_loadings, aes(x = reorder(cluster, Factor1), y = Factor1)) +
    geom_col() +
    coord_flip()

ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_point()

ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_point() +
  geom_text()

ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_point() +
  geom_text(vjust = 0)

ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_text()

ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_text() +
  theme_bw()

ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_text() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(f_loadings, aes(x = Factor1, y = Factor2, label = cluster)) +
  geom_text() +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
