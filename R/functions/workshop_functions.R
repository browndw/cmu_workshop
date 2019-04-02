# Helper functions for the workshop

# Replaces the readtext::readtext function that reads a lists of text files into
# a data frame
readtext_lite <- function(paths) {
  # Get a vector of the file basenames
  doc_ids <- basename(paths)
  # Create a vector collapsing each text file into one element in a character
  # vector
  texts <- vapply(paths, function(i) paste(readLines(i), collapse = "\n"), 
                  FUN.VALUE = character(1))
  text_df <- data.frame(doc_id = doc_ids, text = texts, stringsAsFactors = FALSE)
  return(text_df)
}

# Take a target column and a reference column, and return an effect size
effect_size <- function (n_target, n_reference) {
  total_a <- sum(n_target)
  total_b <- sum(n_reference)
  percent_a <- ifelse(n_target == 0, 0.5 / total_a, n_target/total_a)
  percent_b <- ifelse(n_reference == 0, 0.5 / total_b, n_reference/total_b)
  ratio <- log2(percent_a / percent_b)
  return(ratio)
}
