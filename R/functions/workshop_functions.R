

readtext_lite <- function(x){
  text_df <- lapply(x, function(i){
    text <- readLines(i)
    doc_id <- basename(i)
    text <- paste(text, collapse = "\n")
    doc <- data.frame(cbind(doc_id, text), stringsAsFactors = F)
  })
  text_df <- do.call("rbind", text_df)
  return(text_df)
}

effect_size <- function (x) {
  total_a <- sum(x$n_target)
  total_b <- sum(x$n_reference)
  mapply(function (frequency_a, frequency_b) { 
  percent_a <- if(frequency_a == 0) (.5/total_a) else (frequency_a/total_a)
  percent_b <- if(frequency_b == 0) (.5/total_b) else (frequency_b/total_b)
  ratio <- log2(percent_a/percent_b)
  ratio <- round(ratio, 2)
  return(ratio)
}, x$n_target, x$n_reference)
}