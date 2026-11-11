#Question 2
packages <- c(
  "tm",
  "topicmodels",
  "ggplot2",
  "wordcloud",
  "RColorBrewer",
  "slam"
)

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

library(tm)
library(topicmodels)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(slam)

zip_file <- "news.zip"

if (!file.exists(zip_file)) {
  stop("news.zip was not found in the working directory.")
}

extract_folder <- "news_documents"

dir.create(
  extract_folder,
  showWarnings = FALSE
)

unzip(
  zip_file,
  exdir = extract_folder
)

file_paths <- list.files(
  extract_folder,
  pattern = "\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(file_paths) == 0) {
  stop("No text files were found in the ZIP file.")
}

news_content <- lapply(file_paths, function(file) {
  
  lines <- readLines(
    file,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  paste(lines, collapse = " ")
})

news_df <- data.frame(
  file_name = basename(file_paths),
  content = unlist(news_content),
  stringsAsFactors = FALSE
)

cat("\n--- NEWS DATA FRAME ---\n")

print(news_df)

news_corpus <- VCorpus(
  VectorSource(news_df$content)
)

news_corpus <- tm_map(
  news_corpus,
  content_transformer(tolower)
)

news_corpus <- tm_map(
  news_corpus,
  removePunctuation
)

news_corpus <- tm_map(
  news_corpus,
  removeNumbers
)

news_corpus <- tm_map(
  news_corpus,
  removeWords,
  stopwords("english")
)

custom_stopwords <- c(
  "said", "also", "one", "new",
  "would", "could", "will"
)

news_corpus <- tm_map(
  news_corpus,
  removeWords,
  custom_stopwords
)

news_corpus <- tm_map(
  news_corpus,
  stripWhitespace
)

cleaned_content <- sapply(
  news_corpus,
  as.character
)

news_df$cleaned_content <- cleaned_content

cat("\n--- PREPROCESSED DATA ---\n")

print(
  news_df[, c("file_name", "cleaned_content")]
)

dtm <- DocumentTermMatrix(
  news_corpus,
  control = list(
    wordLengths = c(3, Inf)
  )
)

dtm <- removeSparseTerms(
  dtm,
  0.99
)

row_totals <- slam::row_sums(dtm)

valid_documents <- which(row_totals > 0)

dtm <- dtm[valid_documents, ]

valid_file_names <- news_df$file_name[valid_documents]

if (nrow(dtm) < 3 || ncol(dtm) < 2) {
  stop(
    "Not enough usable documents or terms for LDA. ",
    "Check the input data."
  )
}

cat("\nDocument-Term Matrix dimensions:\n")

print(dim(dtm))

number_of_topics <- 3

set.seed(123)

lda_model <- LDA(
  dtm,
  k = number_of_topics,
  method = "Gibbs",
  control = list(
    seed = 123,
    burnin = 1000,
    iter = 2000,
    thin = 100
  )
)

cat("\n--- LDA MODEL ---\n")

print(lda_model)

topic_terms <- posterior(lda_model)$terms

top_k <- 10

top_words <- data.frame()

for (topic in 1:number_of_topics) {
  
  probabilities <- topic_terms[topic, ]
  
  top_indices <- order(
    probabilities,
    decreasing = TRUE
  )
  
  top_indices <- head(
    top_indices,
    top_k
  )
  
  topic_result <- data.frame(
    topic = topic,
    word = names(probabilities)[top_indices],
    probability = probabilities[top_indices]
  )
  
  top_words <- rbind(
    top_words,
    topic_result
  )
}

cat("\n--- TOP WORDS PER TOPIC ---\n")

print(top_words)

document_topics <- posterior(lda_model)$topics

document_topic_df <- data.frame(
  file_name = valid_file_names,
  document_topics,
  check.names = FALSE
)

document_topic_df$dominant_topic <- apply(
  document_topics,
  1,
  which.max
)

cat("\n--- DOCUMENT TOPIC DISTRIBUTION ---\n")

print(document_topic_df)

bar_chart <- ggplot(
  top_words,
  aes(
    x = reorder(word, probability),
    y = probability
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  facet_wrap(
    ~ topic,
    scales = "free"
  ) +
  
  labs(
    title = "Top 10 Words per LDA Topic",
    subtitle = "Word probability distribution for each topic",
    x = "Top Words",
    y = "Word Probability",
    caption = "Source: LDA analysis of news articles"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    strip.text = element_text(
      face = "bold"
    )
  )

print(bar_chart)

ggsave(
  "Q2_Top_Words_Bar_Chart.png",
  plot = bar_chart,
  width = 10,
  height = 7
)

for (topic in 1:number_of_topics) {
  
  topic_data <- top_words[
    top_words$topic == topic,
  ]
  
  png(
    filename = paste0(
      "Q2_Topic_",
      topic,
      "_Wordcloud.png"
    ),
    width = 1000,
    height = 800
  )
  
  wordcloud(
    words = topic_data$word,
    freq = topic_data$probability,
    min.freq = 0,
    max.words = top_k,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2")
  )
  
  title(
    paste("Word Cloud for Topic", topic)
  )
  
  dev.off()
}

write.csv(
  news_df,
  "Q2_Preprocessed_News.csv",
  row.names = FALSE
)

write.csv(
  top_words,
  "Q2_Topic_Word_Distributions.csv",
  row.names = FALSE
)

write.csv(
  document_topic_df,
  "Q2_Document_Topic_Distributions.csv",
  row.names = FALSE
)

cat("\nQuestion 2 completed successfully.\n")

cat("Files created:\n")
cat("Q2_Preprocessed_News.csv\n")
cat("Q2_Topic_Word_Distributions.csv\n")
cat("Q2_Document_Topic_Distributions.csv\n")
cat("Q2_Top_Words_Bar_Chart.png\n")
cat("Q2_Topic_1_Wordcloud.png\n")
cat("Q2_Topic_2_Wordcloud.png\n")
cat("Q2_Topic_3_Wordcloud.png\n")
