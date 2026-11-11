#Question 3.1
packages <- c("rvest", "httr", "xml2", "stringr", "dplyr")

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

library(rvest)
library(httr)
library(xml2)
library(stringr)
library(dplyr)

articles <- data.frame(
  page_name = c(
    "Air Pollution",
    "Climate Change",
    "Physical Activity",
    "Healthy Diet"
  ),
  url = c(
    "https://www.who.int/health-topics/air-pollution",
    "https://www.who.int/health-topics/climate-change",
    "https://www.who.int/health-topics/physical-activity",
    "https://www.who.int/health-topics/healthy-diet"
  ),
  stringsAsFactors = FALSE
)

scrape_page <- function(url) {
  
  response <- httr::GET(
    url,
    httr::user_agent("Mozilla/5.0"),
    httr::timeout(30)
  )
  
  if (httr::status_code(response) != 200) {
    warning(paste("Unable to access:", url))
    return("")
  }
  
  page <- xml2::read_html(
    httr::content(
      response,
      as = "text",
      encoding = "UTF-8"
    )
  )
  
  paragraphs <- page %>%
    rvest::html_elements("p") %>%
    rvest::html_text2()
  
  paragraphs <- paragraphs[
    nchar(trimws(paragraphs)) > 30
  ]
  
  paste(paragraphs, collapse = " ")
}

article_content <- character(nrow(articles))

for (i in seq_len(nrow(articles))) {
  
  cat("Collecting:", articles$page_name[i], "\n")
  
  article_content[i] <- scrape_page(
    articles$url[i]
  )
  
  Sys.sleep(3)
}

news_data <- data.frame(
  page_name = articles$page_name,
  url = articles$url,
  content = article_content,
  stringsAsFactors = FALSE
)

news_data <- news_data[
  nchar(trimws(news_data$content)) > 0,
]

if (nrow(news_data) == 0) {
  stop("No website content was collected.")
}

news_data$cleaned_content <- news_data$content

news_data$cleaned_content <- str_replace_all(
  news_data$cleaned_content,
  "\\[[0-9]+\\]",
  ""
)

news_data$cleaned_content <- str_replace_all(
  news_data$cleaned_content,
  "[^[:alnum:] .,!?'-]",
  " "
)

news_data$cleaned_content <- str_squish(
  news_data$cleaned_content
)

cat("\n--- SCRAPED DATA FRAME ---\n")

print(
  news_data[, c("page_name", "url")]
)

cat("\n--- CLEANED WEBSITE CONTENT ---\n")

print(
  news_data[, c("page_name", "cleaned_content")]
)

write.csv(
  news_data,
  "Q3_Scraped_Website_Data.csv",
  row.names = FALSE
)

cat("\nQuestion 3.1 completed successfully.\n")

#Question 3.2

packages <- c("tm", "stringr", "dplyr", "slam")

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

library(tm)
library(stringr)
library(dplyr)
library(slam)

all_sentences <- data.frame(
  article_name = character(),
  sentence = character(),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(news_data))) {
  
  sentences <- unlist(
    strsplit(
      news_data$cleaned_content[i],
      "(?<=[.!?])\\s+",
      perl = TRUE
    )
  )
  
  sentences <- str_squish(sentences)
  
  sentences <- sentences[
    nchar(sentences) >= 50
  ]
  
  if (length(sentences) > 0) {
    
    article_sentences <- data.frame(
      article_name = news_data$page_name[i],
      sentence = sentences,
      stringsAsFactors = FALSE
    )
    
    all_sentences <- rbind(
      all_sentences,
      article_sentences
    )
  }
}

if (nrow(all_sentences) < 2) {
  stop("Not enough sentences were collected for summarisation.")
}

stop_words <- tm::stopwords("english")

get_words <- function(text) {
  
  words <- unlist(
    str_extract_all(
      tolower(text),
      "[a-z]{3,}"
    )
  )
  
  words <- words[
    !words %in% stop_words
  ]
  
  words
}

all_words <- unlist(
  lapply(
    all_sentences$sentence,
    get_words
  )
)

word_frequency <- as.data.frame(
  sort(
    table(all_words),
    decreasing = TRUE
  )
)

names(word_frequency) <- c(
  "word",
  "frequency"
)

word_frequency$frequency <- as.numeric(
  word_frequency$frequency
)

cat("\n--- MOST FREQUENT WORDS ---\n")

print(
  head(word_frequency, 15)
)

sentence_scores <- all_sentences

sentence_scores$frequency_score <- sapply(
  sentence_scores$sentence,
  function(sentence) {
    
    words <- get_words(sentence)
    
    if (length(words) == 0) {
      return(0)
    }
    
    word_counts <- word_frequency$frequency[
      match(words, word_frequency$word)
    ]
    
    mean(word_counts, na.rm = TRUE)
  }
)

frequency_summary <- sentence_scores %>%
  group_by(article_name) %>%
  arrange(
    desc(frequency_score),
    .by_group = TRUE
  ) %>%
  slice_head(n = 3) %>%
  ungroup()

cat("\n--- FREQUENCY-BASED SUMMARIES ---\n")

print(
  frequency_summary[
    ,
    c(
      "article_name",
      "sentence",
      "frequency_score"
    )
  ]
)

sentence_corpus <- VCorpus(
  VectorSource(all_sentences$sentence)
)

sentence_dtm <- DocumentTermMatrix(
  sentence_corpus,
  control = list(
    tolower = TRUE,
    removePunctuation = TRUE,
    removeNumbers = TRUE,
    stopwords = TRUE,
    wordLengths = c(3, Inf)
  )
)

sentence_totals <- slam::row_sums(sentence_dtm)

valid_sentences <- which(
  sentence_totals > 0
)

sentence_dtm <- sentence_dtm[
  valid_sentences,
]

tfidf_sentences <- all_sentences[
  valid_sentences,
]

if (ncol(sentence_dtm) == 0) {
  stop("No useful terms remained for TF-IDF summarisation.")
}

tfidf_dtm <- weightTfIdf(
  sentence_dtm,
  normalize = FALSE
)

tfidf_scores <- slam::row_sums(
  tfidf_dtm
) / pmax(
  slam::row_sums(sentence_dtm),
  1
)

tfidf_sentences$tfidf_score <- as.numeric(
  tfidf_scores
)

tfidf_summary <- tfidf_sentences %>%
  group_by(article_name) %>%
  arrange(
    desc(tfidf_score),
    .by_group = TRUE
  ) %>%
  slice_head(n = 3) %>%
  ungroup()

cat("\n--- TF-IDF-BASED SUMMARIES ---\n")

print(
  tfidf_summary[
    ,
    c(
      "article_name",
      "sentence",
      "tfidf_score"
    )
  ]
)

write.csv(
  all_sentences,
  "Q3_Extracted_Sentences.csv",
  row.names = FALSE
)

write.csv(
  word_frequency,
  "Q3_Word_Frequencies.csv",
  row.names = FALSE
)

write.csv(
  frequency_summary,
  "Q3_Frequency_Summaries.csv",
  row.names = FALSE
)

write.csv(
  tfidf_summary,
  "Q3_TFIDF_Summaries.csv",
  row.names = FALSE
)

cat("\nQuestion 3.2 completed successfully.\n")

#Question 3.3


if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)
library(dplyr)

word_plot <- ggplot(
  head(word_frequency, 15),
  aes(
    x = reorder(word, frequency),
    y = frequency
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "15 Most Frequent Words in WHO Website Content",
    x = "Words",
    y = "Frequency"
  ) +
  theme_minimal()

print(word_plot)

ggsave(
  "Q3_Figure_3.1_Word_Frequencies.png",
  plot = word_plot,
  width = 10,
  height = 6,
  dpi = 300
)

frequency_scores <- frequency_summary %>%
  group_by(article_name) %>%
  summarise(
    average_score = mean(frequency_score),
    .groups = "drop"
  ) %>%
  mutate(
    technique = "Frequency-Based"
  )

tfidf_scores_summary <- tfidf_summary %>%
  group_by(article_name) %>%
  summarise(
    average_score = mean(tfidf_score),
    .groups = "drop"
  ) %>%
  mutate(
    technique = "TF-IDF"
  )

comparison_data <- bind_rows(
  frequency_scores,
  tfidf_scores_summary
)

comparison_plot <- ggplot(
  comparison_data,
  aes(
    x = article_name,
    y = average_score
  )
) +
  geom_col() +
  facet_wrap(
    ~ technique,
    scales = "free_y"
  ) +
  labs(
    title = "Average Sentence Scores by Summarisation Technique",
    x = "WHO Website Page",
    y = "Average Sentence Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    )
  )

print(comparison_plot)

ggsave(
  "Q3_Figure_3.2_Summarisation_Comparison.png",
  plot = comparison_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cat("\nQuestion 3.3 completed successfully.\n")
