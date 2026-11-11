
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

url <- "https://news.ycombinator.com/"

response <- httr::GET(
  url,
  httr::user_agent("Mozilla/5.0"),
  httr::timeout(30)
)

if (httr::status_code(response) != 200) {
  stop("Unable to access Hacker News.")
}

page <- xml2::read_html(
  httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )
)

story_rows <- page %>%
  html_elements("tr.athing")

records <- data.frame(
  title = character(),
  link = character(),
  author = character(),
  points = numeric(),
  comments = numeric(),
  posting_time = character(),
  stringsAsFactors = FALSE
)

for (i in seq_along(story_rows)) {
  
  story <- story_rows[[i]]
  
  title_element <- story %>%
    html_element(".titleline > a")
  
  title <- html_text2(title_element)
  link <- html_attr(title_element, "href")
  
  subtext <- story %>%
    html_element(xpath = "following-sibling::tr[1]")
  
  author <- subtext %>%
    html_element("a") %>%
    html_text2()
  
  score_text <- subtext %>%
    html_element(".score") %>%
    html_text2()
  
  time_text <- subtext %>%
    html_element(".age") %>%
    html_text2()
  
  subtext_links <- subtext %>%
    html_elements("a") %>%
    html_text2()
  
  comment_text <- subtext_links[
    str_detect(
      subtext_links,
      "comment"
    )
  ]
  
  points_value <- str_extract(
    score_text,
    "[0-9]+"
  )
  
  comments_value <- str_extract(
    paste(comment_text, collapse = " "),
    "[0-9]+"
  )
  
  if (length(title) > 0 && !is.na(title) && nzchar(title)) {
    
    records <- bind_rows(
      records,
      data.frame(
        title = title,
        link = ifelse(
          is.na(link),
          NA_character_,
          link
        ),
        author = ifelse(
          length(author) == 0 || is.na(author),
          NA_character_,
          author
        ),
        points = ifelse(
          is.na(points_value),
          NA_real_,
          as.numeric(points_value)
        ),
        comments = ifelse(
          is.na(comments_value),
          0,
          as.numeric(comments_value)
        ),
        posting_time = ifelse(
          length(time_text) == 0 || is.na(time_text),
          NA_character_,
          time_text
        ),
        stringsAsFactors = FALSE
      )
    )
  }
}

records <- records %>%
  distinct(title, .keep_all = TRUE) %>%
  slice_head(n = 30)

if (nrow(records) < 20) {
  stop("Fewer than 20 records were collected.")
}

cat("\n--- SCRAPED HACKER NEWS RECORDS ---\n")

print(records)

cat("\nNumber of records collected:", nrow(records), "\n")

write.csv(
  records,
  "Q4_Raw_Hacker_News_Data.csv",
  row.names = FALSE
)

cat("\nQuestion 4.1 completed successfully.\n")

#Question 4.2
str(records)

summary(records)

dim(records)

colSums(is.na(records))

sum(duplicated(records))

sum(duplicated(records$title))






cleaned_records <- records %>%
  mutate(
    title = str_squish(title),
    link = str_squish(link),
    author = str_squish(author),
    posting_time = str_squish(posting_time),
    points = as.numeric(points),
    comments = as.numeric(comments)
  ) %>%
  distinct(title, .keep_all = TRUE) %>%
  filter(
    !is.na(title), title != "",
    !is.na(link), link != "",
    !is.na(author), author != "",
    !is.na(points),
    !is.na(comments),
    !is.na(posting_time), posting_time != ""
  )

write.csv(
  cleaned_records,
  "Q4_Cleaned_Hacker_News_Data.csv",
  row.names = FALSE
)

dim(cleaned_records)
str(cleaned_records)
colSums(is.na(cleaned_records))