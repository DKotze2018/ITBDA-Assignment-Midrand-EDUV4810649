#Question 1
zip_file <- "map_reduce_test_docs.zip"

if (!file.exists(zip_file)) {
  stop("map_reduce_test_docs.zip was not found in the working directory.")
}

extract_folder <- "mapreduce_documents"

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

documents <- lapply(file_paths, function(file) {
  readLines(
    file,
    warn = FALSE,
    encoding = "UTF-8"
  )
})

names(documents) <- basename(file_paths)

mapreduce_config <- list(
  framework = "R MapReduce Simulation",
  input_format = "Text Files",
  output_format = "CSV",
  mapper = "Keyword Mapper",
  reducer = "Keyword Reducer",
  case_sensitive = FALSE,
  keyword = NULL
)

mapper <- function(documents, keyword) {
  
  keyword <- tolower(trimws(keyword))
  
  if (nchar(keyword) == 0) {
    stop("The keyword cannot be empty.")
  }
  
  mapped_results <- data.frame(
    document = character(),
    row = integer(),
    column = integer(),
    keyword = character(),
    stringsAsFactors = FALSE
  )
  
  for (doc_name in names(documents)) {
    
    lines <- documents[[doc_name]]
    
    for (line_number in seq_along(lines)) {
      
      current_line <- tolower(lines[line_number])
      
      positions <- gregexpr(
        keyword,
        current_line,
        fixed = TRUE
      )[[1]]
      
      if (positions[1] != -1) {
        
        for (position in positions) {
          
          mapped_results <- rbind(
            mapped_results,
            data.frame(
              document = doc_name,
              row = line_number,
              column = position,
              keyword = keyword,
              stringsAsFactors = FALSE
            )
          )
        }
      }
    }
  }
  
  return(mapped_results)
}

reducer <- function(mapped_results) {
  
  if (nrow(mapped_results) == 0) {
    return(mapped_results)
  }
  
  reduced_results <- mapped_results[
    order(
      mapped_results$document,
      mapped_results$row,
      mapped_results$column
    ),
  ]
  
  rownames(reduced_results) <- NULL
  
  return(reduced_results)
}

keyword <- readline(
  prompt = "Enter the keyword to search for: "
)

if (nchar(trimws(keyword)) == 0) {
  stop("The keyword cannot be empty.")
}

mapreduce_config$keyword <- keyword

cat("\n--- MAPPER OUTPUT ---\n")

mapped_output <- mapper(
  documents,
  keyword
)

print(mapped_output)

cat("\n--- REDUCER OUTPUT ---\n")

final_output <- reducer(
  mapped_output
)

print(final_output)

write.csv(
  final_output,
  "MapReduce_Keyword_Results.csv",
  row.names = FALSE
)

cat("\n--- MAPREDUCE CONFIGURATION ---\n")

print(mapreduce_config)

cat("\nMapReduce process completed successfully.\n")

