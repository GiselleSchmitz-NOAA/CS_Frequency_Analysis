# Load required packages
required_pkg <- c(
  "here", "XML", "tidyverse", "purrr"
)
pkg_to_install <- required_pkg[!(required_pkg %in%
                                   installed.packages()[, "Package"])]
if (length(pkg_to_install)) install.packages(pkg_to_install)
lapply(required_pkg, library, character.only = TRUE)

#should the files be sorted at this point (like do it multiple times, per science center)
#or should it be sorted based on a column of the xlsx (column D is science center)
# rename file to .zip
xlsx_file <- here::here(
  "download_stock_smart_assessments",
  "Assessment_Summary_Data_allSC.xlsx"
)
zip_file <- sub("xlsx", "zip", xlsx_file)
file.copy(from = xlsx_file, to = zip_file)

# unzip the file
unzip_dir <- here::here(
  "download_stock_smart_assessments",
  "xml_files"
)
ifelse(!dir.exists(unzip_dir), dir.create(unzip_dir), FALSE)
unzip(zip_file, exdir = unzip_dir)
filename <- "sheet1.xml.rels"
rel_path <- file.path(unzip_dir, "xl", "worksheets", "_rels", filename)
rel <- XML::xmlParse(rel_path)
rel <- XML::xmlToList(rel)
rel <- purrr::map_dfr(rel, as.list)
rel <- rel[, c("Id", "Target")]

# Remove duplicate urls
rel <- rel[!duplicated(rel$Target), ]

# Download stock assessments
assessment_dir <- here::here("download_stock_smart_assessments", "AFSCStockAssessment")
ifelse(!dir.exists(assessment_dir), dir.create(assessment_dir), FALSE)
#if (!dir.exists(here::here(folder_name, subfolder_name))) dir.create(here::here(folder_name, subfolder_name))

for (i in seq_along(rel$Target)) {
  download.file(rel$Target[i],
                destfile = file.path(
                  assessment_dir,
                  paste0(rel$Id[i], ".pdf")
                ),
                mode = "wb"
  )
}






#works to this point, then the analysis works separately on an individual pdf, how to combine and apply to all?
#do I need to standardize names to then apply the analysis below to all files?
# Define key terms ---------------------------------------------------------------


keyword <- c(
"citizen science|CS|C.S.",
"community science",
"civic ecology",
"civic science",
"community-based participatory research",
"crowdsourcing",
"participatory action research",
"traditional ecological knowledge|TEK|T.E.K.",
"volunteer monitoring|volunteer|volunteering",
"public participation",
"crowdsourced science",
"volunteers in research",
"app|APP",
"voluntary reporting|voluntary recording",
"voluntary angler diary|angler survey|angling survey"
)

# Create empty database ----------------------------------------------------------------

working_path <- here::here("StockAssessment")
subfolder_path <- list.dirs(path = working_path, full.names = TRUE, recursive = FALSE)
subfolder_name <- list.dirs(path = working_path, full.names = FALSE, recursive = FALSE)

#Create databases

col_name <- c("ID", "Science Center", "File_Path", keyword)
frequency_database <- data.frame(matrix(NA, ncol = length(col_name)))

for (subfolder_id in seq_along(subfolder_path)) {
  file_path <- list.files(subfolder_path[subfolder_id], recursive = FALSE, full.names = TRUE)
  
  for (file_id in seq_along(file_path)) {
    file <- file_path[file_id]
    
    #Create metadata
    docs  <- tm::Corpus(URISource(file),
                        readerControl = list(reader = readPDF)
                        )
    #Convert symbols to space
    toSpace <- tm::content_transformer(
      function(x, pattern) gsub(pattern, " ", x)
    )
    docs <- tm::tm_map(docs, toSpace, "/")
    docs <- tm::tm_map(docs, toSpace, "@")
    docs <- tm::tm_map(docs, toSpace, "\\|")
    
    #Convert text to lower case
    docs <- tm::tm_map(
      docs, 
      content_transformer(tolower)
    )
    #Remove puncutation
    docs <- tm::tm_map(docs, removePunctuation)
    
    #Eliminate extra white spaces
    docs <- tm::tm_map(docs, stripWhitespace)
    
    c()
    for(keyword_id in seq_along(keyword)){
      frequency[keyword_id] <- sum(stringr::str_count(docs[[1]]$content, paste("\\b", keyword[keyword_id], "\\b", sep = "")))
      
    }
    if (subfolder_id == 1 & file_id ==1) {
      frequency_database[1, ] <- c(NA, subfolder_name[subfolder_id], file_path[file_id], frequency)
    } else {
      frequency_database <- rbind(
        frequency_database,
        c(NA, subfolder_name[subfolder_id], file_path[file_id], frequency)
      )
    }
  }
}

frequency_database$ID <- 1:nrow(frequency_database)

frequency_database[, 4:ncol(frequency_database)] <- sapply(frequency_database[, 4:ncol(frequency_database)], as.numeric)

frequency_database <- rbind(
  frequency_database,
  c(NA, NA, NA, apply(frequency_database[, 4:ncol(frequency_database)], 2, sum))
)
frequency_database[nrow(frequency_database), "ID"] <- "Sum"

xlsx_path <- here::here("TextAnalysis", "termfrequency.xlsx")
frequency_database <- xlsx::read.xlsx(file = xlsx_path, sheetName = "frequency")

word_frequency <- as.matrix(frequency_database[frequency_database$ID == "Sum", 4:ncol(frequency_database)])

xlabels <- c(
  "citizen science",
  "community science",
  "civic ecology",
  "civic science",
  "community-based participatory research",
  "crowdsourcing",
  "participatory action research",
  "traditional ecological knowledge",
  "volunteer monitoring",
  "public participation",
  "crowdsourced science",
  "volunteers in research",
  "app",
  "voluntary reporting",
  "voluntary angler diary"
)
  
