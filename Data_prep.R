# movies from TMDB
setwd("~/Desktop")
tmdb_csv <- read.csv("10k_movies.csv")
View(tmdb_csv)

################################################################################
# BASIC DATA INSPECTION & APPLYING EXCLUSION CRITERIA
df <- read.csv("hannes_data.csv")
View(df)
nrow(df)

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

#check for duplicates
df[duplicated(df), ]
sum(duplicated(df))
df_duplicates <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
View(df_duplicates)

# get rid of duplicates
df_cleaned <- df %>%
  distinct()

# remove movies with no release year, director or title
df_cleaned <- df_cleaned %>%
  filter(if_all(c(title, director, release_year, runtime), ~ . != "" & !is.na(.)))

# remove movies with runtime below 20 minutes
df_cleaned <- df_cleaned %>%
  filter(runtime >= 20)

# remove tags column
df_cleaned <- df_cleaned %>%
  select(-tags)

################################################################################
### REFORMAT THE RATING COUNT & POLARIZATION MEASURE ###
# Split 'rating_counts' string column into individual entries
df_cleaned <- df_cleaned %>%
  separate(rating_counts, into = paste0("X", seq(0.5, 5.0, by = 0.5)), sep = "---") %>%
  mutate(across(starts_with("X"), ~ case_when(
    str_detect(., "No ratings for this amount of stars") ~ "0",
    TRUE ~ str_extract(., "\\d{1,3}(,\\d{3})*|\\d+")
  ))) %>%
  mutate(across(starts_with("X"), ~ as.integer(gsub(",", "", .))))

# drop rows with NAs in rating counts
df_cleaned <- df_cleaned %>%
  drop_na(X0.5:X5)

# Calculate the total number of ratings for each movie
df_cleaned <- df_cleaned %>%
  mutate(total_ratings = rowSums(across(c("X0.5", "X1", "X1.5", "X2", "X2.5",
                                          "X3", "X3.5", "X4", "X4.5", "X5"))))
# drop movies with ratings below 200
df_cleaned <- df_cleaned %>%
  filter(total_ratings >= 200) # set to 200 based on power analysis

# POLARIZATION/ENTROPY MEASURE
rating_cols <- c("X0.5", "X1", "X1.5", "X2", "X2.5",
                 "X3", "X3.5", "X4", "X4.5", "X5")

df_cleaned <- df_cleaned %>%
  rowwise() %>%
  mutate(
    polarization = {
      counts <- c_across(all_of(rating_cols))
      probs <- counts / sum(counts)

      # Avoid log(0) — remove 0s
      probs <- probs[probs > 0]

      -sum(probs * log(probs))
    }
  ) %>%
  ungroup()
################################################################################
# REFORMAT THE TAGS COLUMN
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)

# Define safe parser
safe_parse <- safely(function(x) fromJSON(x) %>% as.list())

# Apply safe parsing to each 'tags' entry
parsed <- map(df_cleaned$tags, safe_parse)

# Replace errors with NA-filled rows
filled_results <- map(parsed, function(x) {
  if (is.null(x$error)) {
    return(x$result)
  } else {
    return(list(
      female_protagonist = NA,
      lgbtq_protagonist = NA,
      white_protagonist = NA,
      `political/social_or_moral_topic` = NA,
      experimental = NA
    ))
  }
})

# Bind the parsed tag info to a new data frame
parsed_df <- bind_rows(filled_results)

# Combine with your original df
df_cleaned <- bind_cols(df_cleaned, parsed_df)

# Move new columns next to 'tags'
df_cleaned <- df_cleaned %>%
  relocate(female_protagonist, lgbtq_protagonist, white_protagonist,
           `political/social_or_moral_topic`, experimental, .after = tags)

# check for NAs
colSums(is.na(df_cleaned[c(
  "female_protagonist",
  "lgbtq_protagonist",
  "white_protagonist",
  "political/social_or_moral_topic",
  "experimental"
)]))

################################################################################
# SAVE DF AS CSV FILE
write.csv(df_cleaned, "df_cleaned.csv", row.names = FALSE)


