####################### LOAD DATA & LIBRARIES ##################################
df <- read.csv("hannes_data.csv")
df <- df[-27, ]
View(df)
nrow(df) # 5587

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
################################### BASICS #####################################
## DUPLICATES ##
df_cleaned <- df %>%
  distinct()
nrow(df_cleaned) # 5489

## RELEASE YEAR, DIRECTOR & TITLE ##
df_cleaned <- df_cleaned %>%
  filter(if_all(c(title, director, release_year, runtime), ~ . != "" & !is.na(.)))
nrow(df_cleaned) # 5155

## RUN TIME < 20 MINUTES ##
df_cleaned <- df_cleaned %>%
  filter(runtime >= 20)
nrow(df_cleaned) # 4589
######################## FEATURE ENGINEERING & FORMATING #######################
## RATING COUNTS ##
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
nrow(df_cleaned) # 4530

# Calculate the total number of ratings for each movie
df_cleaned <- df_cleaned %>%
  mutate(total_ratings = rowSums(across(c("X0.5", "X1", "X1.5", "X2", "X2.5",
                                          "X3", "X3.5", "X4", "X4.5", "X5"))))

# drop movies with ratings below 200
#df_cleaned <- df_cleaned %>%
#filter(total_ratings >= 2) # set to 200 based on power analysis

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

## TAGS COLUMN ##
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)

# Define safe parser
safe_parse <- safely(function(x) fromJSON(x) %>% as.list())
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

## UNIFIED MARGINALIZED PROTAGONIST VARIABLE ##
df_cleaned <- df_cleaned %>%
  mutate(
    marginalized_protagonist = case_when(
      female_protagonist == "Yes" |
        lgbtq_protagonist == "Yes" |
        white_protagonist == "No" ~ TRUE,

      female_protagonist == "Unknown" |
        lgbtq_protagonist == "Unknown" |
        white_protagonist == "Unknown" ~ NA,

      TRUE ~ FALSE
    )
  )

## BINARY PREDICTOR FOR MORAL/SOCIAL THEMES ##
df_cleaned <- df_cleaned %>%
  mutate(
    moral_themes = case_when(
      `political/social_or_moral_topic` == "No" ~ FALSE,
      `political/social_or_moral_topic` == "Unknown" ~ NA,
      TRUE ~ TRUE
    )
  #) %>%
  #filter(!is.na(moral_themes))

nrow(df_cleaned)

#nrow(df_cleaned) # 4481
################################################################################
## EXTRA LAST MINUTE STUFF ##
# EXPERIMENTAL COLUMN #
library(dplyr)

#df_cleaned <- df_cleaned %>%
  #filter(experimental != "Unknown")
#nrow(df_cleaned) # 4395

################################################################################
## SAVE FILE ##
write.csv(df_cleaned, "df_cleaned.csv", row.names = FALSE)