# Ciphers
rm(list=ls())
library(tidyverse)
library(cipheR)
library(igraph)

dictionary <- read.csv("/Users/jimmystluka/Downloads/Coding Projects/Scrabble_TWL06.csv")

# Create the Caesar cipher dictionary
cipher_dict <- c(
  "a" = "b", "b" = "c", "c" = "d", "d" = "e",
  "e" = "f", "f" = "g", "g" = "h", "h" = "i",
  "i" = "j", "j" = "k", "k" = "l", "l" = "m",
  "m" = "n", "n" = "o", "o" = "p", "p" = "q",
  "q" = "r", "r" = "s", "s" = "t", "t" = "u",
  "u" = "v", "v" = "w", "w" = "x", "x" = "y",
  "y" = "z", "z" = "a"
)

# Initialize the data frame
all_ciphers <- dictionary

# Loop through the 25 cipher iterations
for (i in 1:25) {
  previous_word <- if (i == 1) "word" else paste0("word", i - 1)
  new_word <- paste0("word", i)
  
  all_ciphers[[new_word]] <- sapply(
    all_ciphers[[previous_word]],
    caesar,
    n = 1,
    dict = cipher_dict
  )
}

# find matching words between ciphers
{
word1_list <- intersect(all_ciphers$word, all_ciphers$word1)
word2_list <- intersect(all_ciphers$word, all_ciphers$word2)
word3_list <- intersect(all_ciphers$word, all_ciphers$word3)
word4_list <- intersect(all_ciphers$word, all_ciphers$word4)
word5_list <- intersect(all_ciphers$word, all_ciphers$word5)
word6_list <- intersect(all_ciphers$word, all_ciphers$word6)
word7_list <- intersect(all_ciphers$word, all_ciphers$word7)
word8_list <- intersect(all_ciphers$word, all_ciphers$word8)
word9_list <- intersect(all_ciphers$word, all_ciphers$word9)
word10_list <- intersect(all_ciphers$word, all_ciphers$word10)
word11_list <- intersect(all_ciphers$word, all_ciphers$word11)
word12_list <- intersect(all_ciphers$word, all_ciphers$word12)
word13_list <- intersect(all_ciphers$word, all_ciphers$word13)
word14_list <- intersect(all_ciphers$word, all_ciphers$word14)
word15_list <- intersect(all_ciphers$word, all_ciphers$word15)
word16_list <- intersect(all_ciphers$word, all_ciphers$word16)
word17_list <- intersect(all_ciphers$word, all_ciphers$word17)
word18_list <- intersect(all_ciphers$word, all_ciphers$word18)
word19_list <- intersect(all_ciphers$word, all_ciphers$word19)
word20_list <- intersect(all_ciphers$word, all_ciphers$word20)
word21_list <- intersect(all_ciphers$word, all_ciphers$word21)
word22_list <- intersect(all_ciphers$word, all_ciphers$word22)
word23_list <- intersect(all_ciphers$word, all_ciphers$word23)
word24_list <- intersect(all_ciphers$word, all_ciphers$word24)
word25_list <- intersect(all_ciphers$word, all_ciphers$word25)
}

# Compile matching words, mark down what cipher they use
word_lists <- mget(paste0("word", 1:25, "_list"))

cipher_names <- paste0(
  "Cipher ", 1:25, " (A = ", LETTERS[2:26], ")"
)

matches <- data.frame(
  ciphered_word = unlist(word_lists, use.names = FALSE),
  cipher = rep(cipher_names, lengths(word_lists))
)

# Function to shift each letter backward by n positions (find original word from given cipher)
shift_word <- function(word, n) {
  chars <- strsplit(tolower(word), "")[[1]]
  
  shifted <- sapply(chars, function(char) {
    if (char %in% letters) {
      letters[(match(char, letters) - 1 - n) %% 26 + 1]
    } else {
      char
    }
  })
  
  paste0(shifted, collapse = "")
}

# Extract the cipher number from the cipher column
matches$cipher <- as.integer(
  sub("Cipher ([0-9]+).*", "\\1", matches$cipher)
)

# Create the altered word column
matches$original_word <- mapply(
  shift_word,
  matches$ciphered_word,
  matches$cipher
)

# Sort alphabetically by original word
matches <- arrange(matches, original_word)














# IGRAPH LOGIC

# Create a graph from the word matches
g <- graph_from_data_frame(
  matches %>%
    select(original_word, ciphered_word),
  directed = FALSE
)

# Identify connected components
components <- components(g)

# Produce one row per connected component
result <- tibble(
  word = names(components$membership),
  component = components$membership
) %>%
  group_by(component) %>%
  summarise(
    words = paste(sort(unique(word)), collapse = ", "),
    .groups = "drop"
  ) %>%
  select(words)






















# GROUPS LOGIC

# Get all unique words
words <- tibble(
  word = unique(c(matches$original_word, matches$ciphered_word))
)

# Initially, each word belongs to its own group
groups <- words %>%
  mutate(group = row_number())

# Repeatedly merge groups that have connected words
repeat {
  updated <- matches %>%
    left_join(groups, by = c("original_word" = "word")) %>%
    rename(group_from = group) %>%
    left_join(groups, by = c("ciphered_word" = "word")) %>%
    rename(group_to = group) %>%
    mutate(group = pmin(group_from, group_to)) %>%
    select(original_word, group) %>%
    group_by(original_word) %>%
    summarise(group = min(group), .groups = "drop")
  
  new_groups <- groups %>%
    left_join(updated, by = c("word" = "original_word")) %>%
    mutate(group = coalesce(group.y, group.x)) %>%
    select(word, group)
  
  # Propagate the smallest group ID across connected words
  new_groups <- new_groups %>%
    left_join(
      matches %>%
        left_join(new_groups, by = c("original_word" = "word")) %>%
        rename(group_from = group) %>%
        left_join(new_groups, by = c("ciphered_word" = "word")) %>%
        rename(group_to = group) %>%
        group_by(original_word) %>%
        summarise(group = min(group_from, group_to), .groups = "drop"),
      by = c("word" = "original_word"),
      suffix = c("", ".new")
    ) %>%
    mutate(group = pmin(group, coalesce(group.new, group))) %>%
    select(word, group)
  
  if (identical(groups$group, new_groups$group)) break
  
  groups <- new_groups
}

# Produce one row per connected group
result <- groups %>%
  group_by(group) %>%
  summarise(
    words = paste(sort(unique(word)), collapse = ", "),
    .groups = "drop"
  )