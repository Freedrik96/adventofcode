### puzzle 1 ### 
library(dplyr)
library(stringr)

data <- read.csv2("day 1/data.csv")

## converts spelled numbers to digits ## Task 2

# Test the function with a vector of strings
replace_number_patterns <- function(strings) {
  # Map of spelled-out numbers to their replacement patterns
  number_map <- c("zero" = "zeroo", "one" = "onee", "two" = "twoo", "three" = "threee", 
                  "four" = "fourr", "five" = "fivee", "six" = "six", 
                  "seven" = "sevenn", "eight" = "eightt", "nine" = "ninee")
  
  # Replace each spelled-out number with its pattern in each string
  for (word in names(number_map)) {
    pattern <- paste0(word)  # Match the whole word
    strings <- gsub(pattern, number_map[word], strings, ignore.case = TRUE)
  }
  strings
}


data$messages_digit <- replace_number_patterns(data$messages)



convert_spelled_numbers_to_digits <- function(strings) {
  # Map of spelled-out numbers to digits
  number_map <- c("zero"=0, "one"=1, "two"=2, "three"=3, "four"=4, 
                  "five"=5, "six"=6, "seven"=7, "eight"=8, "nine"=9)
  
  # Function to replace numbers in a single string
  replace_numbers <- function(string) {
    for (word in names(number_map)) {
      pattern <- word
      string <- str_replace_all(string, regex(pattern, ignore_case = TRUE), as.character(number_map[word]))
    }
    string
  }
  
  # Apply the replacement to each string in the vector
  sapply(strings, replace_numbers)
}
data$messages_digit <- convert_spelled_numbers_to_digits(data$messages_digit)


## Extracts first and last digit, doubles single digits ## task 1

extract_first_last_digit <- function(strings) {
  sapply(strings, function(string) {
    # Find all digits in the string
    digits <- str_extract_all(string, "\\d", simplify = TRUE)
    
    # Check the number of digits found
    if (length(digits) == 0) {
      return(NA)  # Return NA if no digits are found
    } else if (length(digits) == 1) {
      paste0(digits, digits)  # Return the digit if only one is found
    } else {
      # Extract the first and last digit if more than one digit is found
      first_digit <- digits[1]
      last_digit <- digits[length(digits)]
      paste0(first_digit, last_digit)
    }
  })
}

data$numbers <- extract_first_last_digit(data$messages_digit) %>% as.numeric()

sum(data$numbers)
