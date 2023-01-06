
###*** Script to generate data matrix- item columns and person rows of variable number of dichotomous and polytomous items. Generate responses to variable number of answer
###* choices, key to score items, and time data for responses. Items are list columns to accommodate multiple response vectors for polytomous items and answers

library(tidyverse)


# Generate Item and Response Data -----------------------------------------

###*** Probability of each response. Given functions weight each response equally. Can create custom function to weight certain responses. 

dichotomous_probability_func <- function(number_dichotomous_answers){
  rep((1/number_dichotomous_answers), number_dichotomous_answers)
}

custom_dichotomous_probability_func <- function(){
  
}

polytomous_probability_func <- function(number_polytomous_answers){
  rep((1/number_polytomous_answers), number_polytomous_answers)
}

###*** Function to create a sample dataframe with item columns and person responses
sample_dat_func <- function(
    starting_item_value = 0, # Item to start at +1. Default of 0 starts at item_1. Can use if generating another dataset of items to start after another dataset
  # Dichotomous options
    number_dichotomous_items = 9,
    number_candidates = 1000,
    number_dichotomous_answer_choices = 4,
    dichotomous_probability = dichotomous_probability_func(number_dichotomous_answer_choices), # can substitute custom probability_func in call
  # Polytomous
    number_polytomous_items = 1,
    number_polytomous_answer_choices = 5,
    polytomous_probability = polytomous_probability_func(number_polytomous_answer_choices) # can substitute custom probability_func in call
  )
{
  number_dichotomous_items <- number_dichotomous_items + starting_item_value
  # Sample data, persons are rows, items as columns; column 1 is person number
  ## Generate dichotomous items
  purrr::map(
    .x = (starting_item_value + 1):number_dichotomous_items, 
    .f = ~ {
      # Tibble with columns 'item_*'; 1000 rows, random letters a-d (accepting default arguments)
      tibble(!!sym(paste0("item_", .x)) :=
               replicate(number_candidates,
                         list(
                           # Sample one letter (candidate response) for each item (e.g., a-d)
                           sample(
                             letters[1:number_dichotomous_answer_choices],
                             size = 1,
                             prob = dichotomous_probability, # Default is equal probability response of any answer choice
                             replace = TRUE)
                         )
               )
      )
    }) %>% # end dichotomous items
    ## Polytomous item
    # 1000 a-e values, choose 1-5, sorted alphabetically (accepting default argruments)
    bind_cols(.,
              purrr::map(
                .x = (number_dichotomous_items + 1):(number_dichotomous_items + number_polytomous_items), # start after last dichotomous item
                .f = ~ {
                  ## Dichotomous items
                  # Tibble with columns 'item_*'; 1000 rows, random letters a-e, varying number of responses, 1-5
                  tibble(!!sym(paste0("item_", .x)) :=
                           replicate(number_candidates,
                                     list(
                                       sort(
                                         sample(
                                                letters[1:number_polytomous_answer_choices], # from answer choice possibilities
                                                size = sample(
                                                        1:number_polytomous_answer_choices, # choose varying number of letters (e.g., a,b,c or a,d, etc.)
                                                        size = 1, 
                                                        prob = polytomous_probability),
                                                replace = FALSE) # can't choose more than one answer choice
                                          )
                                       )
                           )
                  )
                }
              )
    ) %>% #end polytomous item
    # Persons numbered to 1000
    tibble::add_column(person = forcats::as_factor(1:number_candidates), .before = 1) # candidate number
}
  
## Information on the items (polytomous or dichotomous) and answer key
item_information_func <- function(
    starting_item_value = 0,
    number_dichotomous_items = 9,
    number_dichotomous_answer_choices = 4,
    dichotomous_probability = dichotomous_probability_func(number_dichotomous_answer_choices),
    number_polytomous_items = 1,
    number_polytomous_answer_choices = 5,
    polytomous_probability = polytomous_probability_func(number_polytomous_answer_choices)  
  )
{
  tibble(
    item_number = paste0("item_", (starting_item_value + 1):(starting_item_value + number_dichotomous_items + number_polytomous_items)), # same item numbers as in data
    item_type = c(rep("dichotomous", number_dichotomous_items), rep("polytomous", number_polytomous_items)), # corresponds to sample_data
    # answer key, random; if matches -> TRUE
    key = c(
      # Dichotomous answer key
      sample(
        letters[1:number_dichotomous_answer_choices],
        size = number_dichotomous_items,
        prob = dichotomous_probability, # can alter for different frequency of answers
        replace = TRUE),
      # Polytomous answer key- same strategy of generating answers as generating item responses in sample_dat
      replicate(number_polytomous_items,
               list(
                 sort(
                   sample(
                    letters[1:number_polytomous_answer_choices],
                    size = sample(
                            1:number_polytomous_answer_choices,
                            size = 1,
                            prob = polytomous_probability),
                    replace = FALSE)
                  )
                 )
          )
      )
  )
}

## Time data on how long each item took on average, for flag, can change upper and lower bounds. Random response time currently
time_dat_func <- function(
    starting_item_value = 0,
    number_dichotomous_items = 9,
    number_polytomous_items = 1,
    response_time_lower = 60,
    response_time_upper = 150
    ){
  tibble::tibble(
    item_number = paste0("item_", (starting_item_value + 1):(starting_item_value + number_dichotomous_items + number_polytomous_items)), # same item numbers as above
    response_time = sample(response_time_lower:response_time_upper, (number_dichotomous_items + number_polytomous_items), replace = TRUE)
  )
} 

###*** Call funcs and assign. Can pass optional arguments to override defaults. Those arguments should be same (e.g., if starting_item_value = 10, pass to each call)
sample_dat <- sample_dat_func()
item_information <- item_information_func()
time_dat <- time_dat_func()

# Score Data --------------------------------------------------------------

## Put item responses in long format, score- dichotomous 0,1; polytomous 0-4
long_scored_dat <- sample_dat %>% 
  # Long format data, item numbers and responses
  pivot_longer(., 
               cols = -person, 
               names_to = "item_number", 
               values_to = "response") %>% 
  # Join with answer key, so each item has response and key
  inner_join(.,
             item_information,
             by = "item_number") %>% 
  rowwise() %>% 
  ## Score each item, use %in% to generalize for polytomous case; here just taking number correct and subtracting errors of commision, with 0 min
  mutate(
    score = max(
      0,
      sum(unlist(response) %in% unlist(key)) - # correct responses
        sum(!unlist(response) %in% unlist(key)) # incorrect responses
    )
  ) 

# IRT Model ---------------------------------------------------------------

###*** Conduct Rasch model on scored data- graded response for polytomous and generate theta for each person
rasch_mod <- long_scored_dat %>% 
  # Create data matrix, persons as rows, items as columns; number correct as values
  pivot_wider(., id_cols = person, names_from = item_number, values_from = score) %>% 
  dplyr::select(-person) %>% 
  mirt::mirt(., itemtype = "Rasch", se = TRUE, 
             verbose = FALSE) # suppress iteration results
# Theta of each person
person_theta <- tibble::tibble(
  person = fct_inorder(as_factor(rasch_mod@Data$rowID)),
  theta = as.numeric(mirt::fscores(rasch_mod))
)

# You can output any of the dataframes as csv. However, sample_dat and item_information have list columns (to accommodate multiple responses for polytomous), so those
# need to be coerced to a single cell format. Can use function. Using | to separate answer choices within cells. Can change.
write_csv_func <- function(dat, separator = "|"){
  file_name <- rlang::as_label(rlang::enquo(dat))
  dat %>% 
    rowwise() %>% 
    mutate_if(is.list, ~paste(unlist(.), collapse = separator)) %>% 
    write.csv(paste0(file_name, ".csv"), row.names = FALSE)
}

# Call as
# write_csv_func(sample_dat)
