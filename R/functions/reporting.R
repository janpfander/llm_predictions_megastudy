# Custom helper functions 

## In-text reporting

# round numbers from models
rounded_numbers <- function(x) mutate_if(x, is.numeric, round, 3)

# get model outputs ready for inline text reporting
text_ready <- function(model_output) {
  
  result <- model_output
  
  if ("effect" %in% colnames(result)) {
    # Filter for effect == "fixed" if the variable exists
    result <- result %>% 
      # without the second bit, the normal lm() result would not be selected
      filter(effect == "fixed" | is.na(effect))
  } 
  
  result <- result %>% 
    # report p.value according to apa standards
    mutate(p.value = case_when(p.value < 0.001 ~ "< .001",
                               TRUE ~ paste0("= ", sprintf("%.3f", p.value))
    )
    ) %>% 
    # all other terms
    rounded_numbers() %>% 
    mutate(ci = glue::glue("[{conf.low}, {conf.high}]")) 
  
  if ("term" %in% colnames(result)) {
    # Filter for effect == "fixed" if the variable exists
    result <- result %>% 
      mutate(
        term = ifelse(term == "(Intercept)", "intercept", term),
      ) %>% 
      super_split(model, term)
  } 
  
  return(result)
}

# Function for splitting data along several variables (useful for inline reporting)
# taken from here: https://www.tjmahr.com/lists-knitr-secret-weapon/
super_split <- function(.data, ...) {
  dots <- rlang::enquos(...)
  for (var in seq_along(dots)) {
    var_name <- rlang::as_name(dots[[var]])
    .data <- purrr::map_depth(
      .x = .data,
      .depth = var - 1,
      .f = function(xs) split(xs, xs[var_name])
    )
  }
  .data
}

# function to print a function (for the preregistration)
# this looks like a bit of an overkill, but has the advantage of printing comments, too.
print_a_function_from_file <- function(fun_name, file = here::here("R/functions/statistics.R")) {
  lines <- readLines(file)
  
  # Find the start line: first line containing "fun_name <- function"
  start <- grep(paste0(fun_name, "\\s*<-\\s*function"), lines)[1]
  if (is.na(start)) stop("Function '", fun_name, "' not found in file.")
  
  # Initialize brace count
  brace_count <- 0
  end <- start
  
  # Loop over lines to find the end
  for (i in start:length(lines)) {
    # remove comments
    line_no_comments <- gsub("#.*$", "", lines[i])
    # remove strings
    line_no_strings <- gsub('"[^"]*"|\'[^\']*\'', '', line_no_comments)
    
    # count braces
    brace_count <- brace_count +
      stringr::str_count(line_no_strings, "\\{") -
      stringr::str_count(line_no_strings, "\\}")
    
    # function has started when we see the first '{'
    # function ends when brace_count returns to zero
    if (brace_count > 0 && i > start) {
      end <- i
      break
    }
  }
  
  # If the first { is on a later line, continue counting
  for (i in (end+1):length(lines)) {
    line_no_comments <- gsub("#.*$", "", lines[i])
    line_no_strings <- gsub('"[^"]*"|\'[^\']*\'', '', line_no_comments)
    
    brace_count <- brace_count +
      stringr::str_count(line_no_strings, "\\{") -
      stringr::str_count(line_no_strings, "\\}")
    
    if (brace_count == 0) {
      end <- i
      break
    }
  }
  
  # Print nicely for Quarto
  #cat("**", fun_name, "()**\n\n", sep = "")
  cat("```r\n")
  cat(lines[start:end], sep = "\n")
  cat("\n```\n")
}