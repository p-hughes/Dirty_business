# A function which performs the regex operations in 'Texture to percentages.R'

regex_remove <- function(.regex, init_string){
  stopifnot(is.character(.regex))
    
  end_string <- gsub(.regex, "", init_string)
  
  attributes(end_string) <- list(indices=grep(.regex, init_string), init_string=init_string)
  class(end_string) <- "texturestring"
  
  return(end_string)
}

#An S3 method for printing texture strings
print.texturestring <- function(string){
  cat("After removal:")
  print(table(as.character(string[attributes(string)$indices])))
  cat("\nBefore removal:")
  print(table(as.character(attributes(string)$init_string[attributes(string)$indices])))
}