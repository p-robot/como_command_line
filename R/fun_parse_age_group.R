parse_age_group <- function(vec) {
  regx_str <- "^(([0]*([1-9]|1[0-9]|2[0-1]))[\\,\\:])*([0]*([1-9]|1[0-9]|2[0-1]))$"
  output <- rep(FALSE, 21)
  
  vec <- str_replace_all(vec, "-", ":")
  vec <- str_replace_all(vec, ";", ",")
  vec <- str_replace_all(vec, "[:space:]", "")
  vec <- str_replace_all(vec, "[:alpha:]", "")
  if (!str_detect(vec, regx_str)) {
    return(output)
  }
  vec <- paste0("c(", vec, ")")
  vec2 <- eval(parse(text = vec))
  output[vec2] <- TRUE
  return(output)
}