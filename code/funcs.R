# Load the necessary libraries
library(httr2)
library(dplyr)

# Define the function to get CDC data
get_cdc_data <- function(url, limit = 10000000) {
  # Create a request for the given endpoint URL with a specified limit
  ret <- request(url) |> 
    req_url_query('$limit' = limit) |> 
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
  return(as.data.frame(ret))
}
