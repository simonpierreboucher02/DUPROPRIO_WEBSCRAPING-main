# This code is designed for web scraping and data collection from a real estate website (duproprio.com) and utilizes R packages such as rvest, dplyr, and glue.

# Load the necessary libraries
library(rvest)
library(dplyr)
library(glue)


url <- ""

# Create a loop to generate URLs for different pages of the website
for (i in 1:6562) {
  url[i] <- paste0("https://duproprio.com/en/search/list?search=true&min_price=50000&subtype%5B0%5D=1&subtype%5B1%5D=6&subtype%5B2%5D=2&subtype%5B3%5D=7&subtype%5B4%5D=5&subtype%5B5%5D=4&rooms=1&bathrooms=1&lot_dimension_sq_feet=500~&living_space_sq_feet=100~&is_sold=1&parent=1&pageNumber=", i, "&sort=-published_at")
}

# Initialize empty variables to store links and dates
clinks <- ""
dates <- ""

# Loop through the generated URLs
for (i in 1:6562) {
  # Read the HTML content of the webpage
  page <- read_html(url[i])
  
  # Extract property links using CSS selectors
  links <- page %>%
    html_nodes(".search-results-listings-list__item-image-link") %>%
    html_attr("href")
  
  # Convert the links into a data frame
  links <- as.data.frame(links)
  
  # Extract sold dates using CSS selectors
  date <- page %>%
    html_nodes(".search-results-listings-list div.search-results-listings-list__item-description__item.search-results-listings-list__item-description__sold-in strong") %>%
    html_text()
  
  # Convert the dates into a data frame
  date <- as.data.frame(date)
  
  # Append the extracted links and dates to the respective variables
  clinks <- rbind(clinks, links)
  dates <- rbind(dates, date)
  
  # Print a message indicating the progress
  out <- paste0(i, "th property index page is scrapped") 
  print(out) 
}

# Create a data frame from the collected links and dates
dub <- as.data.frame(cbind(clinks, dates))


# In summary, this R code is designed to scrape data from 10 pages of a real estate website. It collects property links and their associated sold dates, storing this information in data frames. The code uses various CSS selectors to locate and extract the desired data from the web pages. Finally, it combines the links and dates into a single data frame named "dub."
