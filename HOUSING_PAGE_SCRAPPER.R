# This code is intended for web scraping from a specific real estate website (duproprio.com) and organizing the scraped data into a structured format.
# Install and load the required library 'rvest'
install.packages("rvest")
library(rvest)

# Set the working directory to the specified path
setwd("/Users/simon-pierreboucher/Desktop/HEDORATE/webscrapping")

# Load a previously saved RData file named 'dup.RData'
load("dub.RData")

# Create an empty data frame named 'results' with predefined column names
results <- data.frame(
  url = character(),
  price = character(),
  type = character(),
  location = character(),
  bedrooms = character(),
  bathrooms = character(),
  livingspace = character(),
  lotdim = character(),
  f1 = character(),
  f2 = character(),
  f3 = character(),
  f4 = character(),
  f5 = character(),
  f6 = character(),
  f7 = character(),
  f8 = character(),
  f9 = character(),
  f10 = character(),
  Fd1 = character(),
  Fd2 = character(),
  Fd3 = character(),
  Fd4 = character(),
  Fd5 = character(),
  Fd6 = character(),
  Fd7 = character(),
  Fd8 = character(),
  Fd9 = character(),
  Fd10 = character(),
  Fd11 = character(),
  Fd12 = character(),
  Fd13 = character(),
  Fd14 = character(),
  Fd15 = character(),
  Fd16 = character(),
  Fd17 = character(),
  Fd18 = character(),
  Fd19 = character(),
  Fd20 = character(),
  roomdim = character(),
  solddate = character(),
  stringsAsFactors = FALSE
)

# Loop to scrape data from a range of property URLs
for (i in 1:400) {  # You may adjust the range as needed
  # Read the content of the webpage using the URL from the 'duproprio' dataset
  page_content <- read_html(dup$links[i])
  
  # Extract various property details using CSS selectors and store them in the 'results' data frame
  results[i, 1] <- dup$links[i]
  results[i, 2] <- page_content %>% html_node(".listing-sidebar > .listing-information > .listing-profile > .listing-address > .listing-price > .listing-price__amount") %>% html_text()
  results[i, 3] <- page_content %>% html_node(".listing-sidebar > .listing-information > .listing-profile > .listing-address > .listing-location__title > a") %>% html_text()
  results[i,4] <- page_content %>% html_node(".listing-sidebar > .listing-information > .listing-profile > .listing-address div.listing-location__address ") %>% html_text()
  results[i,5] <- page_content %>% html_node(":nth-child(1) > .listing-main-characteristics__label > .listing-main-characteristics__number") %>% html_text()
  results[i,6] <- page_content %>% html_node(".listing-main-characteristics__item--bathrooms > .listing-main-characteristics__label > .listing-main-characteristics__number") %>% html_text()
  results[i,7] <- page_content %>% html_node(".listing-main-characteristics__item--living-space-area > .listing-main-characteristics__item-dimensions > .listing-main-characteristics__number") %>% html_text()
  results[i,8] <- page_content %>% html_node(".listing-main-characteristics__item--lot-dimensions > .listing-main-characteristics__item-dimensions > .listing-main-characteristics__number") %>% html_text()
  results[i,9] <- page_content %>% html_node(".listing-list-characteristics__viewport > :nth-child(2)") %>% html_text()
  results[i,10] <- page_content %>% html_node(".listing-list-characteristics__viewport > :nth-child(3)") %>% html_text()
  results[i,11] <- page_content %>% html_node(".listing-list-characteristics__viewport > :nth-child(4)") %>% html_text()
  results[i,12] <- page_content %>% html_node(".listing-list-characteristics__viewport > :nth-child(5)") %>% html_text()
  results[i,13] <- page_content %>% html_node(".listing-list-characteristics__viewport > :nth-child(6)") %>% html_text()
  results[i,14] <- page_content %>% html_node(".listing-list-characteristics__viewport > :nth-child(7)") %>% html_text()
  results[i,15] <- page_content %>% html_node(".listing-list-characteristics__viewport > :nth-child(8)") %>% html_text()
  results[i,16] <- page_content %>% html_node(".listing-list-characteristics__viewport > :nth-child(9)") %>% html_text()
  results[i,17] <- page_content %>% html_node(".listing-list-characteristics__viewport > :nth-child(10)") %>% html_text()
  results[i,18] <- page_content %>% html_node(".listing-list-characteristics__viewport > :nth-child(11)") %>% html_text()
  results[i,19] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(1)") %>% html_text()
  results[i,20] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(2)") %>% html_text()
  results[i,21] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(3)") %>% html_text()
  results[i,22] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(4)") %>% html_text()
  results[i,23] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(5)") %>% html_text()
  results[i,24] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(6)") %>% html_text()
  results[i,25] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(7)") %>% html_text()
  results[i,26] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(8)") %>% html_text()
  results[i,27] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(9)") %>% html_text()
  results[i,28] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(10)") %>% html_text()
  results[i,29] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(11)") %>% html_text()
  results[i,30] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(12)") %>% html_text()
  results[i,31] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(13)") %>% html_text()
  results[i,32] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(14)") %>% html_text()
  results[i,33] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(15)") %>% html_text()
  results[i,34] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(16)") %>% html_text()
  results[i,35] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(17)") %>% html_text()
  results[i,36] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(18)") %>% html_text()
  results[i,37] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(19)") %>% html_text()
  results[i,38] <- page_content %>% html_node(".listing-complete-list-characteristics__content > :nth-child(20)") %>% html_text()
  results[i,39] <- page_content %>% html_node(".listing-rooms-details") %>% html_text()
  results[i,40] <- dup$date[i]
  
  # Print a progress message
  out <- paste0(i, "th property is scrapped, ", 68941 - i, " property remaining")
  print(out) 
}

# Store the scraped data in the 'bd' data frame
bd <- results


# Save the dataset 'bd' as an RData file named 'bd.RData'
save(bd, file = "bd.RData")

# In summary, this R code loads a dataset of property URLs, scrapes detailed information from each property's webpage, and stores the scraped data in a structured data frame named 'results.' The code then saves this data frame as an RData file named 'bd.RData' for further analysis or use. Please note that you may need to adjust the URL range and CSS selectors based on the specific website structure and your data requirements.
