# DuProprio Web Scraping Project

This repository contains R scripts for scraping and processing housing data from the DuProprio website. The project includes scripts for extracting property URLs, scraping detailed property information, and performing feature engineering on the gathered data.

## Repository Structure

- **[URL_SCRAPPER.R](https://github.com/simonpierreboucher/DUPROPRIO_WEBSCRAPING/blob/main/URL_SCRAPPER.R)**: This script collects URLs for individual property listings from DuProprio. It identifies relevant links from the main search pages to prepare for detailed scraping.
- **[HOUSING_PAGE_SCRAPPER.R](https://github.com/simonpierreboucher/DUPROPRIO_WEBSCRAPING/blob/main/HOUSING_PAGE_SCRAPPER.R)**: This script accesses each property’s page using the URLs from `URL_SCRAPPER.R`, then extracts key information such as price, location, property features, and descriptions.
- **[FEATURE_ENGINEERING.R](https://github.com/simonpierreboucher/DUPROPRIO_WEBSCRAPING/blob/main/FEATURE_ENGINEERING.R)**: After data collection, this script processes and refines the data for analysis, including transformations, creating derived features, and handling missing values.

## Getting Started

### Prerequisites

To run these scripts, you will need:
- **R 4.0 or newer**
- R packages:
  - `rvest` (for web scraping)
  - `tidyverse` (for data manipulation)
  - `httr` (for handling HTTP requests)
  - `jsonlite` (for JSON parsing, if applicable)

### Installation

To install the required packages, run the following commands in your R environment:

```R
install.packages("rvest")
install.packages("tidyverse")
install.packages("httr")
install.packages("jsonlite")
```

### Running the Scripts

1. **URL Extraction**: Start by running `URL_SCRAPPER.R` to collect URLs of individual property listings. This script will output a list of URLs for further scraping.
   
2. **Property Data Scraping**: Use `HOUSING_PAGE_SCRAPPER.R` to extract detailed information from each property page, based on the URLs gathered in the first step.

3. **Feature Engineering**: After gathering the data, run `FEATURE_ENGINEERING.R` to clean and preprocess the data for analysis. This script will handle data transformations and create additional features as needed.

## Notes

- **Compliance**: Ensure compliance with DuProprio's terms of service when using this scraper, as some websites prohibit automated data extraction.
- **Data Storage**: Modify scripts as needed to save output data in the desired format (e.g., CSV, JSON, database).

## License

This repository is for educational and personal use.
