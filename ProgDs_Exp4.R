#khyati khurana 21BDS0349
# Load the necessary library
library(ggplot2)

# Load the txhousing dataset
data("txhousing")

# View the structure of the dataset
str(txhousing)

# Display the first few rows
head(txhousing)

library(dplyr)

# Filter for cities with sales below 100 in 2008
low_sales_2008 <- txhousing %>%
  filter(year == 2008, sales < 100)

# View the result
head(low_sales_2008)

# Filter data for the years 2010-2015 and arrange by median price recovery
price_recovery <- txhousing %>%
  filter(year >= 2010 & year <= 2015) %>%
  arrange(desc(median))

# View top cities
head(price_recovery)

# Select key metrics for specific cities between 2005 and 2015
selected_cities_metrics <- txhousing %>%
  filter(city %in% c("Houston", "Austin", "Dallas"), year >= 2005 & year <= 2015) %>%
  select(city, year, sales, volume, listings)

# View the result
head(selected_cities_metrics)

# Create a new column: ratio of listings to sales for 2015
txhousing_2015 <- txhousing %>%
  filter(year == 2015) %>%
  mutate(listings_to_sales_ratio = listings / sales)

# View the result
head(txhousing_2015)

# Summarize total sales and average median price per year
yearly_summary <- txhousing %>%
  group_by(year) %>%
  summarize(total_sales = sum(sales, na.rm = TRUE),
            avg_median_price = mean(median, na.rm = TRUE))

# View the result
yearly_summary

library(tidyr)

# Pivot the wide-format data back to long format, replacing NA with 0
sales_long <- sales_wide %>%
  pivot_longer(cols = starts_with("Month_"),  # Convert all month columns into rows
               names_to = "month",            # New column for month names
               values_to = "sales",           # New column for sales values
               names_prefix = "Month_") %>%   # Remove 'Month_' prefix from month names
  mutate(sales = replace_na(sales, 0))        # Replace NA values in sales with 0

# View the cleaned result
head(sales_long)


library(ggplot2)
ggplot(sales_long, aes(x = month, y = sales, color = city)) +
  geom_line() +
  labs(title = "Monthly Sales Trends Across Cities", x = "Month", y = "Sales") +
  theme_minimal()

# Pivot the long-format data back to wide format, replacing NA with 0
sales_wide_clean <- sales_long %>%
  pivot_wider(names_from = month,             # Create columns for each month
              values_from = sales,            # Fill these columns with sales data
              values_fill = list(sales = 0))  # Replace NA values with 0

# View the cleaned result
head(sales_wide_clean)


# Create a bar plot with the correct column names
ggplot(sales_wide_clean, aes(x = city)) +
  geom_bar(aes(y = `1`, fill = "Month 1"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = `2`, fill = "Month 2"), stat = "identity", position = "dodge") +
  labs(title = "Comparison of Sales Across Months by City", x = "City", y = "Sales") +
  theme_minimal()


