# Load the necessary libraries
library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)

# Connect to the PostgreSQL database
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "rfm_project", # Database name
  host = "localhost",
  port = ***********,
  user = "*********",
  password = "*****"
)

# Load the data from the table online_sales_project
sales_data <- dbReadTable(con, "online_sales_project")

# 1. Sum all values in the total_price column
total_sales <- sum(sales_data$total_price, na.rm = TRUE)
print(paste("Total sales: ", total_sales))

# 2. Calculate total sales by country
sales_by_country <- sales_data %>%
  group_by(country) %>%
  summarize(total_sales = sum(total_price, na.rm = TRUE)) %>%
  arrange(desc(total_sales))
print(sales_by_country)

# 3.Calculate the total sales value per day
daily_sales_value <- sales_data %>%
  filter(!is.na(invoice_date)) %>%
  group_by(invoice_date) %>%
  summarize(
    total_value = sum(total_price, na.rm = TRUE)
  )

# Display the daily sales value
daily_sales_value

# Create the sales evolution chart
ggplot(daily_sales_value, aes(x = invoice_date, y = total_value)) +
  geom_line(color = "blue", size = 1) +  # Blue line for sales
  labs(
    title = "Sales Evolution Over Time",
    x = "Date",
    y = "Sales"
  ) +
  theme_minimal()  # Clean style for the chart

# 4. Temporarily set locale to English for consistent day names
original_locale <- Sys.getlocale("LC_TIME")  # Save the current locale
Sys.setlocale("LC_TIME", "C")  # Set to English locale

# Create the day_of_week column
sales_data$day_of_week <- factor(
  weekdays(sales_data$invoice_date),
  levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
)

# Restore the original locale
Sys.setlocale("LC_TIME", original_locale)

# Group sales by day of the week and sum total_price
sales_by_day <- aggregate(total_price ~ day_of_week, data = sales_data, sum)

# Create the bar chart for sales by day of the week
ggplot(sales_by_day, aes(x = day_of_week, y = total_price)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "Total Sales by Day of the Week",
    x = "Day of the Week",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate X-axis labels
  )

# 5. RFM Analysis
# Set the reference date as the most recent invoice_date
reference_date <- max(sales_data$invoice_date, na.rm = TRUE)

# Calculate RFM metrics
rfm_data <- sales_data %>%
  filter(customerid != 0) %>% # Exclude rows with customerid = 0
  group_by(customerid) %>%
  summarize(
    Recency = as.numeric(difftime(reference_date, max(invoice_date, na.rm = TRUE), units = "days")),
    Frequency = n_distinct(invoice_no),
    Monetary = sum(total_price, na.rm = TRUE)
  )

# Add RFM scores (Rank each metric into quintiles)
rfm_data <- rfm_data %>%
  mutate(
    RecencyScore = ntile(-Recency, 5),  # Negative sign to assign higher scores to more recent customers
    FrequencyScore = ntile(Frequency, 5),
    MonetaryScore = ntile(Monetary, 5),
    RFM_Score = RecencyScore + FrequencyScore + MonetaryScore
  )
write.csv(rfm_data, "rfm_data.csv", row.names = FALSE)
write.csv(sales_data, "sales_data.csv", row.names = FALSE)
#then i used this .csv archives to do a dashboard, thank for read!
