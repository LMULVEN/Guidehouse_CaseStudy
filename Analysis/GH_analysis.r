# Meta --------------------------------------------------------------------

## Title:         Guidehouse Case Study
## Author:        Leila Mulveny
## Date Created:  5/15/2024
## Description:   This file renders/runs all relevant R code for the case study


# Preliminaries -----------------------------------------------------------

# Install the package if not already installed
if (!require(readxl)) {
  install.packages("readxl")
}

# Load the package
library(readxl)

# Specify the file path of the Excel file
file_path <- "/Users/leilamulveny/Desktop/GH_casestudy.xlsx"

# Create an empty list to store dataframes
dfs <- list()

# Specify sheet names
sheet_names <- c("Encounter Data", "Payor Data", "LOS Benchmarks")

# Loop through sheet names and read data into dataframes
for (sheet_name in sheet_names) {
  dfs[[sheet_name]] <- read_excel(file_path, sheet = sheet_name)
}

# Load the necessary packages
library(dplyr)
library(ggplot2)
library(lubridate)

# Join the "Encounter Data" and "Payor Data" dataframes
dfs[["Encounter Data"]] <- dfs[["Encounter Data"]] %>%
  left_join(dfs[["Payor Data"]], by = c("Payor Type", "Encounter Type"))

# Define the encounter types of interest
encounter_types <- c("Appendectomy", "Ischemic Stroke", "Myocardial Infarction", "Sepsis", "Child Birth", "Behavioral Health", "Bariatric Surgery", "Abdomnioplasty", "Pacemaker Implantation", "Diabetic Foot Amputation", "Hip Fracture", "Heart Transplant")

# Filter the data for the encounter types of interest
dfs[["Encounter Data"]] <- dfs[["Encounter Data"]] %>%
  filter(`Encounter Type` %in% encounter_types)


## Question 1:
#What trend do you see in Hospital A’s net patient service revenue and costs?
#Use graphs, tables, or other visuals to explain your answer

# Total Cost Overtime
# Group the data by "Admission Date", and calculate the total "Total Cost" for each month
dfs[["Encounter Data1"]] <- dfs[["Encounter Data"]] %>%
  mutate(`Admission Date` = floor_date(`Admission Date`, "month")) %>%
  group_by(`Admission Date`) %>%
  summarise(`Total Cost` = sum(`Total Cost`, na.rm = TRUE)) # Calculate total cost

# Filter out the data for December 2018
dfs[["Encounter Data1"]] <- dfs[["Encounter Data1"]] %>%
  filter(!(`Admission Date` == as.Date("2018-12-01")))

# Create a plot of the total "Total Cost" over time
totalcostplot <- ggplot(dfs[["Encounter Data1"]], aes(x = `Admission Date`, y = `Total Cost`)) +
  geom_line() +
  labs(x = "Admission Date", y = "Total Monthly Cost (in thousands of dollars)", title = "Total Monthly Cost Over Time") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

print(totalcostplot)

# Save the plot as an image
ggsave(filename = "/Users/leilamulveny/Desktop/totalcostplot.png", plot = totalcostplot, width = 10, height = 8, dpi = 300)

# Net Patient Revenue Overtime
# Group the data by "Admission Date", and calculate the total difference between "Payments Received" and "Total Cost" for each month
dfs[["Encounter Data2"]] <- dfs[["Encounter Data"]] %>%
  mutate(`Admission Date` = floor_date(`Admission Date`, "month")) %>%
  group_by(`Admission Date`) %>%
  summarise(`Cost Difference` = sum(`Payments Received` - `Total Cost`, na.rm = TRUE)) # Calculate total net patient revenue

# Filter out the data for December 2018
dfs[["Encounter Data2"]] <- dfs[["Encounter Data2"]] %>%
  filter(!(`Admission Date` == as.Date("2018-12-01")))

# Create a plot of the total difference between "Payments Received" and "Total Cost" over time
totalrevplot <- ggplot(dfs[["Encounter Data2"]], aes(x = `Admission Date`, y = `Cost Difference`)) +
  geom_line() +
  labs(x = "Admission Date", y = "Total Net Patient Service Revenue (in thousands)", title = "Total Net Patient Service Revenue Over Time") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

print(totalrevplot)

# Save the plot as an image
ggsave(filename = "/Users/leilamulveny/Desktop/totalrevplot.png", plot = totalrevplot, width = 10, height = 8, dpi = 300)

## Number of Patients Overtime
#Group the data by "Admission Date", and count the number of rows for each month
dfs[["Encounter Data3"]] <- dfs[["Encounter Data"]] %>%
  mutate(`Admission Date` = floor_date(`Admission Date`, "month")) %>%
  group_by(`Admission Date`) %>%
  summarise(`Count` = n())

# Filter out the data for December 2018
dfs[["Encounter Data3"]] <- dfs[["Encounter Data3"]] %>%
  filter(!(`Admission Date` == as.Date("2018-12-01")))

# Create a plot of the count over time
countplot <- ggplot(dfs[["Encounter Data3"]], aes(x = `Admission Date`, y = `Count`)) +
  geom_line() +
  labs(x = "Admission Date", y = "Number of Patients", title = "Monthly Patient Count Over Time") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

print(countplot)

# Save the plot as an image
ggsave(filename = "/Users/leilamulveny/Desktop/countplot.png", plot = countplot, width = 10, height = 8, dpi = 300)

## Put all Three Plots Together
# Join the three data frames into one
dfs[["Encounter Data Combined"]] <- dfs[["Encounter Data1"]] %>%
  left_join(dfs[["Encounter Data2"]], by = "Admission Date") %>%
  left_join(dfs[["Encounter Data3"]], by = "Admission Date")


# Reshape the data frame into a long format
long_data <- dfs[["Encounter Data Combined"]] %>%
  pivot_longer(cols = c("Total Cost", "Cost Difference", "Count"), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = case_when(
    Variable == "Total Cost" ~ "Total Cost",
    Variable == "Cost Difference" ~ "Net Patient Revenue",
    Variable == "Count" ~ "Patient Count",
    TRUE ~ Variable
  ),
  Value = case_when(
    Variable == "Total Cost" ~ Value / 1e6, # Convert to millions
    Variable == "Net Patient Revenue" ~ Value / 1e6, # Convert to millions
    TRUE ~ Value # Leave patient count as is
  ))

# Create a plot with all three lines
agg_drop18_plot <- ggplot(long_data, aes(x = `Admission Date`, y = Value)) +
  geom_line() +
  facet_grid(rows = vars(Variable), scales = "free_y") +
  labs(x = "Admission Date", y = "Total Value (millions of dollars, number of patients)", title = "Monthly Trends Over Time") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 30), # Increase the font size of the title
        axis.title = element_text(size = 15), # Increase the font size of the axis labels
        strip.text = element_text(size = 15)) # Increase the font size of the facet labels

print(agg_drop18_plot)
# Save the plot as an image
ggsave(filename = "/Users/leilamulveny/Desktop/agg_drop18_plot.png", plot = agg_drop18_plot, width = 10, height = 8, dpi = 300)


# Total Cost for December 2018
december_data1 <- dfs[["Encounter Data"]] %>%
  mutate(`Admission Date` = floor_date(`Admission Date`, "month")) %>%
  group_by(`Admission Date`) %>%
  summarise(`Total Cost` = sum(`Total Cost`, na.rm = TRUE) / 1e6) %>% # Calculate total cost and convert to millions
  filter(`Admission Date` == as.Date("2018-12-01"))

# Net Patient Revenue for December 2018
december_data2 <- dfs[["Encounter Data"]] %>%
  mutate(`Admission Date` = floor_date(`Admission Date`, "month")) %>%
  group_by(`Admission Date`) %>%
  summarise(`Cost Difference` = sum(`Payments Received` - `Total Cost`, na.rm = TRUE) / 1e6) %>% # Calculate total net patient revenue and convert to millions
  filter(`Admission Date` == as.Date("2018-12-01"))

# Number of Patients for December 2018
december_data3 <- dfs[["Encounter Data"]] %>%
  mutate(`Admission Date` = floor_date(`Admission Date`, "month")) %>%
  group_by(`Admission Date`) %>%
  summarise(`Count` = n()) %>%
  filter(`Admission Date` == as.Date("2018-12-01"))

# Join the three data frames into one
december_data_combined <- december_data1 %>%
  left_join(december_data2, by = "Admission Date") %>%
  left_join(december_data3, by = "Admission Date") %>%
  rename("Net Patient Revenue" = `Cost Difference`, "Total Cost" = `Total Cost`, "Patient Count" = `Count`) %>% # Rename the columns
  mutate(`Net Patient Revenue` = round(`Net Patient Revenue`, 2), 
         `Total Cost` = round(`Total Cost`, 2)) # Round to 2 decimal places

# Print the combined data for December 2018
print(december_data_combined)

# Install the necessary packages if not already installed
if (!require(gridExtra)) install.packages('gridExtra')
if (!require(grid)) install.packages('grid')

# Load the necessary libraries
library(gridExtra)
library(grid)

# Convert the table to a grid
grid_table <- tableGrob(december_data_combined)

# Save the grid as an image
ggsave(filename = "/Users/leilamulveny/Desktop/december_data_combined.png", grid_table, width = 10, height = 8, dpi = 300)

## Question 2:
#At Hospital A what is the payor mix and which payor is contributing most to Hospital A’s margin?

# Calculate the revenue and total cost for each row
dfs[["Encounter Data"]]$Revenue <- dfs[["Encounter Data"]]$`Payments Received`
dfs[["Encounter Data"]]$TotalCost <- dfs[["Encounter Data"]]$`Total Cost`

# Group by 'Payor Type' and calculate the sum of the revenue and total cost for each group
margin_by_payor <- dfs[["Encounter Data"]] %>%
  group_by(`Payor Type`) %>%
  summarise(Revenue = sum(Revenue, na.rm = TRUE), TotalCost = sum(TotalCost, na.rm = TRUE))

# Calculate the margin for each 'Payor Type'
margin_by_payor$Margin <- margin_by_payor$Revenue / margin_by_payor$TotalCost * 100

# If the margin is negative, set it to zero
margin_by_payor$Margin[margin_by_payor$Margin < 0] <- 0

# Calculate the total margin
total_margin <- sum(margin_by_payor$Margin)

# Calculate the percentage of the total margin for each 'Payor Type'
margin_by_payor$Percentage <- margin_by_payor$Margin / total_margin * 100

# Create a pie chart of the percentage of the total margin for each 'Payor Type'
pie_chart <- ggplot(margin_by_payor, aes(x = "", y = Percentage, fill = `Payor Type`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 15) + # Increase the font size of the percentage values
  theme_void() +
  theme(plot.background = element_rect(fill = "white"), 
        legend.position = "right", 
        plot.title = element_text(size = 40), # Increase the font size of the title
        legend.text = element_text(size = 20), # Increase the font size of the legend labels
        legend.title = element_text(size = 20)) + # Increase the font size of the legend title
  labs(fill = "Payor Type", title = "Percentage of Total Margin by Payor")

print(pie_chart)


# Save the pie chart as an image
ggsave(filename = "/Users/leilamulveny/Desktop/pie_chart.png", plot = pie_chart, width = 10, height = 10, dpi = 300)

##Bonus: Which type of service is most popular among commercial payors?
# Filter the data for commercial payors
commercial_data <- dfs[["Encounter Data"]] %>%
  filter(`Payor Type` == "Commercial")

# Count the number of occurrences of each "Encounter Type"
encounter_counts <- table(commercial_data$`Encounter Type`)

# Find the "Encounter Type" with the most occurrences
most_common_encounter <- names(encounter_counts)[which.max(encounter_counts)]

print(paste("The most common encounter type among commercial payors is:", most_common_encounter))


## Question 3

# Create a new column "Length of Stay"
dfs[["Encounter Data"]]$`Length of Stay` <- as.numeric(difftime(dfs[["Encounter Data"]]$`Discharge Date`, dfs[["Encounter Data"]]$`Admission Date`, units = "days"))

# Add the "Expected Length of Stay" column to the "Encounter Data" sheet
dfs[["Encounter Data"]] <- left_join(dfs[["Encounter Data"]], dfs[["LOS Benchmarks"]][, c("Encounter Type", "Expected Length of Stay")], by = "Encounter Type")

# Create a new column "Difference in Stay"
dfs[["Encounter Data"]]$`Difference in Stay` <- dfs[["Encounter Data"]]$`Length of Stay` - dfs[["Encounter Data"]]$`Expected Length of Stay`

# Create a table of all rows where "Difference in Stay" is positive and greater than 0
positive_difference_in_stay <- dfs[["Encounter Data"]] %>% filter(`Difference in Stay` > 0)

# Install the writexl package
if (!require("writexl")) install.packages("writexl")

# Load the writexl package
library(writexl)

# Write the 'positive_difference_in_stay' dataframe to a new Excel file
write_xlsx(positive_difference_in_stay, path = "/Users/leilamulveny/Desktop/Positive_Difference.xlsx")

# Calculate the percentage of encounters where the length of stay is longer than the expected length of stay
percentage_of_encounters <- (nrow(positive_difference_in_stay) / nrow(dfs[["Encounter Data"]])) * 100

# Print the percentage
print(paste("Percentage of encounters where the length of stay is longer than the expected length of stay: ", round(percentage_of_encounters, 2), "%"))


## Final Steps
# Write the modified 'Encounter Data' dataframe to a new Excel file
write_xlsx(dfs[["Encounter Data"]], path = "/Users/leilamulveny/Desktop/Modified_Encounter_Data.xlsx")


save.image("Desktop/Guidehouse_workspace.Rdata")