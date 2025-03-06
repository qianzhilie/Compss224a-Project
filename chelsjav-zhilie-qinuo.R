###########
# Chelsea Javier's code
###########

library(librarian)
librarian::shelf(qs, tidyverse, tidycensus, tigris, sf, ggplot2, viridis, dplyr)

# loading ACS variables
acs5 <- load_variables(2022, 'acs5')
view(acs5)

# defining years
years <- 2012:2022

##########
# mobility over the years
##########

# defining mobility variables for different ethnic groups
mobility_vars <- list(
  white = c(
    'B07004A_003' = 'moved_same_county_white',
    'B07004A_004' = 'moved_diff_county_white',
    'B07004A_005' = 'moved_diff_state_white',
    'B07004A_006' = 'moved_fr_abroad_white'
  ),
  black = c(
    'B07004B_003' = 'moved_same_county_black',
    'B07004B_004' = 'moved_diff_county_black',
    'B07004B_005' = 'moved_diff_state_black',
    'B07004B_006' = 'moved_fr_abroad_black'
  ),
  asian = c(
    'B07004D_003' = 'moved_same_county_asian',
    'B07004D_004' = 'moved_diff_county_asian',
    'B07004D_005' = 'moved_diff_state_asian',
    'B07004D_006' = 'moved_fr_abroad_asian'
  ),
  latine = c(
    'B07004I_003' = 'moved_same_county_latine',
    'B07004I_004' = 'moved_diff_county_latine',
    'B07004I_005' = 'moved_diff_state_latine',
    'B07004I_006' = 'moved_fr_abroad_latine'
  )
)

# fetching and cleaning ACS mobility data
get_mobility_data <- function(year, race, vars) {
  get_acs(
    geography = 'county',
    variables = names(vars),
    year = year,
    state = 'CA',
    county = 'San Mateo',
    survey = "acs5"
  ) %>%
    mutate(year = year, race = race, variable = recode(variable, !!!vars))
}

# fetching data for all groups and years
mobility_data <- map_dfr(names(mobility_vars), function(race) {
  map_dfr(years, function(year) get_mobility_data(year, race, mobility_vars[[race]]))
})

view(mobility_data)

# preparing data for visualization
mobility_plot_data <- mobility_data %>%
  mutate(variable = factor(variable, 
                           levels = c("moved_same_county_white", "moved_diff_county_white",
                                      "moved_diff_state_white", "moved_fr_abroad_white",
                                      "moved_same_county_black", "moved_diff_county_black",
                                      "moved_diff_state_black", "moved_fr_abroad_black",
                                      "moved_same_county_asian", "moved_diff_county_asian",
                                      "moved_diff_state_asian", "moved_fr_abroad_asian",
                                      "moved_same_county_latine", "moved_diff_county_latine",
                                      "moved_diff_state_latine", "moved_fr_abroad_latine"),
                           labels = rep(c("Same County", "Different County", 
                                          "Different State", "From Abroad"), 4)))

# plotting mobility trends by race/ethnicity
ggplot(mobility_plot_data, aes(x = year, y = estimate, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(name = "Mobility Type") +
  facet_wrap(~ race) +
  labs(title = "Population Mobility in San Mateo County (2012-2022)",
       x = "Year",
       y = "Population Estimate") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = years)

##########
# rent burden over the years
##########

# defining relevant ACS rent burden variables from table B25070
rent_burden_vars <- c(
  "B25070_007" = "30_34_percent_income",
  "B25070_008" = "35_39_percent_income",
  "B25070_009" = "40_49_percent_income",
  "B25070_010" = "50_or_more_percent_income"
)

# fetching rent burden data for a given year
get_rent_burden <- function(year) {
  get_acs(
    geography = "county",
    variables = names(rent_burden_vars),
    year = year,
    state = "CA",
    county = "San Mateo",
    survey = "acs5"
  ) %>%
    mutate(year = year, variable = recode(variable, !!!rent_burden_vars))
}

# fetching data for all years
rent_burden_data <- map_dfr(years, get_rent_burden)
view(rent_burden_data)

# preparing data for visualization
rent_burden_plot_data <- rent_burden_data %>%
  mutate(variable = factor(variable, 
                           levels = c("30_34_percent_income", "35_39_percent_income",
                                      "40_49_percent_income", "50_or_more_percent_income"),
                           labels = c("30-34% Income", "35-39% Income",
                                      "40-49% Income", "50% or More Income")))

# plotting rent burden trends over time
ggplot(rent_burden_plot_data, aes(x = year, y = estimate, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(name = "Rent as % of Income") +
  labs(title = "Rent Burden in San Mateo County (2012-2022)",
       x = "Year",
       y = "Households") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = years)

##########
# geospatial mapping mobility data
##########

year2012 <- 2012
year2022 <- 2022

# defining ACS mobility variables 
mobility_vars <- c(
  "B07004A_003" = "moved_same_county_white",
  "B07004A_004" = "moved_diff_county_white",
  "B07004A_005" = "moved_diff_state_white",
  "B07004A_006" = "moved_fr_abroad_white",
  "B07004B_003" = "moved_same_county_black",
  "B07004B_004" = "moved_diff_county_black",
  "B07004B_005" = "moved_diff_state_black",
  "B07004B_006" = "moved_fr_abroad_black",
  "B07004D_003" = "moved_same_county_asian",
  "B07004D_004" = "moved_diff_county_asian",
  "B07004D_005" = "moved_diff_state_asian",
  "B07004D_006" = "moved_fr_abroad_asian",
  "B07004I_003" = "moved_same_county_latine",
  "B07004I_004" = "moved_diff_county_latine",
  "B07004I_005" = "moved_diff_state_latine",
  "B07004I_006" = "moved_fr_abroad_latine"
)

# fetching mobility data at Census tract level 2012
mobility_data2012 <- get_acs(
  geography = "tract",
  variables = names(mobility_vars),
  year = year2012,
  state = "CA",
  county = "San Mateo",
  survey = "acs5",
  geometry = TRUE 
) %>%
  mutate(variable = recode(variable, !!!mobility_vars))
mobility_data

# summarizing total mobility per tract 2012
mobility_summary2012 <- mobility_data2012 %>%
  group_by(GEOID) %>%
  summarise(
    total_moved = sum(estimate, na.rm = TRUE),
    geometry = sf::st_union(geometry)  
  ) %>%
  ungroup() %>%
  st_as_sf()
mobility_summary2012

# geospatial map for 2012
ggplot(data = mobility_summary2012) +
  geom_sf(aes(fill = total_moved), color = "black", size = 0.1) +
  scale_fill_viridis_c(name = "Total Movers", option = "viridis", direction= -1) +
  labs(
    title = "Mobility Trends by Census Tract in San Mateo County (2012)",
    subtitle = "Population that moved in the past year",
    caption = "Source: ACS 2012 (5-Year Estimates)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# fetching mobility data at Census tract level 2022
mobility_data2022 <- get_acs(
  geography = "tract",
  variables = names(mobility_vars),
  year = year2022,
  state = "CA",
  county = "San Mateo",
  survey = "acs5",
  geometry = TRUE 
) %>%
  mutate(variable = recode(variable, !!!mobility_vars))
mobility_data

# summarizing total mobility per tract 2022
mobility_summary2022 <- mobility_data2022 %>%
  group_by(GEOID) %>%
  summarise(
    total_moved = sum(estimate, na.rm = TRUE),
    geometry = sf::st_union(geometry)  
  ) %>%
  ungroup() %>%
  st_as_sf()
mobility_summary2022

# geospatial map for 2022
ggplot(data = mobility_summary2022) +
  geom_sf(aes(fill = total_moved), color = "black", size = 0.1) +
  scale_fill_viridis_c(name = "Total Movers", option = "viridis", direction = -1) +
  labs(
    title = "Mobility Trends by Census Tract in San Mateo County (2022)",
    subtitle = "Population that moved in the past year",
    caption = "Source: ACS 2022 (5-Year Estimates)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")




###########
# Zhilie Qian's code
###########

**Research Question 1 (Rent Burden - Population Part ): How has the rent burden in San Mateo County changed over time?**
  
  install.packages(c("tidyverse", "tidycensus", "qs", "sf", "ggplot2", "tigris", "viridis"))

library(tidyverse) 
library(tidycensus) 
library(qs) 
library(sf)
library(ggplot2)
library(tigris)
library(viridis)
library(scales)

census_api_key("769ca12e1964566908c58c102df4729e91301ba3", install = TRUE, overwrite = TRUE)

years <- 2012:2022  
state <- "CA"
county <- "San Mateo"

acs_vars <- c(
  median_income = "B19013_001",   
  rent_burden_total = "B25070_001",  # Total renters
  rent_burden_0_9 = "B25070_002",    # Less than 10%
  rent_burden_10_14 = "B25070_003",  # 10-14%
  rent_burden_15_19 = "B25070_004",  # 15-19%
  rent_burden_20_24 = "B25070_005",  # 20-24%
  rent_burden_25_29 = "B25070_006",  # 25-29%
  rent_burden_30_34 = "B25070_007",  # 30-34%
  rent_burden_35_39 = "B25070_008",  # 35-39%
  rent_burden_40_49 = "B25070_009",  # 40-49%
  rent_burden_50_plus = "B25070_010", # 50% or more
  total_renters = "B25003_003",      # Total renters (alternative)
  poverty = "B17001_002",    
  total_pop = "B01003_001",     
  white_pop = "B02001_002",        
  black_pop = "B02001_003",       
  asian_pop = "B02001_005",          
  latinx_pop = "B03002_012"          
)
acs_data_all <- map_dfr(years, function(year) {
  get_acs(
    geography = "tract",
    variables = acs_vars,
    state = state,
    county = county,
    year = year,
    survey = "acs5",
    geometry = TRUE
  ) %>%
    mutate(year = year) 
})

acs_clean_all <- acs_data_all %>%
  select(GEOID, NAME, variable, estimate, year) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    rent_burden_pct = (rent_burden_30_34 + rent_burden_35_39 + rent_burden_40_49 + rent_burden_50_plus) / rent_burden_total * 100,
    poverty_rate = (poverty / total_pop) * 100,
    area_sq_km = as.numeric(st_area(geometry)) / 1e6, 
    population_density = total_pop / area_sq_km    
  )


common_limits <- c(
  min(acs_clean_all$rent_burden_pct, na.rm = TRUE),
  max(acs_clean_all$rent_burden_pct, na.rm = TRUE)
)

map_2012 <- ggplot(acs_clean_all %>% filter(year == 2012)) +
  geom_sf(aes(fill = rent_burden_pct), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Rent Burden (%)",
                       limits = common_limits) +
  labs(
    title = "Rent Burden in San Mateo County (2012)",
    caption = "Data Source: ACS 2012"
  ) +
  theme_minimal()

map_2022 <- ggplot(acs_clean_all %>% filter(year == 2022)) +
  geom_sf(aes(fill = rent_burden_pct), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Rent Burden (%)",
                       limits = common_limits) +
  labs(
    title = "Rent Burden in San Mateo County (2022)",
    caption = "Data Source: ACS 2022"
  ) +
  theme_minimal()

map_2012 
map_2022  

rent_burden_trend <- acs_clean_all %>%
  group_by(year) %>%
  summarise(mean_rent_burden = mean(rent_burden_pct, na.rm = TRUE))


green_viridis <- viridis(5, option = "viridis")[3]  # Adjust index for desired shade

ggplot(rent_burden_trend, aes(x = year, y = mean_rent_burden)) +
  geom_line(color = green_viridis, size = 1.2) +   # Green Viridis color for line
  geom_point(color = green_viridis, size = 3) +    # Same color for points
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  labs(
    title = "Mean Rent Burden Over Time in San Mateo County (2012–2022)",
    x = "Year",
    y = "Mean Rent Burden (%)",
    caption = "Data Source: ACS 2012-2022"
  ) +
  theme_minimal()


# Research Question 2 [Descriptive Analysis & Correlation Part]

install.packages("tidyverse")  
install.packages("tidycensus")
install.packages("qs")
install.packages(c("sf", "ggplot2", "tigris", "viridis", "corrplot"))

library(tidyverse) 
library(tidycensus) 
library(qs) 
library(sf)
library(ggplot2)
library(tigris)
library(viridis)
library(scales)
library(corrplot)

census_api_key("769ca12e1964566908c58c102df4729e91301ba3", install = TRUE, overwrite = TRUE)

# Define variables to retrieve from ACS
acs_vars <- c(
  # Rent Burden
  rent_burden = "B25070_007",       
  total_renters = "B25003_003",
  
  # Migration & Mobility
  total_population = "B07003_001",
  moved_same_house = "B07003_002",
  moved_within_state = "B07003_003",
  moved_different_state = "B07003_004",
  moved_abroad = "B07003_005"
)

# Define parameters
years <- 2012:2022  
state <- "CA"
county <- "San Mateo"

# Retrieve data for each year
acs_data_all <- map_dfr(years, function(year) {
  get_acs(
    geography = "tract",
    variables = acs_vars,
    state = state,
    county = county,
    year = year,
    survey = "acs5",
    geometry = FALSE
  ) %>%
    mutate(year = year) 
})

# Clean and reshape data
acs_clean_all <- acs_data_all %>%
  select(GEOID, NAME, variable, estimate, year) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    rent_burden_pct = (rent_burden / total_renters) * 100
  )

# Aggregate data at the county level
acs_county_data <- acs_clean_all %>%
  group_by(year) %>%
  summarise(
    mean_rent_burden = mean(rent_burden_pct, na.rm = TRUE),
    total_population = sum(total_population, na.rm = TRUE),
    movers_same_house = sum(moved_same_house, na.rm = TRUE),
    movers_within_state = sum(moved_within_state, na.rm = TRUE),
    movers_different_state = sum(moved_different_state, na.rm = TRUE),
    movers_abroad = sum(moved_abroad, na.rm = TRUE)
  )

# Ensure numeric values before reshaping
summary_stats <- acs_county_data %>%
  summarise(
    mean_rent_burden = mean(mean_rent_burden, na.rm = TRUE),
    sd_rent_burden = sd(mean_rent_burden, na.rm = TRUE),
    min_rent_burden = min(mean_rent_burden, na.rm = TRUE),
    max_rent_burden = max(mean_rent_burden, na.rm = TRUE),
    
    mean_total_population = mean(total_population, na.rm = TRUE),
    sd_total_population = sd(total_population, na.rm = TRUE),
    min_total_population = min(total_population, na.rm = TRUE),
    max_total_population = max(total_population, na.rm = TRUE),
    
    mean_movers_same_house = mean(movers_same_house, na.rm = TRUE),
    sd_movers_same_house = sd(movers_same_house, na.rm = TRUE),
    min_movers_same_house = min(movers_same_house, na.rm = TRUE),
    max_movers_same_house = max(movers_same_house, na.rm = TRUE),
    
    mean_movers_within_state = mean(movers_within_state, na.rm = TRUE),
    sd_movers_within_state = sd(movers_within_state, na.rm = TRUE),
    min_movers_within_state = min(movers_within_state, na.rm = TRUE),
    max_movers_within_state = max(movers_within_state, na.rm = TRUE),
    
    mean_movers_different_state = mean(movers_different_state, na.rm = TRUE),
    sd_movers_different_state = sd(movers_different_state, na.rm = TRUE),
    min_movers_different_state = min(movers_different_state, na.rm = TRUE),
    max_movers_different_state = max(movers_different_state, na.rm = TRUE),
    
    mean_movers_abroad = mean(movers_abroad, na.rm = TRUE),
    sd_movers_abroad = sd(movers_abroad, na.rm = TRUE),
    min_movers_abroad = min(movers_abroad, na.rm = TRUE),
    max_movers_abroad = max(movers_abroad, na.rm = TRUE)
  )

# Convert all values to numeric to avoid list storage issues
summary_stats <- summary_stats %>%
  mutate(across(everything(), as.numeric))

# Convert summary_stats to long format while keeping movers separate
summary_stats_clean <- summary_stats %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Statistic", "Variable"),
    names_pattern = "(mean|min|max|sd)_(.*)"
  ) %>%
  pivot_wider(names_from = "Statistic", values_from = "value")

# Ensure Variable column is properly formatted
summary_stats_clean <- summary_stats_clean %>%
  mutate(Variable = as.character(Variable))

# Print cleaned summary statistics table
print(summary_stats_clean, n = Inf)  # Show all rows

# Correlation Analysis
correlation_within_state <- cor(acs_county_data$mean_rent_burden, acs_county_data$movers_within_state, use = "complete.obs")
correlation_different_state <- cor(acs_county_data$mean_rent_burden, acs_county_data$movers_different_state, use = "complete.obs")
correlation_abroad <- cor(acs_county_data$mean_rent_burden, acs_county_data$movers_abroad, use = "complete.obs")

# Print correlation results
cat("Correlation between Rent Burden and Movers within State: ", correlation_within_state, "\n")
cat("Correlation between Rent Burden and Movers from Different States: ", correlation_different_state, "\n")
cat("Correlation between Rent Burden and Movers from Abroad: ", correlation_abroad, "\n")

# Correlation Analysis
cor_matrix <- acs_county_data %>%
  select(mean_rent_burden, movers_same_house, movers_within_state, movers_different_state, movers_abroad) %>%
  cor(use = "complete.obs")

# Print correlation matrix
print(cor_matrix)

# Visualizing Correlations
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.cex = 0.7, tl.srt = 30, col = viridis(200))

install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot(cor_matrix, 
           method = "square", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 4,  # Increase text size
           colors = c("blue", "white", "red"), 
           title = "Correlation Heatmap of Rent Burden and Mobility",
           ggtheme = theme_minimal()) +
  theme(
    axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1, size = 10),  # Less slanted
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid = element_blank(),  # Remove gridlines
    axis.title = element_blank()  # Remove axis titles for a cleaner look
  )

# Scatter plots for pairwise relationships
ggplot(acs_county_data, aes(x = movers_within_state, y = mean_rent_burden)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Rent Burden vs Movers Within State",
    x = "People Moved Within State",
    y = "Mean Rent Burden (%)"
  ) +
  theme_minimal()

ggplot(acs_county_data, aes(x = movers_different_state, y = mean_rent_burden)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Rent Burden vs Movers from Different States",
    x = "People Moved from Different States",
    y = "Mean Rent Burden (%)"
  ) +
  theme_minimal()

ggplot(acs_county_data, aes(x = movers_abroad, y = mean_rent_burden)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Rent Burden vs Movers from Abroad",
    x = "People Moved from Abroad",
    y = "Mean Rent Burden (%)"
  ) +
  theme_minimal()





library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(viridis)

# Reshape data to long format
acs_long <- acs_county_data %>%
  pivot_longer(
    cols = c(movers_within_state, movers_different_state, movers_abroad),
    names_to = "Mobility_Type", 
    values_to = "Movers"
  )

# Function to compute R² for annotation
calculate_r2 <- function(data, x, y) {
  model <- lm(data[[y]] ~ data[[x]], data = data)
  r2_value <- summary(model)$r.squared
  return(round(r2_value, 2))
}

# Compute R² values for each mobility type
r2_labels <- acs_long %>%
  group_by(Mobility_Type) %>%
  summarise(R2 = calculate_r2(cur_data(), "Movers", "mean_rent_burden")) %>%
  mutate(label = paste0("R² = ", R2))

# Compute label positions dynamically for better alignment with regression lines
r2_labels_positions <- acs_long %>%
  group_by(Mobility_Type) %>%
  summarise(
    x = median(Movers, na.rm = TRUE) * 1.05,  # Slightly shift right
    y = median(mean_rent_burden, na.rm = TRUE) * 1.02  # Slightly shift up
  ) %>%
  left_join(r2_labels, by = "Mobility_Type")

# Create a visually improved combined plot with separate R² labels
ggplot(acs_long, aes(x = Movers, y = mean_rent_burden, color = Mobility_Type)) +
  geom_point(alpha = 0.6, size = 3) +  # Larger points
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +  # Regression lines
  geom_text(
    data = r2_labels_positions,
    aes(x = x, y = y, label = label, color = Mobility_Type),
    size = 5, fontface = "bold",
    show.legend = FALSE
  ) +  # R² labels positioned for each group
  scale_x_continuous(labels = comma_format()) +  # Format x-axis numbers
  
  scale_color_viridis_d(option = "viridis", end = 0.9) + 

  
  labs(
    title = "Rent Burden vs Mobility Patterns",
    subtitle = "Comparing Movers Within State, Different States, and Abroad",
    x = "Number of Movers",
    y = "Mean Rent Burden (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    text = element_text(size = 14),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )



###########
# Qinuo Yang's code
###########

---
  title: "Group Project"
output: html_document
date: "2025-02-19"
---

library(tidyverse)
library(tidycensus)
library(dplyr)

library(ggplot2)
library(sf)
library(tigris)
library(ggthemes)

library("viridis") 


# Set Census API Key (Replace with your own key)
census_api_key("63217a192c5803bfc72aab537fe4bf19f6058326", overwrite = TRUE, install = TRUE)

```

# Import ACS


# Define ACS Variables with Readable Names
acs_vars <- c(
  # Migration & Mobility
  "B07003_001" = "Total Population (1yr+)",
  "B07003_002" = "Same House (No Move)",
  "B07003_003" = "Moved Within State (Intrastate)",
  "B07003_004" = "Moved to Different State (Interstate)",
  "B07003_005" = "Moved from Abroad",
  
  # Foreign-Born Population
  "B05002_001" = "Total Foreign-Born Population",
  "B05002_002" = "Naturalized U.S. Citizen",
  "B05002_003" = "Not a U.S. Citizen",
  
  # Year of Entry for Immigrants
  "B05005_001" = "Total Foreign-Born (Year of Entry)",
  "B05005_002" = "Entered 2010 or Later",
  "B05005_003" = "Entered Before 2010",
  
  # Mobility by Citizenship Status
  "B07001_001" = "Total Population (Mobility Status)",
  "B07001_017" = "Foreign-Born Movers",
  
  # Migration Flow by Race/Ethnicity
  "B07004A_001" = "Migration Flow - White",
  "B07004B_001" = "Migration Flow - Black",
  "B07004D_001" = "Migration Flow - Asian",
  "B07004I_001" = "Migration Flow - Latinx",
  
  # Rent Burden
  "B25070_007" = "Rent Burden Rate"
)

# Fetch ACS data for Sub-Districts within San Mateo County (2012-2022)
years <- 2012:2022
acs_data_list <- lapply(years, function(y) {
  get_acs(
    geography = "tract",
    state = "CA",
    county = "San Mateo",
    variables = names(acs_vars),
    year = y,
    survey = "acs5"
  ) %>%
    mutate(year = y)  # Add a column for year
})

# Combine data into a single dataframe
acs_data <- bind_rows(acs_data_list)

# Rename the variables using the defined labels
acs_data <- acs_data %>%
  mutate(variable = recode(variable, !!!acs_vars))

# View cleaned dataset
print(acs_data)


# Aggregate tract-level data to county level
acs_county_data <- acs_data %>%
  group_by(year, variable) %>%
  summarize(estimate = sum(estimate, na.rm = TRUE), .groups = "drop")

# Define categories for separate plots
categories <- list(
  "Migration & Mobility" = c("Total Population (1yr+)", "Same House (No Move)", 
                             "Moved Within State (Intrastate)", "Moved to Different State (Interstate)", "Moved from Abroad"),
  
  "Foreign-Born Population" = c("Total Foreign-Born Population", "Naturalized U.S. Citizen", "Not a U.S. Citizen"),
  
  "Year of Entry for Immigrants" = c("Total Foreign-Born (Year of Entry)", "Entered 2010 or Later", "Entered Before 2010"),
  
  "Mobility by Citizenship Status" = c("Total Population (Mobility Status)", "Foreign-Born Movers"),
  
  "Migration Flow by Race/Ethnicity" = c("Migration Flow - White", "Migration Flow - Black", 
                                         "Migration Flow - Asian", "Migration Flow - Latinx")
)


plot_migration_trends <- function(category_name, variables) {
  
  colors <- viridis(length(variables))
  
  ggplot(acs_county_data %>% filter(variable %in% variables), 
         aes(x = year, y = estimate, color = variable, group = variable)) +
    geom_line(size = 1) +  # Line plot
    geom_point(size = 2) +  # Add points
    scale_color_manual(values = colors) +  # Apply rainbow colors
    labs(
      title = paste0(category_name, " Trends in San Mateo County (2012-2022)"),
      x = "Year",
      y = "Population Estimate",
      color = "Category"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plots <- lapply(names(categories), function(cat) {
  plot_migration_trends(cat, categories[[cat]])
})

# Display all plots
print(plots[[1]])  # Migration & Mobility
print(plots[[2]])  # Foreign-Born Population
print(plots[[3]])  # Year of Entry for Immigrants
print(plots[[4]])  # Mobility by Citizenship Status
print(plots[[5]])  # Migration Flow by Race/Ethnicity



# Causal Inference



library(plm)  # For panel data regression

acs_panel <- acs_data %>%
  select(GEOID, year, variable, estimate) %>%  # Keep only necessary columns
  pivot_wider(names_from = variable, values_from = estimate)  # Reshape data


acs_panel <- acs_panel %>%
  rename(
    RentBurdenRate = `Rent Burden Rate`,
    ForeignBornPop = `Total Foreign-Born Population`,
    NaturalizedCitizen = `Naturalized U.S. Citizen`,
    NonCitizen = `Not a U.S. Citizen`,
    ForeignBornEntry = `Total Foreign-Born (Year of Entry)`,
    Entered2010Later = `Entered 2010 or Later`,
    EnteredBefore2010 = `Entered Before 2010`,
    MobilityPop = `Total Population (Mobility Status)`,
    ForeignBornMovers = `Foreign-Born Movers`,
    MigrationWhite = `Migration Flow - White`,
    MigrationBlack = `Migration Flow - Black`,
    MigrationAsian = `Migration Flow - Asian`,
    MigrationLatinx = `Migration Flow - Latinx`
  )

acs_panel <- acs_panel %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(
    lag_ForeignBornPop = lag(ForeignBornPop),
    lag_NaturalizedCitizen = lag(NaturalizedCitizen),
    lag_NonCitizen = lag(NonCitizen),
    lag_Entered2010Later = lag(Entered2010Later),
    lag_EnteredBefore2010 = lag(EnteredBefore2010),
    lag_ForeignBornMovers = lag(ForeignBornMovers),
    lag_MigrationWhite = lag(MigrationWhite),
    lag_MigrationBlack = lag(MigrationBlack),
    lag_MigrationAsian = lag(MigrationAsian),
    lag_MigrationLatinx = lag(MigrationLatinx)
  ) %>%
  ungroup()

# View reshaped dataset
print(acs_panel)


panel_model <- plm(
  RentBurdenRate ~ 
    lag_ForeignBornPop + 
    lag_NaturalizedCitizen +
    lag_NonCitizen +
    lag_Entered2010Later +
    lag_EnteredBefore2010 +
    lag_ForeignBornMovers +
    lag_MigrationWhite + 
    lag_MigrationBlack + 
    lag_MigrationAsian + 
    lag_MigrationLatinx,
  data = acs_panel,
  index = c("GEOID", "year"),  # Panel structure (tract, year)
  model = "within",  # Fixed effects
  effect = "twoways"  # Includes both tract and year fixed effects
)

# View Regression Results
summary(panel_model)



library(broom)

# Extract regression results
regression_results <- tidy(panel_model) %>%
  filter(term != "(Intercept)") %>%  # Remove intercept if present
  mutate(
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  )

ggplot(regression_results, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_point(aes(color = significance), size = 3) +  
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, 
                     xmax = estimate + 1.96 * std.error), height = 0.2) + 
  scale_color_viridis_d(option = "magma") +  
  labs(
    title = "Panel Regression Results: Immigration and Rent Burden Rate",
    x = "Coefficient Estimate",
    y = "Predictor Variables",
    color = "Significance Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )

