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

year <- 2022

# Define ACS mobility variables (Table B07004)
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

# Fetch mobility data at Census Tract level
mobility_data <- get_acs(
  geography = "tract",
  variables = names(mobility_vars),
  year = year,
  state = "CA",
  county = "San Mateo",
  survey = "acs5",
  geometry = TRUE  # Include spatial data
) %>%
  mutate(variable = recode(variable, !!!mobility_vars))

# Check the first few rows
print(mobility_data)

# Summarize total mobility per tract
mobility_summary <- mobility_data %>%
  group_by(GEOID) %>%
  summarise(
    total_moved = sum(estimate, na.rm = TRUE),
    geometry = sf::st_union(geometry)  # Retain spatial features
  ) %>%
  ungroup() %>%
  st_as_sf()  # Ensure it's an sf object

# Check summary data
print(mobility_summary)

# Create a geospatial map
ggplot(data = mobility_summary) +
  geom_sf(aes(fill = total_moved), color = "black", size = 0.1) +
  scale_fill_viridis_c(name = "Total Movers", option = "viridis") +
  labs(
    title = "Mobility Trends by Census Tract in San Mateo County (2022)",
    subtitle = "Population that moved in the past year",
    caption = "Source: ACS 2022 (5-Year Estimates)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


##########
# geospatial mapping rent burdens
##########

# fetching ACS data at the Census Tract level for San Mateo County
year <- 2022 

rent_burden_vars <- c(
  "B25070_007" = "30_34_percent_income",
  "B25070_008" = "35_39_percent_income",
  "B25070_009" = "40_49_percent_income",
  "B25070_010" = "50_or_more_percent_income"
)

rent_burden_data <- get_acs(
  geography = "tract",
  variables = names(rent_burden_vars),
  year = year,
  state = "CA",
  county = "San Mateo",
  survey = "acs5",
  geometry = TRUE
) %>%
  mutate(variable = recode(variable, !!!rent_burden_vars))
rent_burden_data

# summarize total rent burden per tract
rent_burden_summary <- rent_burden_data %>%
  group_by(GEOID) %>%
  summarise(
    total_rent_burden = sum(estimate, na.rm = TRUE),
    geometry = sf::st_union(geometry)
  ) %>%
  ungroup() %>%
  st_as_sf()  
rent_burden_summary

# geospatial map
ggplot(data = rent_burden_summary) +
  geom_sf(aes(fill = total_rent_burden), color = "black", size = 0.1) +
  scale_fill_viridis_c(name = "Households Rent Burdened", option = "viridis") +
  labs(
    title = "Rent Burden by Census Tract in San Mateo County (2022)",
    subtitle = "Households spending 30%+ of income on rent",
    caption = "Source: ACS 2022 (5-Year Estimates)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")