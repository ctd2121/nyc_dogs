library(dplyr)
library(ggplot)
library(tidyverse)
library(extracat)
library(vcd)
library(choroplethr)
library(choroplethrZip)

# Read in dataset
df <- read.csv("./data/Dogs of NYC _ WNYC-filtered.csv")

# Clean NAs
df <- data.frame(lapply(df, function(x) {
  gsub("n/a", NA, x)
}))

# Get number of NAs in each column
missing <- colSums(is.na(df))
# Create dataframe from the above information
missing_df <- data.frame(missing) %>%
  add_rownames("var") %>%
  mutate(missing_pct = (missing / nrow(df)) * 100)

# View missing values by variable
ggplot(filter(missing_df, missing_pct > 0), aes(x = reorder(var, -missing_pct), y = missing_pct)) +
  geom_bar(stat = "identity",
           fill = "red",
           color = "black",
           alpha = 0.6) +
  ggtitle("Missing Values by Variable (excludes variables with no missing values)") +
  scale_x_discrete(name = "Variable") + 
  scale_y_continuous(name = "Percent Missing") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

# View patterns of missing variables within the dataset
visna(df, sort = "b")

# Convert birth variable to date class
df$birth <- as.character(df$birth)
df <- df %>%
  mutate(birth = ifelse((substr(birth, 2, 2) == "-"),
                        paste0(substr(birth, 3, 5), "-", substr(birth, 1, 1)),
                        birth)) %>%
  filter(nchar(birth) > 2) %>%
  mutate(birth = ifelse((nchar(birth) == 5 & substr(birth, 4, 4) == "-"),
                        paste0(substr(birth, 1, 4), "0", substr(birth, 5, 5)),
                        birth)) %>%
  mutate(birth = ifelse((substr(birth, 3, 3) == "-"),
                        paste0(substr(birth, 4, 6), "-", substr(birth, 1, 2)),
                        birth)) %>%
  mutate(birth = paste0(birth, "-01")) %>%
  mutate(birth = as.Date(birth, "%b-%y-%d"))

# Create frequency histogram of birth date with a one-month binwidth
hist(df$birth, "months", freq = TRUE,
     main = "Birthdates Histogram", 
     xlab = "Birth Month",
     format = "%b %y")

# Remove impossible values
df <- df %>%
  filter(birth < "2018-01-01" & birth > "1994-01-01")

# Redraw histogram with a one-year binwidth
hist(df$birth, "years", freq = TRUE,
     main = "Birthdates Histogram", 
     xlab = "Birth Year",
     format = "%Y")

# Get top five values of dominant_color present in the dataset
color_counts <- aggregate(data.frame(count = df$dominant_color),
                          list(value = df$dominant_color), length) %>%
  arrange(-count)
top_colors <- color_counts %>%
  head(5)
top_colors <- as.list(as.character(top_colors$value))
dogs_top_colors <- df %>%
  filter(dominant_color %in% top_colors)

# Group all remaining values of dominant_color into category "Other"
dogs_bot_colors <- df %>%
  filter(!(dominant_color %in% top_colors))
dogs_bot_colors$dominant_color = "OTHER"
dogs_6colors <- rbind(dogs_top_colors, dogs_bot_colors)

# Convert dominant_color to factor class 
dogs_6colors$dominant_color <- factor(dogs_6colors$dominant_color)
# Create new levels of dominant_color
levels(dogs_6colors$dominant_color) <- c("Black", "Blond", "Brown", "Tan", "White", "Other")
# Sort levels by frequency
dogs_6colors$dominant_color <- fct_infreq(dogs_6colors$dominant_color)
dogs_6colors$Group <- fct_infreq(dogs_6colors$Group)
# Put "Other" level at the end of order
dogs_6colors$dominant_color <- factor(dogs_6colors$dominant_color, levels(dogs_6colors$dominant_color)[c(1,2,4,5,6,3)])

# Create mosaic plot of the top five colors to see if dominant_color depends on Group
vcd::mosaic(~ Group + dominant_color,
            data = dogs_6colors,
            direction = "v",
            main = "Dominant Color vs. Group",
            labeling= labeling_border(rot_labels = c(45,0,0,30), 
                                      just_labels = c("left", 
                                                      "center", 
                                                      "center", 
                                                      "center")))

# Get the percent of dogs that are spayed or neutered by zip code
zips_df <- df[c("spayed_or_neutered", "zip_code")] %>%
  group_by(zip_code, spayed_or_neutered) %>%
  summarise(counts = n()) %>%
  spread(spayed_or_neutered, counts, fill = 0) %>%
  mutate(percent = 100 * (Yes / (No + Yes))) %>%
  transmute(region = as.character(zip_code), value = percent)
zips_df$zip_code <- as.character(zips_df$zip_code)

# Define zip codes for the five boroughs
nyc_fips <- c(36005, 36047, 36061, 36081, 36085)

# Create spatial heat map visualization
zip_choropleth(zips_df, 
               title = "Spayed or Neutered Dogs in NYC: Spatial Heat Map",
               county_zoom = nyc_fips,
               legend = "Percent Spayed or Neutered Dogs") +
  scale_fill_brewer(palette=7)

# Get dog data by Group and filter by birth_year
dog_groups <- df[c(2,4,5,12)] %>%
  mutate(birth_year = substr(birth, 1, 4)) %>%
  group_by(birth_year, Group) %>%
  summarise(counts = n()) %>%
  filter(1991 < birth_year & birth_year < 2012)

# Create Cleveland Dot Plot of Group counts by birth_year
ggplot(dog_groups, aes(x = counts,
                          y = birth_year,
                          color = Group)) +
  geom_point() +
  labs(color = 'Dog Group') +
  ggtitle('Mutts and Toy Dogs Gaining Popularity in NYC') +
  scale_x_continuous(name = 'Count') +
  scale_y_discrete(name = 'Year') +
  theme(axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 16),
        legend.title = element_text(hjust = 0.5, size = 10))