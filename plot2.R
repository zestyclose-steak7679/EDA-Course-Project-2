# Load libraries
library(dplyr)
library(ggplot2)

# Load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Filter for Baltimore City
baltimore <- NEI %>%
  filter(fips == "24510")

# Identify motor vehicle sources
vehicle_scc <- SCC %>%
  filter(grepl("vehicle", EI.Sector, ignore.case = TRUE)) %>%
  pull(SCC)

# Filter Baltimore motor vehicle data
baltimore_vehicle <- baltimore %>%
  filter(SCC %in% vehicle_scc)

# Aggregate emissions by year
emissions_by_year <- baltimore_vehicle %>%
  group_by(year) %>%
  summarise(total_emission = sum(Emissions))

# Plot
p <- ggplot(emissions_by_year, aes(x = year, y = total_emission)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Motor Vehicle PM2.5 Emissions in Baltimore (1999–2008)",
    x = "Year",
    y = "Total PM2.5 Emissions"
  ) +
  theme_minimal()

# Save plot
ggsave("plot2.png", plot = p, width = 6, height = 4)