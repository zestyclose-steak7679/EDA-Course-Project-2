library(dplyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# ---------------------------
# Q1: Compare Baltimore vs LA (ALL sources)
# ---------------------------

cities <- NEI %>%
  filter(fips %in% c("24510", "06037")) %>%
  group_by(fips, year) %>%
  summarise(Emissions = sum(Emissions), .groups="drop")

cities$fips <- factor(cities$fips,
                      levels=c("24510","06037"),
                      labels=c("Baltimore","Los Angeles"))

png("plot1.png", width=480, height=480)
ggplot(cities, aes(year, Emissions, color=fips)) +
  geom_line() +
  geom_point() +
  labs(title="Total Emissions: Baltimore vs LA")
dev.off()

# ---------------------------
# Find motor vehicle SCC
# ---------------------------

vehicle_scc <- SCC %>%
  filter(grepl("vehicle", EI.Sector, ignore.case=TRUE)) %>%
  pull(SCC)

# ---------------------------
# Q2: Baltimore motor vehicles
# ---------------------------

balt_mv <- NEI %>%
  filter(fips=="24510", SCC %in% vehicle_scc) %>%
  group_by(year) %>%
  summarise(Emissions=sum(Emissions))

png("plot2.png", 480, 480)
ggplot(balt_mv, aes(year, Emissions)) +
  geom_line(color="blue") +
  geom_point()
dev.off()

# ---------------------------
# Q3: LA motor vehicles
# ---------------------------

la_mv <- NEI %>%
  filter(fips=="06037", SCC %in% vehicle_scc) %>%
  group_by(year) %>%
  summarise(Emissions=sum(Emissions))

png("plot3.png", 480, 480)
ggplot(la_mv, aes(year, Emissions)) +
  geom_line(color="red") +
  geom_point()
dev.off()

# ---------------------------
# Q4: Compare MV Baltimore vs LA
# ---------------------------

mv_compare <- NEI %>%
  filter(fips %in% c("24510","06037"),
         SCC %in% vehicle_scc) %>%
  group_by(fips, year) %>%
  summarise(Emissions=sum(Emissions), .groups="drop")

mv_compare$fips <- factor(mv_compare$fips,
                          levels=c("24510","06037"),
                          labels=c("Baltimore","Los Angeles"))

png("plot4.png", 480, 480)
ggplot(mv_compare, aes(year, Emissions, color=fips)) +
  geom_line() +
  geom_point()
dev.off()

# ---------------------------
# Q5: Coal combustion US
# ---------------------------

coal_scc <- SCC %>%
  filter(grepl("coal", EI.Sector, ignore.case=TRUE)) %>%
  pull(SCC)

coal_us <- NEI %>%
  filter(SCC %in% coal_scc) %>%
  group_by(year) %>%
  summarise(Emissions=sum(Emissions))

png("plot5.png", 480, 480)
ggplot(coal_us, aes(year, Emissions)) +
  geom_line(color="black") +
  geom_point()
dev.off()

# ---------------------------
# Q6: Baltimore by source type
# ---------------------------

balt_type <- NEI %>%
  filter(fips=="24510") %>%
  group_by(type, year) %>%
  summarise(Emissions=sum(Emissions), .groups="drop")

png("plot6.png", 480, 480)
ggplot(balt_type, aes(year, Emissions, color=type)) +
  geom_line() +
  facet_wrap(~type)
dev.off()