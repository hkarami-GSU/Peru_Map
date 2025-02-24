# Load required libraries
library(sf)
library(dplyr)
library(ggplot2)

# Read the shapefile
peru_regions <- st_read("C:/Users/hamedkp/Desktop/Smirnova/Cholera project/Peru/fr210fd4714.shp")

# Define the exact region order
region_order <- c(
  'AMAZONAS', 'ANCASH', 'APURIMAC', 'AREQUIPA', 'AYACUCHO',
  'CAJAMARCA', 'CALLAO', 'CUSCO', 'HUANCAVELICA', 'HUANUCO',
  'ICA', 'JUNIN', 'LIBERTAD', 'LAMBAYEQUE', 'LIMA',
  'LORETO', 'M.DE DIOS', 'MOQUEGUA', 'PASCO', 'PIURA',
  'PUNO', 'SAN MARTIN', 'TACNA', 'TUMBES', 'UCAYALI'
)

# Clean and standardize names
peru_regions <- peru_regions %>%
  mutate(
    name_1 = iconv(name_1, to = "ASCII//TRANSLIT"),
    name_1 = toupper(name_1),
    name_1 = case_when(
      name_1 == "LA LIBERTAD" ~ "LIBERTAD",
      name_1 == "MADRE DE DIOS" ~ "M.DE DIOS",
      name_1 == "LIMA PROVINCE" ~ "LIMA",
      is.na(name_1) & st_intersects(geometry, st_point(c(-73.5, -13.5)), sparse=FALSE) ~ "APURIMAC",
      is.na(name_1) & st_intersects(geometry, st_point(c(-76.2, -9.9)), sparse=FALSE) ~ "HUANUCO",
      is.na(name_1) & st_intersects(geometry, st_point(c(-75.5, -11.5)), sparse=FALSE) ~ "JUNIN",
      is.na(name_1) & st_intersects(geometry, st_point(c(-76.8, -6.5)), sparse=FALSE) ~ "SAN MARTIN",
      TRUE ~ name_1
    )
  )

# Create region numbers
region_numbers <- data.frame(
  name_1 = region_order,
  RegionNumber = 1:length(region_order)
)

# Join numbers to regions
peru_regions <- peru_regions %>%
  left_join(region_numbers, by = "name_1")

# Transform to UTM projection for better visualization
peru_regions <- st_transform(peru_regions, 32718)

# Create a named vector for custom legend
region_labels <- setNames(paste0(1:length(region_order), "-", region_order), 1:length(region_order))

ggplot(data = peru_regions) +
  geom_sf(aes(fill = as.factor(RegionNumber)), color = "black") +
  geom_sf_text(aes(label = RegionNumber), 
               size = 4,
               fontface = "bold") +
  scale_fill_manual(
    values = viridis::viridis(length(region_order)),
    labels = region_labels,
    name = "Regions"
  ) +
  labs(fill = NULL) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.5, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.margin = margin(10, 10, 10, 10),
    legend.box.spacing = unit(0.5, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    # This sets the relative widths of the map and legend
    plot.ratio = unit(c(2, 1), "null")
  ) +
  coord_sf(expand = FALSE) +
  theme(aspect.ratio = 1.5)

# Save with specific dimensions to maintain the 2:1 ratio
ggsave("peru_map_wide.png", width = 15, height = 8, dpi = 300)
