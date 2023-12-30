# Load required libraries
library(vegan)
library(readxl)
library(openxlsx)

# Read the Excel file
data <- read_excel("Library/CloudStorage/OneDrive-UniversityofOklahoma/Shared_Dongyu/wetland_soil_project/Data/Species_0426.xlsx")

# Calculate Shannon's diversity index for each column
shannon_diversity <- apply(data[, -1], 2, function(column) {
  diversity <- diversity(column, index = "shannon")
  return(diversity)
})

# Calculate Simpson's diversity index for each column
simpson_diversity <- apply(data[, -1], 2, function(column) {
  diversity <- diversity(column, index = "simpson")
  return(diversity)
})

# Calculate species richness for each column
species_richness <- apply(data[, -1], 2, function(column) {
  richness <- sum(column > 0)
  return(richness)
})

# Calculate Chao1 index for each column
chao1_index <- apply(data[, -1], 2, function(column) {
  observed_richness <- sum(column > 0)
  singletons <- sum(column == 1)
  doubletons <- sum(column == 2)
  chao <- observed_richness + ((singletons * (singletons - 1)) / (2 * (doubletons + 1)))
  return(chao)
})

# Calculate Pielou's Evenness Index for each column
evenness <- apply(data[, -1], 2, function(column) {
  observed_richness <- sum(column > 0)
  total_abundance <- sum(column)
  evenness_value <- diversity(column, index = "shannon") / log(observed_richness)
  return(evenness_value)
})

# Create a new data frame with the diversity indices and Chao1 index
diversity_data <- data.frame(
  Condition = colnames(data[, -1]),
  Shannon_Diversity = shannon_diversity,
  Simpson_Diversity = simpson_diversity,
  Species_Richness = species_richness,
  Chao1_Index = chao1_index,
  Evenness_Index = evenness
)

# Filter out the control group "N"
diversity_data <- subset(diversity_data, Condition != "N")

# Update Condition column to a factor with appropriate levels
diversity_data$Condition <- factor(diversity_data$Condition, levels = c("S", "O", "B"))

# Write the diversity data to an Excel file
write.xlsx(diversity_data, "Species_0426.xlsx", row.names = FALSE)

# Print the diversity data
print(diversity_data)
