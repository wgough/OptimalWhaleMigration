# Load required library
install.packages("geosphere")  # For distance calculations
library(geosphere)
library(lubridate)  # For working with datetime objects

#### Calculate Distance of a CSV track ####

# Function to read and process CSV file
process_csv_file <- function(file_path) {
  # Load the CSV data
  csv_data <- read.csv(file_path)

  # Extract latitude and longitude columns
  latitudes <- csv_data$location.lat
  longitudes <- csv_data$location.long

  # Combine latitude and longitude into a matrix
  coord_matrix <- cbind(longitudes, latitudes)  # Reversed order for correct input to geosphere

  # Calculate total distance
  total_distance <- sum(distVincentySphere(coord_matrix))

  # Return the coordinates and total distance
  return(list(coordinates = coord_matrix, total_distance = total_distance))
}

# Path to your CSV file
csv_file_path <- "data-raw/SatTracks/Fin_Azores_89969_Clipped.csv"

# Process the CSV file
result <- process_csv_file(file_path = csv_file_path)

#### Calculate track duration in days ####

# Function to calculate duration in days with decimals
calculate_duration <- function(start_timestamp, end_timestamp) {
  # Convert timestamps to datetime objects
  start_datetime <- as.POSIXct(start_timestamp, format = "%m/%d/%Y %I:%M:%S %p")
  end_datetime <- as.POSIXct(end_timestamp, format = "%m/%d/%Y %I:%M:%S %p")

  # Calculate time difference in seconds
  time_difference_seconds <- as.numeric(difftime(end_datetime, start_datetime, units = "secs"))

  # Convert time difference to days with decimal representation
  duration_days_decimal <- time_difference_seconds / (60 * 60 * 24)

  return(duration_days_decimal)
}

# Start and end timestamps (replace with your actual timestamps)
start_timestamp <- "5/15/2010 12:11:00 PM"
end_timestamp <- "6/13/2010 5:25:00 AM"

# Calculate the duration in days with decimals
duration_days_decimal <- calculate_duration(start_timestamp, end_timestamp)

# Display the coordinates, total distance, and duration results
print(result$coordinates)
print(paste("Total distance:", result$total_distance, "meters"))
print(paste("Total distance:", result$total_distance/1000, "kilometers"))
print(paste("Duration between the timestamps:", duration_days_decimal, "days"))
