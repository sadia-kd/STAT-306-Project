# Loading in the data set
flightdata = read.csv("flights.csv")

# Removing unnecessary columns: "X" and "flight"
flightdata = flightdata[, 2:12]
flightdata = subset(flightdata, select = -flight)

# Check the number of rows and columns in the data frame
num_rows = nrow(flightdata)
num_rows
num_cols = ncol(flightdata)
num_cols

# Available column names 
colnames(flightdata)

# ----------------------------------------------------------------------------
# Apply levels and factor to all categorical variables
unique(flightdata$airline)
flightdata$airline <- factor(flightdata$airline, 
                             levels = unique(flightdata$airline))
levels(flightdata$airline)

unique(flightdata$source_city)
flightdata$source_city <- factor(flightdata$source_city, 
                                 levels = unique(flightdata$source_city))
levels(flightdata$source_city)

unique(flightdata$departure_time)
flightdata$departure_time <- factor(flightdata$departure_time, 
                                    levels = unique(flightdata$departure_time))
levels(flightdata$departure_time)

unique(flightdata$stops)
flightdata$stops <- factor(flightdata$stops, 
                           levels = unique(flightdata$stops))
levels(flightdata$stops)

unique(flightdata$arrival_time)
flightdata$arrival_time <- factor(flightdata$arrival_time, 
                                  levels = unique(flightdata$arrival_time))
levels(flightdata$arrival_time)

unique(flightdata$destination_city)
flightdata$destination_city <- factor(flightdata$destination_city, 
                                      levels = unique(flightdata$destination_city))
levels(flightdata$destination_city)

unique(flightdata$class)
flightdata$class <- factor(flightdata$class, levels = unique(flightdata$class))
levels(flightdata$class)
# ----------------------------------------------------------------------------

# ***************************************************************************
# 1: Fitting a model with price as the response variable
#    and the rest 9 columns as the explanatory variables
full_model = lm(price ~ ., data = flightdata)
full_model

# Print a summary of the full model
summary(full_model)

# Plot the residuals from the full model
residuals_full = residuals(full_model)
expected_vals = full_model$fitted.values

plot(expected_vals, residuals_full, 
     xlab = "Fitted Values", ylab = "Residuals")
title("Residuals vs Fitted Values")
abline(h = 0, col = "red")

# ****************************************************************************


# ****************************************************************************
# 2: Using a smaller data set size to fit the full model
set.seed(0)
sample_1000 = flightdata[sample(nrow(flightdata), 1000, replace = TRUE), ]

# Repeat all the steps in 1
# Fit a model on sample, print summary, and plot 
sample_full_model = lm(price ~ . , data = sample_1000)
summary(sample_full_model)

# Plot the residuals from the full model
sample_residuals_full = residuals(sample_full_model)
sample_expected_vals = sample_full_model$fitted.values

plot(sample_expected_vals, sample_residuals_full, 
     xlab = "Fitted Values", ylab = "Residuals")
title("Residuals vs Fitted Values")
abline(h = 0, col = "red")

# ***************************************************************************


# ***************************************************************************
# 3: Filtering for airline
airlines <- unique(flightdata$airline)
print(airlines)
# ----------------------------------------------------------------------------

# Airline 1: SpiceJet 
airline_SpiceJet = flightdata[flightdata$airline == "SpiceJet",]
unique(airline_SpiceJet$airline)

colnames(airline_SpiceJet)
head(airline_SpiceJet)

# no 'class' variable
unique(airline_SpiceJet$class)
# From EDA, we know SpiceJet only has Economy tickets in this data
# cannot use 'class' variable as explanatory variables

SpiceJet_model = lm(price ~ source_city + departure_time + stops + arrival_time 
                    + destination_city + duration + days_left, data = airline_SpiceJet)

summary(SpiceJet_model)

# ----------------------------------------------------------------------------

# Airline 2: AirAsia 
airline_AirAsia = flightdata[flightdata$airline == "AirAsia",]
unique(airline_AirAsia$airline)

colnames(airline_AirAsia)
head(airline_AirAsia)

# no 'class' variable
unique(airline_AirAsia$class)
# From EDA, we know AirAsia only has Economy tickets in this data
# cannot use 'class' variable as explanatory variables

AirAsia_model = lm(price ~ source_city + departure_time + stops + arrival_time 
                    + destination_city + duration + days_left, data = airline_AirAsia)

summary(AirAsia_model)

# ----------------------------------------------------------------------------

# Airline 3: Vistara 
airline_Vistara = flightdata[flightdata$airline == "Vistara",]
unique(airline_Vistara$airline)

colnames(airline_Vistara)
head(airline_Vistara)

# can use 'class' variable
unique(airline_Vistara$class)
# From EDA, we know Vistara has both Economy and Business tickets in this data
# can still use 'class' variable as explanatory variables

Vistara_model = lm(price ~ source_city + departure_time + stops + arrival_time 
                   + destination_city + class + duration + days_left, data = airline_Vistara)

summary(Vistara_model)

# ----------------------------------------------------------------------------

# Airline 4: GO_FIRST 
airline_GO_FIRST = flightdata[flightdata$airline == "GO_FIRST",]
unique(airline_GO_FIRST$airline)

colnames(airline_GO_FIRST)
head(airline_GO_FIRST)

# cannot use 'class' variable
unique(airline_GO_FIRST$class)
# From EDA, we know GO_FIRST has only Economy tickets in this data
# cannot  use 'class' variable as an explanatory variable

GO_FIRST_model = lm(price ~ source_city + departure_time + stops + arrival_time 
                   + destination_city + duration + days_left, data = airline_GO_FIRST)

summary(GO_FIRST_model)

# ----------------------------------------------------------------------------

# Airline 5: Indigo 
airline_Indigo = flightdata[flightdata$airline == "Indigo",]
unique(airline_Indigo$airline)

colnames(airline_Indigo)
head(airline_Indigo)

# cannot use 'class' variable
unique(airline_Indigo$class)
# From EDA, we know GO_FIRST has only Economy tickets in this data
# cannot  use 'class' variable as an explanatory variable

Indigo_model = lm(price ~ source_city + departure_time + stops + arrival_time 
                    + destination_city + duration + days_left, data = airline_Indigo)

summary(Indigo_model)

# ----------------------------------------------------------------------------

# Airline 6: Air_India 
airline_Air_India = flightdata[flightdata$airline == "Air_India",]
unique(airline_Air_India$airline)

colnames(airline_Air_India)
head(airline_Air_India)

# can use 'class' variable
unique(airline_Air_India$class)
# From EDA, we know Air_India has both Economy and business tickets in this data
# can  use 'class' variable as an explanatory variable

Air_India_model = lm(price ~ source_city + departure_time + stops + arrival_time 
                  + destination_city + class + duration + days_left, data = airline_Air_India)

summary(Air_India_model)

# ----------------------------------------------------------------------------

print(airlines)
model_summaries <- list()

model_summaries[["SpiceJet"]] <- summary(SpiceJet_model)
model_summaries[["AirAsia"]] <- summary(AirAsia_model)
model_summaries[["Vistara"]] <- summary(Vistara_model)
model_summaries[["GO_FIRST"]] <- summary(GO_FIRST_model)
model_summaries[["Indigo"]] <- summary(Indigo_model)
model_summaries[["Air_India"]] <- summary(Air_India_model)

# ----------------------------------------------------------------------------

# Function to plot residuals against fitted values
create_residual_plot <- function(model_summary, model_name) {
  # Extract residuals and fitted values from the model summary
  residuals <- residuals(model_summary)
  fitted_values <- fitted(model_summary)
  
  # Create the residual plot
  plot(fitted_values, residuals, main = paste("Residual Plot for", model_name),
       xlab = "Fitted Values", ylab = "Residuals")
  abline(h = 0, col = "red")
}

# ----------------------------------------------------------------------------

# Make the plots
create_residual_plot(SpiceJet_model, "SpiceJet")
create_residual_plot(AirAsia_model, "AirAsia")
create_residual_plot(Vistara_model, "Vistara")
create_residual_plot(GO_FIRST_model, "GO_FIRST")
create_residual_plot(Indigo_model, "Indigo")
create_residual_plot(Air_India_model, "Air_India")


# ***************************************************************************

model_summaries <- list()
airlines

# Loop through list of airlines
for (x in airlines) {
  
  # Filter for the airline
  airline_sample = flightdata[flightdata$airline == x, ]
  
  # Display the first few rows
  #cat("Airline:", x, "\n")
  #cat("Number of rows:", nrow(airline_sample), "\n")
  #head(airline_sample)
  
  # Fit model for each airline
  model = lm(price ~ source_city + departure_time + stops + arrival_time + destination_city + duration + days_left, data = airline_sample)
  
  # Save the summary
  model_summary <- summary(model)
  model_summaries[[x]] <- model_summary
  
  # Print the model
  print(x)
  print(model_summary)
  print("")
}


model_summaries

