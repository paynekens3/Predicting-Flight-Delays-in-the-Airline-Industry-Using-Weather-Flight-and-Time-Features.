# Predicting-Flight-Delays-in-the-Airline-Industry-Using-Weather-Flight-and-Time-Features.

## Overview
This project aims to predict flight delays in the airline industry by analyzing various factors such as weather conditions, flight details, and time-based features. The dataset used contains information on flights, delays, and other relevant variables. The model performance is evaluated using both Linear Regression and Random Forest.

## Libraries Used
tidyverse – Data manipulation and visualization.
caret – For data splitting and model training.
randomForest – For training a Random Forest model.
Metrics – For evaluation metrics like RMSE, MAE.
ggplot2 – For creating visualizations.

## Dataset
- The dataset DelayedFlights.csv contains information on flight delays. The columns include:
Month: The month of the flight.
DayOfWeek: The day of the week (1 = Monday, 7 = Sunday).
DepTime: The scheduled departure time.
CRSDepTime: The scheduled departure time (in minutes).
FlightNum: The flight number.
ArrDelay: The arrival delay time (target variable).
