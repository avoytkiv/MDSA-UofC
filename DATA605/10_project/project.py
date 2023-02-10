import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import datetime as dt

# Read in the data with first row as header
df = pd.read_csv('/Users/berg/DataspellProjects/MDSA-UofC/DATA605/10_project/data/rescuetime-minute-full.csv', header=0, index_col=0)
# Rename column 'Actitivity' to 'Activity'
df.rename(columns={'Actitivity': 'Activity', 'Seconds': 'Duration'}, inplace=True)

# Convert "Date" column to datetime
df['Date'] = pd.to_datetime(df['Date'])
# Set "Date" column as index
df.set_index('Date', inplace=True)
# Sort by index
df.sort_index(inplace=True)

# Extract year, month, day, hour, and weekday from "Date" column
df['year'] = df.index.map(lambda row: row.year)
df['month'] = df.index.map(lambda row: row.month)
df['dom'] = df.index.map(lambda row: row.day)
df['weekday'] = df.index.map(lambda row: row.strftime("%A"))
df['month_name'] = df.index.map(lambda row: row.strftime("%B"))
df['dow'] = df.index.map(lambda row: row.weekday())
df['hour'] = df.index.map(lambda row: row.hour)
df['minute'] = df.index.map(lambda row: row.minute)

# Create 'Device' column from 'Activity' column.
# If 'Activity' contains 'iOS Device', then 'Device' is 'Mobile'.
# Otherwise, 'Device' is 'PC'.
df['Device'] = np.where(df['Activity'].str.contains('iOS Device'), 'Mobile', 'PC')
# Reset index and put column 'Date' back in the dataframe
df.reset_index(inplace=True)
# Check type pf 'Date' column
print(df['Date'].dtype)
# Sort by 'Device', 'Date'
df.sort_values(by=['Device', 'Date'], inplace=True)
# Select only the columns we need for the analysis: 'Device', 'Date', 'Duration', 'day_name', 'hour', 'minute'
df = df[['Date', 'Device', 'year', 'weekday', 'hour', 'minute', 'Duration']]
# Split into two dataframes: one for PC and one for Mobile
df_pc = df[df['Device'] == 'PC']
df_mobile = df[df['Device'] == 'Mobile']
# Drop the 'Device' column
df_pc.drop(columns=['Device'], inplace=True)
df_mobile.drop(columns=['Device'], inplace=True)
# Create a new dataframe with Date starting from first date depending on which device has the earliest date and ending on the last date depending on which device has the latest date. And the `Duration` column is set to 0.
# Get the earliest date
earliest_date = min(df_pc['Date'].min(), df_mobile['Date'].min())
# Get the latest date
latest_date = max(df_pc['Date'].max(), df_mobile['Date'].max())
# Create a new dataframe with the dates between the earliest and latest dates in the format 'YYYY-MM-DD HH:MM'
df_dates = pd.DataFrame(pd.date_range(earliest_date, latest_date, freq='5min'), columns=['Date'])
# Add `Duration` column and set it to 0
df_dates['Duration'] = 0
# Set "Date" column as index
df_dates.set_index('Date', inplace=True)
# Sort by index
df_dates.sort_index(inplace=True)
# Extract year, month, day, hour, and weekday from "Date" column
df_dates['year'] = df_dates.index.map(lambda row: row.year)
df_dates['weekday'] = df_dates.index.map(lambda row: row.strftime("%A"))
df_dates['hour'] = df_dates.index.map(lambda row: row.hour)
df_dates['minute'] = df_dates.index.map(lambda row: row.minute)
# Merge the two dataframes on how='outer' on index 'Date'. Append only 'Duration' column from the df_mobileto df_dates.
df_merge = df_dates.merge(df_pc, how='outer', on='Date')
# Keep columns with '_x' prefix and 'Duration_y' column
df_merge = df_merge.filter(regex='_x|Duration_y')
# Drop column 'Duration_x'
df_merge.drop(columns=['Duration_x'], inplace=True)
# Rename columns. Remove the prefix '_x' or '_y'
df_merge.rename(columns={'Duration_y': 'Duration', 'year_x': 'year',
                         'weekday_x': 'weekday', 'hour_x': 'hour', 'minute_x': 'minute'}, inplace=True)
# Fill NaN values with 0
df_merge.fillna(0, inplace=True)
# Group by 'Device' day name, hour and minute. Create a dataframe with the sum of 'Duration' for each group.
df_group = df_merge.groupby(['weekday', 'hour', 'minute'])['Duration'].sum().reset_index()
# Create an unique index with all the possible combinations of 'day_name', 'hour', and 'minute'
df_group['hm'] = df_group['hour'].apply(lambda x: '0' + str(x) if x < 10 else str(x)) + ':' + df_group['minute'].apply(lambda x: '0' + str(x) if x < 10 else str(x))
# Save the dataframe to a csv file
df_group.to_csv('/Users/berg/DataspellProjects/MDSA-UofC/DATA605/10_project/data/rescuetime-minute-grouped-pc.csv')
#
print('df_merge')
# Combine 'hour' and 'minute' columns into a single column
# If the hour is less than 10, add a leading zero
# If the minute is less than 10, add a leading zero
# df_grouped['hm'] = df_grouped['hour'].apply(lambda x: '0' + str(x) if x < 10 else str(x)) + ':' + df_grouped['minute'].apply(lambda x: '0' + str(x) if x < 10 else str(x))
# Filter out rows where 'Device' is 'Mobile'

# Save the dataframe to a csv file
# df_grouped.to_csv('/Users/berg/DataspellProjects/MDSA-UofC/DATA605/10_project/data/rescuetime-minute-full-grouped.csv')



