import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import datetime as dt

# Read in the data with no header
df = pd.read_csv('/Users/berg/DataspellProjects/MDSA-UofC/DATA605/10_project/data/portable_user_history-2023-01-30-csv.csv', header=None)
# Check the data
df.head()
df.info()
df.describe()
# Check for missing values
df.isnull().sum()
# Check for duplicates
df.duplicated().sum()
# Convert the date column to datetime
df['date'] = pd.to_datetime(df['Date'])
# Difference in days between the first and last date
time_range = df['date'].max() - df['date'].min()

