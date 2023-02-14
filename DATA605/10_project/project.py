import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import datetime as dt

# Read in the data with first row as header
df = pd.read_csv('/Users/berg/DataspellProjects/MDSA-UofC/DATA605/10_project/data/rescuetime_hourly_20018-2022.csv', header=0, index_col=0)

# How many Mondays, Tuesdays, etc. are there in the data?
print(df['weekday'].value_counts())

# Column names
print(df.columns)

#%%
