import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import datetime as dt

# Read in the data with first row as header
df = pd.read_csv('/Users/berg/DataspellProjects/MDSA-UofC/DATA605/10_project/data/rescutime_hourly_cleaned_1.csv', header=0, index_col=0)


