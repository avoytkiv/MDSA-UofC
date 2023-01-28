# How do staff pick projects differ from normal projects in terms of success
# (backers, amount pledged, timeliness, etc.)?
# https://webrobots.io/kickstarter-datasets/

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# graph facecolor
sns.set(rc={'axes.facecolor':'#c0cefa'})
font_color = '#525252'
# title font
csfont = {'fontname':'Georgia'}
# main font
hfont = {'fontname':'Georgia'}
# annotation color
ann_color = '#c449cc'

# Read three files
kickstarter001 = pd.read_csv('./data/Kickstarter001.csv')
kickstarter002 = pd.read_csv('./data/Kickstarter002.csv')
kickstarter000 = pd.read_csv('./data/Kickstarter.csv')
# Concatenate the three files
kickstarter = pd.concat([kickstarter000, kickstarter001, kickstarter002])
# Pivot the kickstarter data where index is 'Staff Pick', columns is 'State', and values is 'id'
kickstarter_pivot = kickstarter.pivot_table(index='staff_pick', columns='state', values='id', aggfunc='count')
# Drop columns with missing values
kickstarter_pivot = kickstarter_pivot.dropna(axis=1)
# Calculate the percentage of total projects for each state
kickstarter_pivot_pct = kickstarter_pivot.apply(lambda x: x / x.sum(), axis=1)
# Make percentages from proportions
kickstarter_pivot_pct = kickstarter_pivot_pct.apply(lambda x: x * 100)
# Round percentages to whole numbers and change type to int
kickstarter_pivot_pct = kickstarter_pivot_pct.round()
kickstarter_pivot_pct = kickstarter_pivot_pct.astype(int)
# Filter successful and failed projects
kickstarter_pivot_pct = kickstarter_pivot_pct[['successful', 'failed']]
# Save the results to a csv
# kickstarter_pivot_pct.to_csv('kickstarter_pivot_pct.csv')

# How do staff pick projects differ from normal projects in terms of backers?
# Create year column from 'created_at' column using datetime
kickstarter['created_at'] = pd.to_datetime(kickstarter['created_at'])
kickstarter['year'] = kickstarter['created_at'].dt.year
# Group by 'Staff Pick' and year in column 'created_at' and sum the 'backers_count'
kickstarter_backers = kickstarter.groupby(['staff_pick', 'year'])['backers_count'].sum()
# Unstack the 'Staff Pick' column
kickstarter_backers = kickstarter_backers.unstack(level=0)
# Rename the columns
kickstarter_backers = kickstarter_backers.rename(columns={False: 'Normal projects', True: 'Kickstarter picks'})
# Drop rows with missing values
kickstarter_backers = kickstarter_backers.dropna(axis=0)

# Plots
# Create figure with two subplots
fig, (ax1, ax2) = plt.subplots(2, 1, facecolor='#f0eeee')
fig.set_size_inches(8, 12)
# Plot the line graph of 'Staff Pick' and 'backers_count'
kickstarter_backers.plot(kind='line', ax=ax1, color=['#c449cc', '#525252'], marker='o', linewidth=1, ms=2)
# Change the line type
plt.rcParams['lines.linestyle'] = '-'
# Fill the area between the two lines depending on which line is higher
ax1.fill_between(kickstarter_backers.index, kickstarter_backers['Normal projects'], kickstarter_backers['Kickstarter picks'], where=kickstarter_backers['Normal projects'] < kickstarter_backers['Kickstarter picks'], facecolor='#c449cc', alpha=0.2)
ax1.fill_between(kickstarter_backers.index, kickstarter_backers['Normal projects'], kickstarter_backers['Kickstarter picks'], where=kickstarter_backers['Normal projects'] > kickstarter_backers['Kickstarter picks'], facecolor='#525252', alpha=0.2)
# Put text on the plot in the middle of the area between the two lines between 2020 and 2022

# Make the plot look nice
ax1.set_title('How do staff pick projects differ from normal projects in terms of backers?', fontsize=20, color=font_color, **csfont)
ax1.set_xlabel('', fontsize=16, color=font_color, **hfont)
ax1.set_ylabel('Number of backers', fontsize=16, color=font_color, **hfont)
# Change legend title and font size
ax1.legend(title='', title_fontsize=16, fontsize=16)
ax1.legend(loc=2, prop={'size': 10})


# Plot line on that figure
kickstarter_pivot_pct.plot(kind='line', ax=ax2, color=['#c449cc', '#525252'], marker='o', linewidth=3, ms=2)
# Change line color (red and blue)
plt.rcParams['lines.color'] = '#1f77b4'
# Set y-axis range from 0 to 1
ax2.set_ylim(0, 100)
# Make the x-axis only True and False
ax2.set_xticks([0, 1], ['Normal projects', 'Kickstarter picks'])
# Change font size of x-axis labels
ax2.tick_params(axis='x', labelsize=16)
# Annotate the difference between starting and ending values for both lines
ax2.annotate('The proportion of SUCCESSFUL projects is 87%,' +
             '\n(if you are a Kickstarter pick)',
             xy=(1, kickstarter_pivot_pct['successful'][1]),
             xytext=(1.1, kickstarter_pivot_pct['successful'][1] + 0.01),
             color = ann_color,
             arrowprops=dict(facecolor='black', shrink=0.01))
ax2.annotate('The proportion of FAILED projects is only 9%,' +
             '\n(if you are a Kickstarter pick)',
             xy=(1, kickstarter_pivot_pct['failed'][1]),
             xytext=(1.1, kickstarter_pivot_pct['failed'][1] - 0.01),
             arrowprops=dict(facecolor='black', shrink=0.05))
# Plot scatter on that figure
y = [40, 44, 87, 9]
x = ['Normal projects', 'Normal projects', 'Kickstarter picks', 'Kickstarter picks']
def set_marker_size(y, factor):
    return [i * factor for i in y]

ax2.scatter(x, y, s=set_marker_size(y, 4), color=['#c449cc', '#525252', '#c449cc', '#525252'])
# Make the plot look nice
ax2.set_title('Do Kickstarter Picks help projects to succeed?', fontsize=20, color=font_color, **csfont)
ax2.set_xlabel(' ', fontsize=16, color=font_color, **hfont)
ax2.set_ylabel('Percentage of total projects', fontsize=16, color=font_color, **hfont)
# Change legend title and font size
ax2.legend(title='', title_fontsize=16, fontsize=16)
ax2.legend(loc=2, prop={'size': 10})
# Save the plot
plt.savefig('kickstarter_picks.png', dpi=300, bbox_inches='tight')


