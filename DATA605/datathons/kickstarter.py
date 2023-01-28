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

# Set figure
fig, ax = plt.subplots(facecolor='#f0eeee')
fig.set_size_inches(4, 8)
# Plot line on that figure
kickstarter_pivot_pct.plot(kind='line', ax=ax, color=['#c449cc', '#525252'], marker='o', linewidth=3, ms=2)
# Change line color (red and blue)
plt.rcParams['lines.color'] = '#1f77b4'
# Set y-axis range from 0 to 1
plt.ylim(0, 100)
# Make the x-axis only True and False
plt.xticks([0, 1], ['Others', 'Kickstarter picks'])
# Change font size of x-axis labels
plt.xticks(fontsize=16)
# Annotate the difference between starting and ending values for both lines
plt.annotate('The proportion of SUCCESSFUL projects is 87%,' +
             '\nwhich is almost 50% more than Others' +
             '\n(if you are a Kickstarter pick)',
             xy=(1, kickstarter_pivot_pct['successful'][1]),
             xytext=(1.1, kickstarter_pivot_pct['successful'][1] + 0.01),
             color = ann_color,
             arrowprops=dict(facecolor='black', shrink=0.01))
plt.annotate('The proportion of FAILED projects is only 9%,' +
             '\n(if you are a Kickstarter pick)',
             xy=(1, kickstarter_pivot_pct['failed'][1]),
             xytext=(1.1, kickstarter_pivot_pct['failed'][1] - 0.01),
             arrowprops=dict(facecolor='black', shrink=0.05))
# Plot scatter on that figure
y = [40, 44, 87, 9]
x = ['Others', 'Others', 'Kickstarter picks', 'Kickstarter picks']
def set_marker_size(y, factor):
    return [i * factor for i in y]

ax.scatter(x, y, s=set_marker_size(y, 4), color=['#c449cc', '#525252', '#c449cc', '#525252'])
# Make the plot look nice
ax.set_title('Do Kickstarter Picks help projects to succeed?', fontsize=20, color=font_color, **csfont)
ax.set_xlabel(' ', fontsize=16, color=font_color, **hfont)
ax.set_ylabel('Percentage of total projects', fontsize=16, color=font_color, **hfont)
# Change legend title
ax.legend(title='', title_fontsize=16, fontsize=16)
# Change legend font size
plt.legend(loc=2, prop={'size': 10})
# Adjust subplots so that the title and labels would fit
plt.subplots_adjust(top=1, bottom=0.3, left=0.1, right=0.9)
# Save the plot
plt.savefig('kickstarter.png', bbox_inches='tight')

