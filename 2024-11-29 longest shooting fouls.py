#!/usr/bin/env python
# coding: utf-8

# In[ ]:


#https://altair-viz.github.io/getting_started/starting.html


# In[1]:


import pandas as pd
import numpy as np


# In[23]:


import altair as alt
alt.data_transformers.disable_max_rows()


# In[45]:


#import data from a csv
wnba_pbp_2024 = pd.read_csv('/Users/maddy/Desktop/wnba_pbp_2024.csv')
wnba_pbp = pd.read_csv('/Users/maddy/Desktop/wnba_pbp_since_2010.csv')


# In[ ]:


print("number of shooting fouls in 2024")
len(wnba_pbp_2024[(wnba_pbp_2024['type_text'] == "Shooting Foul")])


# In[46]:


print("number of shooting fouls since 2010")
len(wnba_pbp[(wnba_pbp['type_text'] == "Shooting Foul")])


# In[15]:


wnba_pbp.head()


# In[16]:


# Assuming wnba_pbp is your DataFrame
column_names = wnba_pbp.columns
print(column_names)

# Print a specific range of rows, for example, rows 50 to 150
print(wnba_pbp['type_text'][50:150])


# In[25]:


import matplotlib.pyplot as plt

# record shooting fouls only, regular season only
subdata = wnba_pbp[(wnba_pbp['type_text'] == "Shooting Foul")]
subdata = subdata[['game_date', 'text', 'coordinate_x', 'coordinate_y', 'coordinate_x_raw', 'coordinate_y_raw']]

#print(subdata)


# In[26]:


#the horizontal axis was actually flipped, so flip it back by changing x coordinates to 50-x
subdata['flipped_x'] = 50 - subdata['coordinate_x_raw']


# In[30]:


# Create a DataFrame with the coordinates of the four points
court_coords = pd.DataFrame({
    'x': [0, 0, 50, 50, 0],   # x-coordinates of the four points
    'y': [90, -4, 90, -4, 90]  # y-coordinates of the four points
})

# Create a DataFrame with the coordinates of the horizontal lines
horizontal_lines = pd.DataFrame({
    'y': [-4, 43]  # y-coordinates of the horizontal lines
})

# Draw the horizontal lines using mark_rule()
horizontal_lines_chart = alt.Chart(horizontal_lines).mark_rule(color='black', strokeWidth=2.5, strokeOpacity=1).encode(
    y='y:Q'
)

#court is 50 feet wide and 94 feet long. basket located at approximately (25,0)
# Draw the basketball court outline rectangle using mark_line()
court_rect = alt.Chart(court_coords).mark_line(color='black', strokeWidth=2.5, strokeOpacity=1).encode(
    x='x:Q',
    y='y:Q'
)

# Create a DataFrame with the center point of the circle
circle_center = pd.DataFrame({
    'x': [25],  # x-coordinate of the center point
    'y': [43]   # y-coordinate of the center point
})

#center court circle 12 feet in diameter
# Draw the circle using mark_point() with shape='circle' and filled=False
circle_chart = alt.Chart(circle_center).mark_point(shape='circle', size=144*60, color='black', opacity=1, filled=False).encode(
    x='x:Q',
    y='y:Q'
)

# Create a DataFrame with the center point of the circle
basket_center = pd.DataFrame({
    'x': [25],  # x-coordinate of the center point
    'y': [0.9]   # y-coordinate of the center point
})

#center court circle 12 feet in diameter
# Draw the circle using mark_point() with shape='circle' and filled=False
basket_chart = alt.Chart(basket_center).mark_point(shape='circle', size=144, color='black', opacity=1, filled=False).encode(
    x='x:Q',
    y='y:Q'
)

# Create a DataFrame with the center point of the circle
basket_center2 = pd.DataFrame({
    'x': [25],  # x-coordinate of the center point
    'y': [85.1]   # y-coordinate of the center point
})

#center court circle 12 feet in diameter
# Draw the circle using mark_point() with shape='circle' and filled=False
basket_chart2 = alt.Chart(basket_center2).mark_point(shape='circle', size=144, color='black', opacity=1, filled=False).encode(
    x='x:Q',
    y='y:Q'
)

# Define the vertical lines coordinates
vertical_lines_data = pd.DataFrame({
    'x': [3.75, 46.25],
    'y_start': [-4, -4],
    'y_end': [1.25, 1.25]
})

vertical_lines = alt.Chart(vertical_lines_data).mark_rule(color='black', strokeWidth=2).encode(
    x=alt.X('x:Q'),
    y=alt.Y('y_start:Q'),
    y2=alt.Y2('y_end:Q')
)

# Define the backboard lines coordinates
horizontal_lines_data = pd.DataFrame({
    'y': [86, 0],
    'x_start': [22, 22],
    'x_end': [28, 28]
})

horizontal_lines = alt.Chart(horizontal_lines_data).mark_rule(color='black', strokeWidth=2.8).encode(
    y=alt.Y('y:Q'),
    x=alt.X('x_start:Q'),
    x2=alt.X2('x_end:Q')
)

# Define the free throw lines coordinates
horizontal_lines_data2 = pd.DataFrame({
    'y': [15, 71],
    'x_start': [19, 19],
    'x_end': [31, 31]
})

horizontal_lines2 = alt.Chart(horizontal_lines_data2).mark_rule(color='black', strokeWidth=2.5).encode(
    y=alt.Y('y:Q'),
    x=alt.X('x_start:Q'),
    x2=alt.X2('x_end:Q')
)

# Define the vertical lines coordinates
vertical_lines_data2 = pd.DataFrame({
    'x': [3.75, 46.25],
    'y_start': [90, 90],
    'y_end': [84.75, 84.75]
})

vertical_lines2 = alt.Chart(vertical_lines_data2).mark_rule(color='black', strokeWidth=2).encode(
    x=alt.X('x:Q'),
    y=alt.Y('y_start:Q'),
    y2=alt.Y2('y_end:Q')
)

# Define the coordinates for the two free throw lanes
vertical_lines_data3 = pd.DataFrame({
    'x': [19, 31],
    'y_start': [-4, -4],
    'y_end': [15, 15]
})

vertical_lines3 = alt.Chart(vertical_lines_data3).mark_rule(color='black', strokeWidth=2).encode(
    x=alt.X('x:Q'),
    y=alt.Y('y_start:Q'),
    y2=alt.Y2('y_end:Q')
)

# Define the coordinates for the two free throw lanes
vertical_lines_data4 = pd.DataFrame({
    'x': [19, 31],
    'y_start': [90, 90],
    'y_end': [71, 71]
})

vertical_lines4 = alt.Chart(vertical_lines_data4).mark_rule(color='black', strokeWidth=2).encode(
    x=alt.X('x:Q'),
    y=alt.Y('y_start:Q'),
    y2=alt.Y2('y_end:Q')
)

# Create a DataFrame with the center point of the circle
circle_center2 = pd.DataFrame({
    'x': [25],  # x-coordinate of the center point
    'y': [71]   # y-coordinate of the center point
})

# free throw circles
# Draw the circle using mark_point() with shape='circle' and filled=False
circle_chart2 = alt.Chart(circle_center2).mark_point(shape='circle', size=144*42, color='black', opacity=1, filled=False).encode(
    x='x:Q',
    y='y:Q'
)

# Create a DataFrame with the center point of the circle
circle_center3 = pd.DataFrame({
    'x': [25],  # x-coordinate of the center point
    'y': [15]   # y-coordinate of the center point
})

# free throw circles
# Draw the circle using mark_point() with shape='circle' and filled=False
circle_chart3 = alt.Chart(circle_center3).mark_point(shape='circle', size=144*42, color='black', opacity=1, filled=False).encode(
    x='x:Q',
    y='y:Q'
)

# Define the angle range for the arc (in radians)
theta = np.linspace(0, np.pi, 100)

# Calculate the x and y coordinates of the arc
arc_x = 25 + 21.25 * np.cos(theta)
arc_y = 0.9 + 21.25 * np.sin(theta)

# Create a DataFrame to hold the arc coordinates
arc_data = pd.DataFrame({'x': arc_x, 'y': arc_y})

# Draw the arc
arc = alt.Chart(arc_data).mark_line(color='black').encode(
    x=alt.X('x:Q'),
    y=alt.Y('y:Q')
)

# Define the angle range for the arc (in radians)
theta2 = np.linspace(np.pi, 2*np.pi, 100)

# Calculate the x and y coordinates of the arc
arc_x2 = 25 + 21.25 * np.cos(theta2)
arc_y2 = 85.1 + 21.25 * np.sin(theta2)

# Create a DataFrame to hold the arc coordinates
arc_data2 = pd.DataFrame({'x': arc_x2, 'y': arc_y2})

# Draw the arc
arc2 = alt.Chart(arc_data2).mark_line(color='black').encode(
    x=alt.X('x:Q'),
    y=alt.Y('y:Q')
)

# Base chart
base_chart = alt.Chart(subdata).mark_circle(size=80, opacity=0.3).encode(
    x=alt.X('flipped_x', scale=alt.Scale(domain=[0, 50]), title='Horizontal Court Location'),
    y=alt.Y('coordinate_y_raw', scale=alt.Scale(domain=[-3, 90]), title='Vertical Court Location'),
    tooltip=['game_date','text']
)

# Display plot to scale
#if we adjust the size of the plot the size of the plot points and court lines may also need to be adjusted
shot_plot = (circle_chart + horizontal_lines_chart + court_rect + base_chart+ basket_chart + basket_chart2 + vertical_lines + vertical_lines2 + arc + arc2 + horizontal_lines + horizontal_lines2 + vertical_lines3 + vertical_lines4 + circle_chart2 + circle_chart3).properties(
    title='WNBA Shooting Fouls Since 2010',
    width=400,  # Adjust width as needed
    height=800  #ratio should be 2:1 (or 1:2 idk)
).configure_axis(
    # ticks=False,
    # labels=False,
    # title=None
).configure_axisX(
    grid=False  # Remove gridlines from the x-axis
).configure_axisY(
    grid=False  # Remove gridlines from the y-axis
)


# Show plot
shot_plot


# In[38]:


import pandas as pd

# Assuming your DataFrame is named 'subdata'
subdata['distance_from_basket'] = ((subdata['coordinate_x_raw'] - 25)**2 + (subdata['coordinate_y_raw'] - 0)**2)**0.5

# Filter shots with distance greater than 22.15
shots_outside_three_point = subdata[subdata['distance_from_basket'] > 22.1]

# Display shots outside the three-point arc
print(shots_outside_three_point)

#three point shooting foul chart
# Define dropdown selection
text_dropdown2 = alt.selection_single(
    name='Select',
    fields=['text'],
    bind=alt.binding_select(options=shots_outside_three_point['text'].unique().tolist())
)

# Base chart with dropdown filter
base_chart2 = alt.Chart(shots_outside_three_point).mark_circle(size=80, opacity=0.2, color="red").encode(
    x=alt.X('flipped_x', scale=alt.Scale(domain=[0, 50]), title='Horizontal Court Location'),
    y=alt.Y('coordinate_y_raw', scale=alt.Scale(domain=[-3, 90]), title='Vertical Court Location'),
    tooltip=['game_date','text', 'distance_from_basket']
)


# Display plot to scale
#if we adjust the size of the plot the size of the plot points and court lines may also need to be adjusted
shot_plot2 = (circle_chart + horizontal_lines_chart + court_rect + base_chart2+ basket_chart + basket_chart2 + vertical_lines + vertical_lines2 + arc + arc2 + horizontal_lines + horizontal_lines2 + vertical_lines3 + vertical_lines4 + circle_chart2 + circle_chart3).properties(
    title='WNBA Shooting Fouls Beyond the Arc Since 2010',
    width=400,  # Adjust width as needed
    height=800  #ratio should be 2:1 (or 1:2 idk)
).configure_axis(
    # ticks=False,
    # labels=False,
    # title=None
).configure_axisX(
    grid=False  # Remove gridlines from the x-axis
).configure_axisY(
    grid=False  # Remove gridlines from the y-axis
)


# Show plot
shot_plot2


# In[47]:


print("number of shooting fouls beyond the arc since 2010")
len(shots_outside_three_point)


# In[50]:


#last second shooting fouls
#beyond the arc
filtered_wnba_pbp = shots_outside_three_point[
    (wnba_pbp['type_text'] == "Shooting Foul") &
    (wnba_pbp['clock_minutes'] == 0) &
    (wnba_pbp['clock_seconds'] <= 1)
]
filtered_wnba_pbp
#len(filtered_wnba_pbp)


# In[ ]:




