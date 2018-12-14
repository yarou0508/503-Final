# -*- coding: utf-8 -*-
"""
Created on Tue Nov 27 14:45:16 2018

@author: 47532
"""

import plotly
plotly.tools.set_credentials_file(username = 'yx160',api_key = 'AcbKobOFUHf7UYvCtsPH')
import plotly.plotly as py
import plotly.graph_objs as go
import pandas as pd
from plotly import tools
h1b = pd.read_csv('cleaned_h1b_2016.csv')
x= ['CERTIFIED','CERTIFIED-WITHDRAWN','WITHDRAWN','DENIED']
y0 = h1b[h1b['CASE_STATUS'] == 'CERTIFIED']['duration']
y1 = h1b[h1b['CASE_STATUS'] == 'CERTIFIED-WITHDRAWN']['duration']
y2 = h1b[h1b['CASE_STATUS'] == 'WITHDRAWN']['duration']
y3 = h1b[h1b['CASE_STATUS'] == 'DENIED']['duration']

colors = ['rgba(93, 164, 214, 0.5)', 'rgba(255, 144, 14, 0.5)', 
          'rgba(44, 160, 101, 0.5)', 'rgba(207, 114, 255, 0.5)']

trace1 = go.Box(
            x = y0,
            name = x[0],
            marker=dict(
                color = colors[0],
            ))

trace2 = go.Box(
            x = y1,
            name = x[1],
            marker=dict(
                color = colors[1],
            ))

trace3 = go.Box(
            x = y2,
            name = x[2],
            marker=dict(
                color = colors[2],
            ))

trace4 = go.Box(
            x = y3,
            name = x[3],
            marker=dict(
                color = colors[3],
            ))

fig = tools.make_subplots(rows=4, cols=1)
fig.append_trace(trace1, 1, 1)
fig.append_trace(trace2, 2, 1)
fig.append_trace(trace3, 3, 1)
fig.append_trace(trace4, 4, 1)

fig['layout'].update(height=600, width=800, title='H1B processing time distribution') 
fig['layout']['yaxis1'].update(showticklabels=True, tickangle=60) 
fig['layout']['yaxis2'].update(showticklabels=True, tickangle=60) 
fig['layout']['yaxis3'].update(showticklabels=True, tickangle=60) 
fig['layout']['yaxis4'].update(showticklabels=True, tickangle=60) 
py.iplot(fig, filename='sub_boxplot-h1b-duration')
plotly.offline.plot(fig, filename = 'sub_boxplot-h1b-duration.html')
                                                        
