# -*- coding: utf-8 -*-
"""
Created on Fri Nov 30 21:51:15 2018

@author: 47532
"""
from wordcloud import WordCloud
import matplotlib.pyplot as plt
from PIL import Image
import numpy as np
import pandas as pd
import nltk

h1b = pd.read_csv('H-1B_Disclosure_Data_FY17.csv')
h1b = h1b[h1b['CASE_STATUS'] == 'CERTIFIED']
h1b = h1b.dropna(subset=['JOB_TITLE']).reset_index()
text = ''
for i in range(len(h1b)):   
    for w in nltk.word_tokenize(h1b['JOB_TITLE'][i]):
        if w.isalpha():
            text += w + ' '

mask = np.array(Image.open('Trump.jpg'))   
wc = WordCloud(background_color="white",
               random_state=5, max_words=1000, mask = mask).generate(text)
plt.figure(figsize=(20,15))
plt.imshow(wc)
plt.axis("off")
plt.savefig('H1B_Job_Title.png')