#!/usr/bin/python
"""
Copyright 2016 Accenture and/or its affiliates.  All Rights Reserved.  
You may not use, copy, modify, and/or distribute this code and/or its documentation without permission from Accenture.
Please contact the Advanced Analytics-Operations Analytics team and/or Frode Huse Gjendem (lead) with any questions.

\brief This is the starter script for the Accenture's Datathon 2016 competition.

\version 1.0

\date $Date: 2016/05/17

"""
import os
import gc
import pandas as pd
import numpy as np
import re
import csv
from datetime import datetime, timedelta
from collections import OrderedDict
from itertools import product

# Enter your input data and output data paths below.
PATH = ".\datathon\data"
OUTPATH = ".\datathon\results"
# Set the input data folder as default path.
os.chdir(PATH)

def as_Date(stringDate):
    """ It formats the given string date. 
        @param stringDate is the input string containing a date.
        @return a datetime object.
    """  
    month = re.sub('(^[0-9]{4})-([0-9]{1,2}|[a-zA-Z]{3,9})-([0-9]{1,2}|[a-zA-Z]{3,9})$', '\\2', stringDate)
    result = np.nan
    if str.isalpha(month):
        result = datetime.strptime(stringDate, "%Y-%B-%d")
    return result
    
def naive_model(training):
    """ It computes the naive mean model.
        @param training is the pandas data frame with the data.
        @return a dictionary-based model, where the key is the 
                composition of <month, Shift, GridID>.
    """
    aggregated_data = training.Accident.groupby([training.month, training.Shift, training.GridID])
    count_agg = aggregated_data.count()
    sum_agg = aggregated_data.sum()
    mean_agg = sum_agg/count_agg
    dict_agg = mean_agg.to_dict()
    return dict_agg

# Read the input files. Notice that it is necessary to use encoding = 'cp1252' to fix issues with file encoding. 
accidents = pd.read_csv("accidents.csv", encoding = 'cp1252') 
grid = pd.read_csv("city-grid.csv")
test = pd.read_csv("test.csv")

# Format the dates.
accidents.dropna(inplace = True)
accidents.GridID = accidents.GridID.astype(int)
for i in range(0, len(accidents.date)):
    accidents.date.values[i] = as_Date(accidents.date.values[i])

# Group at prediction level.
data = accidents[['GridID', 'date', 'Shift']].copy()
data.dropna(inplace = True)
data['Accident'] = pd.Series(1, index = data.index)

# Generate the no accidents data.
start = datetime.strptime("2010-01-01", "%Y-%m-%d")
end = datetime.strptime("2015-01-01", "%Y-%m-%d")
dates = [start + timedelta(days = x) for x in range(0, (end - start).days)]
Shift = data.Shift.unique()
GridID = data.GridID.unique()
tmp = {'date': dates, 'Shift': Shift, 'GridID': GridID}
# This is the typical cartessian product... pandas version...
od_tmp = OrderedDict(sorted(tmp.items()))
cartProd_tmp = list(product(*od_tmp.values()))
noAccidents = pd.DataFrame(cartProd_tmp, columns = od_tmp.keys())
noAccidents['Accident'] = pd.Series(0, index = noAccidents.index)
# Join the data together.
frames = [data, noAccidents] 
training = pd.concat(frames)
training = pd.merge(training, grid, on = 'GridID', how = 'left')
del dates, frames,  start, end, od_tmp, cartProd_tmp, accidents, noAccidents, data, tmp, Shift, grid, GridID
gc.collect()

# Run the naive model.
training['month'] = pd.DatetimeIndex(training.date).month
model = naive_model(training)

# Perform the prediction.
for i in range(0, len(test.date)):
    test.date.values[i] = datetime.strptime(test.date.values[i], "%Y-%m-%d")
test['month'] = pd.DatetimeIndex(test.date).month
test['AccidentLikelihood'] = pd.Series(0.0, index = test.index)
for i in range(0, len(test)):
    value = 0.0
    try:
        value = model[test.month.values[i], test.Shift.values[i], test.GridID.values[i]]
    except KeyError:
        pass
    test.AccidentLikelihood.values[i] = value

# Save the submission
submission = test[['date', 'Shift', 'GridID', 'AccidentLikelihood']].copy()
# Reshape the time to fit the submission format...
for i in range(0, len(submission)):
    submission.date.values[i] = datetime.strftime(submission.date.values[i], "%Y-%m-%d")
# Write the final CSV file.
submission.to_csv(OUTPATH + "/submission.csv", quoting = csv.QUOTE_NONNUMERIC, index = False)

# Free memory.
del model, test, training, submission, i, value
gc.collect()
