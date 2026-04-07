# import nice libraries
import pandas as pd
import numpy as np

# CHANGE THIS to your csv file name
FILE_NAME = '1'

df = pd.read_csv(FILE_NAME + '.csv')

# Sort chronologically within each game (JUST IN CASE, but should already be sorted)
df = df.sort_values(['gameid', 'compiledgametime']).reset_index(drop=True)

# Tag each faceoff with a unique sequence ID
df['faceoff_sequence_id'] = np.nan
df['faceoff_time'] = np.nan

# Mark faceoff events and assign sequence IDs and times
faceoff_mask = df['eventname'] == 'faceoff'
df.loc[faceoff_mask, 'faceoff_sequence_id'] = df.index[faceoff_mask]
df.loc[faceoff_mask, 'faceoff_time'] = df.loc[faceoff_mask, 'compiledgametime']

# Forward fill within each game so every event knows its preceding faceoff
df['faceoff_sequence_id'] = df.groupby('gameid')['faceoff_sequence_id'].ffill()
df['faceoff_time'] = df.groupby('gameid')['faceoff_time'].ffill()
df['faceoff_period'] = df.groupby('gameid')['period'].transform(
    lambda x: x.where(faceoff_mask.loc[x.index]).ffill()
)

# Apply filters
result = df[
    # Must have a preceding faceoff (or be the faceoff itself)
    (df['faceoff_sequence_id'].notna()) &
    # Within 10 seconds of the faceoff
    (df['compiledgametime'] <= df['faceoff_time'] + 10) &
    # Same period as the faceoff
    (df['period'] == df['faceoff_period'])
].copy()

# export
result = result.drop(columns=['faceoff_period'])
result.to_csv('post_faceoff_events.csv', index=False)

# summary of the csv made in terminal
print(f"Done! {len(result)} events across {result['faceoff_sequence_id'].nunique()} faceoff sequences.")