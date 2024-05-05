import pandas as pd
import numpy as np

raabe = pd.read_csv('ioc-tb_raabe.csv')

def set_daughter_gen(parent_gen_number, parent_airway_index):
    daughter_gen = parent_gen_number+1	
    raabe.at[parent_airway_index, 'generation'] = daughter_gen	
    for airway in raabe.iloc[parent_airway_index]['daughter_branches'].strip('[]').split(', '):
        airway = int(airway)
        try:
            set_daughter_gen(daughter_gen, airway)
        except:
            pass

set_daughter_gen(-1,0)

pd.DataFrame.to_csv(raabe, 'ioc-tb_raabe-generations.csv')