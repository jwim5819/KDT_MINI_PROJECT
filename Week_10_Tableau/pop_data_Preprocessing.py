# Module Loading
import pandas as pd
import numpy as np
import re


# Data Processing
def data_processing(filepath, savename):
    data = pd.read_csv(filepath, encoding='cp949')
    data.insert(0, '시,도', np.NaN)
    data.insert(1, '시,군,구', np.NaN)
    for i in range(len(data['행정구역'])):
        data['행정구역'][i] = data['행정구역'][i].strip()
        split_address = data['행정구역'][i].split(' ')
        if len(split_address) == 1:
            data['시,도'][i] = split_address[0]
            data['시,군,구'][i] = split_address[0]
        if len(split_address) == 2:
            if re.search('((시|도)$)', split_address[0]):
                data['시,도'][i] = split_address[0]
                data['시,군,구'][i] = split_address[1]
        if len(split_address) == 3:
            data['시,도'][i] = split_address[1]
            data['시,군,구'][i] = split_address[2]

    data.to_csv(savename, encoding='cp949')

# for i in range(2012, 2022):
#     data_processing(f'./popdata/{i}12_{i}12_연령별인구현황_연간.csv', f'{i}.csv')
data_processing(f'./popdata/202112_202112_주민등록인구및세대현황_연간.csv', f'2021남녀.csv')
