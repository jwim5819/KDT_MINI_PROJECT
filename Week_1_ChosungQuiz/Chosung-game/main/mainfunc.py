import time
import pandas as pd
import os
import openpyxl
import random


DIR_WORD_PATH='../word/'
DIRC_EXCEL='./rank.xlsx'


# 플레이어 정답 저장
answerlist=[]
select=''  # 초성 파일
selectfile = ''

# 플레이어한테 초성 제시
def givechosung():
    """Return txt file name as a string"""
    randomfile=random.choice(os.listdir(DIR_WORD_PATH))
    return randomfile


def iscorrect(player_say, select):
    """Returns true if player's answer in a answer list, otherwise false"""
    global answerlist
    with open(DIR_WORD_PATH+select, encoding='utf-8') as f:
        if player_say in f.read():
            answerlist.append(player_say)
            return True
        else: return False

# 2번째 입력부터 저장된 정답 리스트에 있는 단어인지 확인
def isduplicate(player_say):
    """Returns true if not player's answer is not existing in a answer list"""
    if not answerlist:
        return iscorrect(player_say)
    else:
        if iscorrect(player_say):
            for answer in answerlist:
                if player_say is not answer:
                    return True
            else: return False

# 랭킹 확인
def highscore():
    # rank.xlsx 열기, 시간
    highscore=pd.read_excel(DIRC_EXCEL, engine='openpyxl', index_col=0, usecols=[0, 1, 2, 3])
    return highscore.loc[1:10]  # 10등까지만 보여주기


# 게임 결과 저장
def save_result(player, end, start):
    if not os.path.exists(DIRC_EXCEL):  # rank.xlsx 파일 없을시 생성
        df=pd.DataFrame({'이름':{0:"-", 1:"-", 2:"-", 3:"-", 4:"-", 5:"-", 6:"-", 7:"-", 8:"-", 9:"-"},
                           '기록':{0:999, 1:999, 2:999, 3:999, 4:999, 5:999, 6:999, 7:999, 8:999, 9:999},
                           '시간':{0:"00/00/00 00:00:00", 1:"00/00/00 00:00:00", 2:"00/00/00 00:00:00",
                                 3:"00/00/00 00:00:00", 4:"00/00/00 00:00:00", 5:"00/00/00 00:00:00",
                                 6:"00/00/00 00:00:00", 7:"00/00/00 00:00:00", 8:"00/00/00 00:00:00",
                                 9:"00/00/00 00:00:00"},
                           '시간값':{0:0, 1:0, 2:0, 3:0, 4:0, 5:0, 6:0, 7:0, 8:0, 9:0}})
        filename='rank.xlsx'
        df.to_excel(filename)


        data=pd.read_excel(DIRC_EXCEL, engine='openpyxl', index_col=0)  # 랭킹 엑셀 읽기
        player=player.replace(' ', '')  # 공백 제거
        playtime = end - start
        rank_time=time.localtime()  # 현재 시간 저장
        data.loc[11]=[player, round(playtime, 2), time.strftime("%y/%m/%d %H:%M:%S", rank_time), f'{time.time():.2f}']
        # 새로운 게임 결과를 11등 위치에 저장

        cols=['기록', '시간값']
        tups=data[cols].sort_values(by=cols, ascending=(True, False)).apply(tuple, axis=1)
        # 기록이 낮을수록, 기록이 같다면 시간값이 클수록 순위가 높게

        # 기록, 시간값 2가지 기준에 따라 정렬
        f, i=pd.factorize(tups)
        factorized=pd.Series(f+1, tups.index)

        # 새로운 순위를 'rank'행에 저장
        data['rank'] = factorized
        data = data.sort_values(by=['rank'])  # 'rank'에 따라 정렬
        data.index = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]  # 다시 1~10위 부여하기
        data = data.drop('rank', axis=1)  # 'rank'행 삭제
        data = data.drop(11)  # 11위 삭제
        data.to_excel(DIRC_EXCEL)  # rank.xlsx에 덮어쓰기

    else:
        data=pd.read_excel(DIRC_EXCEL, engine='openpyxl', index_col=0)  # 랭킹 엑셀 읽기
        player=player.replace(' ', '')  # 공백 제거
        playtime = end - start
        rank_time=time.localtime()  # 현재 시간 저장
        data.loc[11]=[player, round(playtime, 2), time.strftime("%y/%m/%d %H:%M:%S", rank_time), f'{time.time():.2f}']
        # 새로운 게임 결과를 11등 위치에 저장

        cols=['기록', '시간값']
        tups=data[cols].sort_values(by=cols, ascending=(True, False)).apply(tuple, axis=1)
        # 기록이 낮을수록, 기록이 같다면 시간값이 클수록 순위가 높게

        # 기록, 시간값 2가지 기준에 따라 정렬
        f, i=pd.factorize(tups)
        factorized=pd.Series(f+1, tups.index)

        # 새로운 순위를 'rank'행에 저장
        data['rank'] = factorized
        data = data.sort_values(by=['rank'])  # 'rank'에 따라 정렬
        data.index = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]  # 다시 1~10위 부여하기
        data = data.drop('rank', axis=1)  # 'rank'행 삭제
        data = data.drop(11)  # 11위 삭제
        data.to_excel(DIRC_EXCEL)  # rank.xlsx에 덮어쓰기
