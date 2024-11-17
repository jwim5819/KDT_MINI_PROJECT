# for mac
import matplotlib
matplotlib.use("TkAgg")
import os
os.chdir(os.path.dirname(os.path.abspath(__file__)))


import time
import tkinter
import tkinter.font
import tkinter.messagebox
import tkinter.ttk as ttk
from tkinter import *
from tkinter import simpledialog
import pandas
import time as t
import mainfunc
import ecxelTotxt
import tqdm
import os

i = 0
chosung = ''
name = ''
end = 0
start = 0

DIR_WORD_PATH = r'../word'
# 처음 실행시 사전파일 텍스트파일로 변환
if len(os.listdir(DIR_WORD_PATH))!=191:
    excelList = os.listdir('../dic/')
    for i in tqdm.tqdm(excelList, desc='Loading... ', ncols=80, unit='단어'):
        ecxelTotxt.ETT(i)
        i = i[:-5] + '.txt'
        ecxelTotxt.clean_txt(i)
        ecxelTotxt.set_txt(i)
        ecxelTotxt.chosung_txt(i)


    ecxelTotxt.refile(DIR_WORD_PATH)
    ecxelTotxt.redouble(DIR_WORD_PATH)


# class로 chosungApp 기본 프레임 묶어주기
class chosungApp(Tk):
    def __init__(w):
        super().__init__()

        w.title('아름다운 우리말 초성게임')
        w.geometry('500x500')
        w.working_frame = ttk.Frame(w)
        w.working_frame.grid(row=0, column=0, sticky='nsew')
        w.window1()

    # 전환 창 정의
    def window1(w):
        w.working_frame.destroy()
        w.working_frame = ttk.Frame(w, width=500, height=500, relief='groove')
        w.working_frame.pack_propagate(False)
        # 메인 gui 이미지 넣기
        main = PhotoImage(file='../image/mainimage.png')
        main_label = tkinter.Label(w.working_frame, image=main)
        main_label.place(x=0, y=0)
        # 랭킹보기 클릭하면 rank.xlxs 파일을 txt로 변환 후 읽어서 listbox에 표시한다.
        ttk.Button(w.working_frame, text='랭킹보기', width=7, command=w.window2).place(x=65, y=450)
        ttk.Button(w.working_frame, text='퀴즈풀기', width=7, command=w.window3).place(x=200, y=450)
        ttk.Button(w.working_frame, text='종료', width=7, command=w.destroy).place(x=335, y=450)
        w.working_frame.pack(side='top', pady=(0, 0))
        w.mainloop()

    def window2(w):
        w.working_frame.destroy()
        w.working_frame = ttk.Frame(w, width=500, height=500, relief='groove')
        w.working_frame.pack_propagate(False)
        font = tkinter.font.Font(size=30, weight='bold')
        ttk.Label(w.working_frame, text='랭 킹 현 황', font=font).place(x=148, y=32)
        ttk.Button(w.working_frame, text='퀴즈풀기', command=w.window3, width=7).place(x=50, y=450)
        ttk.Button(w.working_frame, text='초기화면', command=w.window1, width=7).place(x=185, y=450)
        ttk.Button(w.working_frame, text='종료', width=7, command=w.destroy).place(x=320, y=450)
        font = tkinter.font.Font(size=16, weight='bold')
        # rnak엑셀파일 열어서 txt 저장
        data = pandas.read_excel("./rank.xlsx")
        row = data.iloc[:, :4]
        row.to_csv('rank.txt', index=0)
        # 리스트 박스에 순위 담기
        listbox1 = Listbox(w.working_frame, width=6, height=11, font=font)
        listbox1.place(x=30, y=110)
        listbox2 = Listbox(w.working_frame, width=14, height=11, font=font)
        listbox2.place(x=95, y=110)
        listbox3 = Listbox(w.working_frame, width=7, height=11, font=font)
        listbox3.place(x=240, y=110)
        listbox4 = Listbox(w.working_frame, width=13, height=11, font=font)
        listbox4.place(x=310, y=110)
        with open(r'./rank.txt', mode='r', encoding='utf-8') as rank_txt:
            for i in range(1, 11):
                if i == 1:
                    txt = str(rank_txt.readlines(i))
                    txt = txt.split(',')
                    listbox1.insert(i, '  순위')
                    listbox2.insert(i, '       ' +'닉네임')
                    listbox3.insert(i, ' ' + ' 시간')
                    listbox4.insert(i, '        ' + '기록시간')
                txt = str(rank_txt.readlines(i))
                txt = txt.split(',')
                time = txt[3].split('/')[1] + '/' + txt[3].split('/')[2][:8]
                listbox1.insert(i, '    ' + txt[0][2:])
                listbox2.insert(i, '   ' + txt[1])
                listbox3.insert(i, '  ' + txt[2])
                listbox4.insert(i, '  ' + time)
        w.working_frame.pack(side='top', pady=(0, 0))

    def window3(w):
        w.working_frame.destroy()
        w.working_frame = ttk.Frame(w, width=500, height=500, relief='groove')
        w.working_frame.pack_propagate(False)

        def nickname():
            how_btn['state'] = 'disabled'
            start_btn['state'] = 'disabled'
            # 닉네임 입력받는 부분, name으로 저장된다.
            a = 0
            while a == 0:
                global name
                name = tkinter.simpledialog.askstring('닉네임 입력', '')
                if name != '':
                    a = 1
                else:
                    tkinter.messagebox.showwarning('', '닉네임을 입력하세요')
                    a = 0
            # 시작시간 수정
            global start
            start = time.time()
            time2 = '시작시간 : ' + t.strftime('%H:%M:%S') + '(시:분:초)'
            start_time = Label(w.working_frame, text=time2, font=font, width=25)
            start_time.place(x=65, y=35)
            chosung_display()
            # 시작시간 표시 후 곧바로 게임 시작
        # 초성제시 -> 정답인풋 -> txt파일의 데이터와 비교 -> (정답)프로그래스바+=1, 다음초성 제시 -> (정답5개누적) 종료시간 표시 -> 축하합니다! 걸린시간 ' ' 출력 -> 랭킹 제
        #                                           -> (오답)메인메뉴로 이동 -> game over, '좀 더 공부하세요' 출력
        # 초성제시

        def chosung_display():
            global chosung
            chosung = mainfunc.givechosung()
            chosung1 = chosung[:1]
            chosung2 = chosung[1:2]
            chosung1 = Label(w.working_frame, text=chosung1, font=tkinter.font.Font(size=40, weight='bold'), height=1, width=2, anchor='center', relief='ridge', borderwidth=5)
            chosung1.place(x=125, y=130)
            chosung2 = Label(w.working_frame, text=chosung2, font=tkinter.font.Font(size=40, weight='bold'), height=1, width=2, anchor='center', relief='ridge', borderwidth=5)
            chosung2.place(x=245, y=130)
        # 정답인풋

        def chosung_input2(event):
            result = chosung_input.get()
            chosung_input.delete(0, "end")
            if mainfunc.iscorrect(result, chosung) == True:
                chosung_display()
                global i
                i += 1
                curr_progress.set(i)
                progress_bar.update()
                if i == 5:
                    global end
                    end = time.time()
                    mainfunc.save_result(name, end, start)
                    i = 0
                    curr_progress.set(i)
                    progress_bar.update()
                    time3 = '종료시간 : ' + t.strftime('%H:%M:%S') + '(시:분:초)'
                    end_time = Label(w.working_frame, text=time3, font=font, width=25)
                    end_time.place(x=65, y=75)
                    tkinter.messagebox.showwarning('', 'GAME CLEAR   축하합니다!')
                    w.window1()
                    # 데이터 저장
            else:
                tkinter.messagebox.showwarning('', 'GAME OVER   좀 더 공부하세요!')
                i = 0
                curr_progress.set(i)
                progress_bar.update()
                w.window1()
            # if True 이면 다시 초성 제시 후 비교
            # i += 한다.
            # if i == 5면 게임 클리어
            # False 이면 메인메뉴

        # 버튼삽입
        how_btn = Button(w.working_frame, text='게임설명', height=2, width=7, command=w.window4)
        how_btn.place(x=30, y=400)
        # 게임시작을 누르면 timer 함수가 실행된다.
        start_btn = Button(w.working_frame, text='게임시작!', height=2, width=7, command=nickname)
        start_btn.place(x=185, y=400)
        exit_btn2 = Button(w.working_frame, text='종   료', height=2, width=7, command=w.destroy)
        exit_btn2.place(x=340, y=400)
        # 초성 표시 부분
        chosung1 = Label(w.working_frame, height=4, width=8, anchor='center', relief='ridge', borderwidth=5)
        chosung1.place(x=125, y=130)
        chosung2 = Label(w.working_frame, height=4, width=8, anchor='center', relief='ridge', borderwidth=5)
        chosung2.place(x=245, y=130)
        # 남은 단어 시각화
        curr_progress = tkinter.DoubleVar()
        progress_bar = tkinter.ttk.Progressbar(w.working_frame, length=300, maximum=5, variable=curr_progress)
        progress_bar.place(x=87, y=255)
        # 정답 입력 부분
        font = tkinter.font.Font(size=40)
        chosung_input = Entry(w.working_frame, font=font, width=10, justify='center')
        chosung_input.place(x=70, y=300)


        chosung_input.bind("<Return>", chosung_input2)
        # 라운드 수 표시
        font = tkinter.font.Font(size=18, weight='bold')
        start_time = Label(w.working_frame, text='시작시간 : --:--:--(시:분:초)', font=font, width=25)
        start_time.place(x=65, y=35)
        end_time = Label(w.working_frame, text='종료시간 : --:--:--(시:분:초)', font=font, width=25)
        end_time.place(x=65, y=75)
        w.working_frame.pack(side='top', pady=(0, 0))

    def window4(w):
        w.working_frame.destroy()
        w.working_frame = ttk.Frame(w, width=500, height=500, relief='groove')
        w.working_frame.pack_propagate(False)
        page = PhotoImage(file='../image/guide.png')
        page_label = tkinter.Label(w.working_frame, image=page)
        page_label.place(x=0, y=0)
        ttk.Button(w.working_frame, text='초기화면', command=w.window1, width=17).place(x=190, y=465)
        w.working_frame.pack(side='top', pady=(0, 0))
        w.mainloop()


if __name__ == '__main__':
    chosungApp().mainloop()