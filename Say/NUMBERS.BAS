10 PRINT"** SAY/DVR Demo Program **"
20 PRINT
30 ON ERROR GOTO 240
40 OPEN "O",1,"*SY"
50 ON ERROR GOTO 0
60 DIM U$(20):FOR I=0 TO 20:READ U$(I):NEXT
70 DATA zero,one,two,three,four,five,six,seven,eight,nine
80 DATA ten,eleven,twelve,thirteen,fourteen,fifteen,sixteen,seventeen,eighteen,nineteen,twenty
90 DIM T$(10):FOR I=0 TO 9:READ T$(I):NEXT
100 DATA zero,ten,twenty,thirty,forty,fifty,sixty,seventy,eighty,ninety
110 FOR I=0 TO 99
120 N=I
130 GOSUB 160
140 NEXT
150 END
160 '--- Subroutine to say number in N (between 0 and 99)
170 IF N<20 THEN 220
180 PRINT T$(N\10); : PRINT #1,T$(N\10);
190 N=N MOD 10
200 IF N=0 THEN PRINT "." : PRINT #1,"." : RETURN
210 PRINT" "; : PRINT #1," ";
220 PRINT U$(N);"." : PRINT #1,U$(N);"."
230 RETURN
240 '--- Error handler
250 PRINT "Can't open driver. Error code:";ERR
260 PRINT"Load SAY/DVR from LS-DOS with the following command:"
270 PRINT"  SET *SY SAY"
280 RESUME 290
290 ON ERROR GOTO 0
300 END
