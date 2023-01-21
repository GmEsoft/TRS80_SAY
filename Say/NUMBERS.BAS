10 PRINT"** SAY/DVR Demo Program **"
20 PRINT
30 ON ERROR GOTO 260
40 OPEN "O",1,"*SY"
50 ON ERROR GOTO 0
60 PRINT #1,"|e1 ";
70 DIM U$(20):FOR I=0 TO 20:READ U$(I):NEXT
80 DATA zero,one,two,three,four,five,six,seven,eight,nine
90 DATA ten,eleven,twelve,thirteen,fourteen,fifteen,sixteen,seventeen,eighteen,nineteen,twenty
100 DIM T$(10):FOR I=0 TO 9:READ T$(I):NEXT
110 DATA zero,ten,twenty,thirty,forty,fifty,sixty,seventy,eighty,ninety
120 FOR I=0 TO 99
130 N=I
140 GOSUB 170
150 NEXT
160 END
170 '--- Subroutine to say number in N (between 0 and 99)
180 IF N<20 THEN 230
190 PRINT #1,T$(N\10);
200 N=N MOD 10
210 IF N=0 THEN PRINT #1,"." : GOTO 240
220 PRINT #1," ";
230 PRINT #1,U$(N);"."
240 PRINT
250 RETURN
260 '--- Error handler
270 PRINT "Can't open driver. Error code:";ERR
280 PRINT"Load SAY/DVR from LS-DOS with the following command:"
290 PRINT"  SET *SY SAY"
300 RESUME 310
310 ON ERROR GOTO 0
320 END
