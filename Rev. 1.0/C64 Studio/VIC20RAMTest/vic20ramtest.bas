10 N$="OVERLAY":REM THE ASSEBLER PROGRAM
20 SYS57809(N$),8,1:REM SET LOAD PARAMETERS
30 POKE780,0
40 SYS65493:REM CLOAD
50 SYS4864:REM START RAM TEST