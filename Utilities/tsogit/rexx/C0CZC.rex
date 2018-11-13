/*REXX EXEC*/
ADDRESS ISREDIT "MACRO (COMPTYP) NOPROCESS"
MSG1 = "     !!!!!!!!! DO NOT REMOVE THESE LINES !!!!!!!!!     "
MSG2 = "     ENTER 5 POSITION " ||COMPTYP|| " NAMES - 1 PER LINE ",   
       || "IN COLUMNS 1 THROUGH 5 "                                   
MSG3 = "     !!!!!!!!! DO NOT REMOVE THESE LINES !!!!!!!!!     "
ADDRESS ISREDIT "LINE_AFTER 0 =  (MSG1)"              
ADDRESS ISREDIT "LINE_AFTER 1 =  (MSG2)"              
ADDRESS ISREDIT "LINE_AFTER 2 =  (MSG3)"              
