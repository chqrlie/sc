# This data file was generated by the Spreadsheet Calculator.
# You almost certainly shouldn't edit it.

set extfun color colorneg colorerr
abbrev "a123 this is a cell name"
abbrev "abc this is normal"
abbrev "xxx this is an error"
format E 14 2 0
format F 13 0 0
format G 13 2 0
format I 7 0 0
format J 7 0 0
format K 7 0 0
format L 7 0 0
format M 7 0 0
define "page1" A0:H23
define "page2" A24:H43
define "page3" A44:H63
define "page4" A64:H83
define "page5" A84:H103
label A0 = 2
label B0 = 1
let C0 = A0+B0
fmt C0 "0.& \euro;-0.& \euro;--;@"
label D0 = A0&B0
let E0 = -1
leftstring A1 = "This is a brief sc tutorial, best run in a 24-line window."
leftstring A2 = "Type 'q' to exit, ^Z to suspend (w/ Job Control)."
leftstring A3 = "^G interrupts a command."
leftstring A5 = "abcdefghij"
leftstring B5 = "abcdefghij"
leftstring C5 = "abcdefghij"
leftstring D5 = "abcdefghij"
leftstring E5 = "abcdefghij"
leftstring F5 = "abcdefghij"
leftstring G5 = "abcdefghij"
leftstring H5 = "abcdefghij"
leftstring I5 = "abcdefghij"
leftstring J5 = "abcdefghij"
leftstring K5 = "abcdefghij"
leftstring L5 = "abcdefghij"
leftstring M5 = "abcdefghij"
leftstring N5 = "abcdefghij"
leftstring A8 = "Cells are named by their column and row number.  For example,"
leftstring G8 = @upper(H8)
rightstring H8 = "Date"
let I8 = @now
let J8 = @date(2021,8,24)+@time(1,23,45)
let K8 = @date(1904,1,1)+@time(0,0,0)
let L8 = @date(2000,3,1)
let M8 = @date(2001,3,1)
leftstring A9 = "Cell A6"
leftstring B9 = "Cell B6"
leftstring C9 = "Cell C6"
leftstring G9 = @lower(H9)
rightstring H9 = "Year"
let I9 = @year(I$8)
let J9 = @year(J$8)
let K9 = @year(K$8)
let L9 = @year(L$8)
let M9 = @year(M$8)
leftstring A10 = "Cell A7"
leftstring G10 = @proper(H10)
rightstring H10 = "Month"
let I10 = @month(I$8)
let J10 = @month(J$8)
let K10 = @month(K$8)
let L10 = @month(L$8)
let M10 = @month(M$8)
leftstring A11 = "Cell A8"
leftstring C11 = "Cell C8"
rightstring H11 = "Day"
let I11 = @day(I$8)
let J11 = @day(J$8)
let K11 = @day(K$8)
let L11 = @day(L$8)
let M11 = @day(M$8)
leftstring A12 = "Cells range from A0 to ZZ(some number depending on free memory)."
rightstring H12 = "Hour"
let I12 = @hour(I$8)
let J12 = @hour(J$8)
let K12 = @hour(K$8)
let L12 = @hour(L$8)
let M12 = @hour(M$8)
leftstring A13 = "Cells can also be named by the user.  See 'range names' in the manual."
rightstring H13 = "Minute"
let I13 = @minute(I$8)
let J13 = @minute(J$8)
let K13 = @minute(K$8)
let L13 = @minute(L$8)
let M13 = @minute(M$8)
leftstring A14 = "You can move the cursor a couple of different ways:"
rightstring H14 = "Second"
let I14 = @second(I$8)
let J14 = @second(J$8)
let K14 = @second(K$8)
let L14 = @second(L$8)
let M14 = @second(M$8)
leftstring B15 = "^n, j and the <DOWN> arrow key go down"
let I15 = @max(I9:I14)
let J15 = @min(J9:J14)
let K15 = @sum(K9:K14)
let L15 = @count(L9:L14)
let M15 = @count(M9:M14)
leftstring B16 = "^p, k and the <UP> arrow key go up"
let G16 = @sum(I8:I14!A10:K10)
leftstring I16 = @datefmt(I8)
leftstring B17 = "^b, h and the <LEFT> arrow key go left"
let G17 = @false()
leftstring I17 = @datefmt(I8,"%Y-%m-%d %H:%M:%S")
leftstring B18 = "^f, l and the <RIGHT> arrow key go right"
let I18 = 18835.8326388889
leftstring J18 = @datefmt(I18)
leftstring B19 = "You can go directly to a cell by typing 'g' and the cell name. "
let I19 = @days(J8,I8)
let J19 = @days360(I8,J8)
let K19 = @days360(J8,K8)
let L19 = @days360(K8,L8)
let M19 = @days360(L8,M8)
leftstring B20 = "'g c6' will take you to cell c6."
leftstring H20 = @ext("pwd")
leftstring A21 = "Cells can contain numbers, formulas, or text."
let F21 = @assert(@len("zzz"),3)
let I21 = @sum(I9:I14)
let J21 = @sum(J9:J14)
let K21 = @sum(K9:K14)
leftstring A22 = "Most of the cells on this page contain text."
let F22 = "a\\\"b\\\c"
let H22 = "<>"
let I22 = -2^2
leftstring C23 = "<Type 'g page2' to continue>"
let F23 = "a\"b\c"
let H23 = @countif(H8:H15,H22)
let I23 = 1
let J23 = 2
let K23 = 3
let F24 = "\x"
let H24 = toto(tutu)+@toto(@tutu)+@tutu
let I24 = 4
let J24 = 5
let K24 = 6
leftstring A25 = "Cell d22 contains text"
leftstring D25 = "Text "
let H25 = "test: "&@unichar(960)
let I25 = @sumproduct(I23:K23,I24:K24)
let J25 = @sumx2my2(I23:K23,I24:K24)
let K25 = @sumxmy2(I23:K23,I24:K24)
leftstring A26 = "Cell d23 contains a number"
let D26 = 123.34
let E26 = 1234.5
fmt E26 ",#.&"
let F26 = E26
fmt F26 ",#.&\"#\""
let G26 = @sum(D26:F26)
let I26 = @isbetween(J26,K26,L26)
let J26 = 1
let K26 = 0
let L26 = 3
leftstring A27 = "Cell d24 contains a formula"
let D27 = D26+88
let E27 = 12345678
fmt E27 ",#.&"
let F27 = E27
fmt F27 ",#.&"
let G27 = @sum(D27:F27)
let D28 = @sum(D26:D27)
let E28 = @sum(E26:E27)
let F28 = @sum(F26:F27)
let G28 = @sum(G26:G27)=@sum(D28:F28)
let H28 = @max(1,2,3,4,5,A1)
leftstring A29 = "To see what the cell contains, just move the cursor"
leftstring A30 = "onto the cell.  The contents will show up on line 1 in the brackets."
leftstring A32 = "You can enter data into cells like this:"
leftstring B33 = "'<text' enters left justified text."
leftstring B34 = "'>text' enters right justified text."
leftstring B35 = "'=number' enters a number"
leftstring B36 = "'=formula' enters a formula."
leftstring A38 = "Try duplicating d22 through d24 in e22 through e24."
leftstring A40 = "You erase a cell by typing 'x' with the cursor on the cell."
leftstring C43 = "<Type 'g page3' to continue>"
leftstring A45 = "Here is a typical use for numbers and formulas:"
let A47 = 10.3
let B47 = 1877.5
let C47 = 234.7
let E47 = @sum(A47:C47)
let A48 = 44.56
let B48 = 44.3
let C48 = -3
let E48 = @sum(A48:C48)
let A49 = 88.74
let B49 = 8000
let C49 = -9
let E49 = @sum(A49:C49)
let A50 = 99.2
let B50 = -88
let C50 = -44.6
let E50 = @sum(A50:C50)
let A52 = @sum(A47:A50)
let B52 = @sum(B47:B50)
let C52 = @sum(C47:C50)
let E52 = @sum(A47:C50)
leftstring A54 = "The data is entered in a44 through c47."
leftstring A55 = "Cells a49, b49, and c49, sum their respective columns."
leftstring A56 = "Cells e44, e45, e46, and e47 sum their respective rows."
leftstring A57 = "Cell E49 is a grand total."
leftstring A58 = "Try changing some of the data cells and watch the sums change."
leftstring A60 = "You can also edit cells by putting the cursor on the cell and typing:"
leftstring B61 = "'e' to edit the numeric portion."
leftstring B62 = "'E' to edit the string portion."
leftstring C63 = "<Type 'g page4' to continue>"
leftstring A65 = "Since you are reading this, you know that you can load "
leftstring A66 = "a data base from a file by typing the file name as an"
leftstring A67 = "argument to the program.  You can also load or save a "
leftstring A68 = "data base using the file commands:"
leftstring B70 = "'G file'"
leftstring C70 = "Gets the data from an sc file."
leftstring B71 = "'P file'"
leftstring C71 = "Puts the data from the spreadsheet into a file."
leftstring A73 = "Try 'P foo.sc' to write this to the file foo.sc"
leftstring A74 = "The Get command erases the current spreadsheet.  "
leftstring A75 = "To merge a spreadsheet with the one currently in"
leftstring A76 = "the machine, use:"
leftstring B78 = "'M file'"
leftstring C78 = "Merge the data from a saved sc file."
leftstring A80 = "You can also get human readable versions of the data"
leftstring A81 = "by using the Write command:"
leftstring C83 = "<Type 'g page5' to continue>"
leftstring A85 = "Try 'W tut.txt' for a clear text version of the tutorial."
leftstring A88 = "This is the end of the tutorial.  We have explored"
leftstring A89 = "The basic commands.  Much more detail is available"
leftstring A90 = "in the man page."
leftstring A92 = "To quit this program, type 'q'."
leftstring D94 = "GOOD LUCK!"
addnote F21 "this cell has a note"
goto F21 A0
