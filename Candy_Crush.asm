; ------- Muhammad Usman Shahid 20i-1797 ------------
; ----------- Musaab Imraan 2Oi-1794 ----------------
; ----------------------- T -------------------------
;--------------------- Project ----------------------

; ::::::::::::::::::::::::::::::::::::::::::::
; :::::::::::::::::: Macros ::::::::::::::::::
; ::::::::::::::::::::::::::::::::::::::::::::

;for setting up the background
setBackground MACRO num
    
    PUSHA 

    MOV AH, 06 ;scroll up window mode
    MOV AL, 0   ;Lines to scroll up

    MOV BH, num ;passing the number entered

    MOV CH,0 ;upper row no
    MOV CL,0 ;left col. no

    MOV DH,255 ;lower row no
    MOV DL,255 ;right col. no

    INT 10H

    POPA

ENDM

;for printing any given string
printString MACRO toPrint

    ;pushing all general purpose
    PUSHA
    
    MOV DX, 0

    ;displaying the string entered
    MOV DX, OFFSET toPrint
    MOV AH, 09
    INT 21H

    POPA ;poping them all

ENDM

;for going to next line
nextLine MACRO
    
    PUSHA

    ;for moving to next line
    MOV DL, 10
    MOV AH, 02
    INT 21H

    MOV DL, 13
    MOV AH, 02
    INT 21H

    POPA

ENDM

;for setting the cursor
setCursor MACRO height , width, pageNo
    
    PUSHA 

    ;setting cursor 
    MOV AH, 02
    MOV BH, pageNo ;pageNo
    MOV DH, height ;how much down
    MOV DL, width  ;how much spaces
    INT 10H

    POPA

ENDM

;for inputting in the given string
inputName MACRO nameStorage

   MOV SI, OFFSET nameStorage

input:

    MOV AH, 01
    INT 21H

    ;if entered stop input
    CMP AL, 13
    JE endInput ;end the input

    MOV AH, 0

    ;else store and take again
    MOV [SI] , AX ;assigning the value
    INC SI ;going to next index

    JMP input


endInput:

ENDM

;for going to next page 
switchPage MACRO pgNo

    ;pushing all general purpose registers
    PUSHA

    MOV AH, 05H
    MOV AL, pgNo
    INT 10H
    
    ;poping all of the general purpose registers pushed
    POPA

ENDM

;for drawing pixel
drawPixel MACRO xPos , yPos, pageNo, color

    ;pushing all general purpose registers
    PUSHA

   ;for writing pixel
    MOV AH, 0CH
    MOV AL, color ;color of pixel

    MOV BH, pageNo ;page number
    
    MOV CX, xPos ;x-axis
    MOV DX, yPos ;y-axis
    INT 10H

    ;poping all of the general purpose registers pushed
    POPA
    
ENDM

;for making square candy will be acting as rectangle by manipulating the x and y positions from calling
;thus for both rectangle and square
makeSquareRecCandy  MACRO startX , endX , startY , endY , color , pageNo

    ;pushing all general purpose registers
    PUSHA

    PUSH var1
    PUSH var2
    PUSH var3
    PUSH var4

    ;moving value of startingX in var1 so to avoid errors 
    MOV var1, startX
   
    MOV VAR2, startY  ;moving value of starting y in var1 so to avoid errors
 MOV CX, var2 ;value was also saved for later use

    ;loop
    .WHILE ( var1 <= endX  )

         MOV var2, CX

         ;draw pixel --- the left line
         ;only first time otherwise will have right of previous
         
        drawPixel var1, var2 , pageNo , color
        
        
        .WHILE ( var2 <= endY )

            ;the first line and last printing only horizontal
            ;if the first then print and if last then print
            ;.if( var2 == startY || var2 == endY)
            
                 ;draw pixel --- the left line
                drawPixel var1, var2 , pageNo , color

            ;.ENDIF

            INC var2

        .ENDW

        ;draw pixel ---- the right line
        drawPixel var1, var2 , pageNo , color
        
        INC var1
    .ENDW

    POP var4
    POP var3
    POP Var2
    POP var1
    ;poping all of the general purpose registers pushed
    POPA

ENDM

;for drawing the triangle
drawTriangle MACRO startX , endX , startY , endY , color , pageNo
    

    ;pushing all general purpose registers
    PUSHA
    PUSH var1
    PUSH var2
    PUSH var3
    PUSH var4

    ;the starting x
    MOV var1, startX
    
    ;moving this to middle
    ADD var1, 10

    ;the strating of y index
    MOV var2, startY
    ADD var2, 5 ;for bringing down

    ;having the dimensions of ending 
    MOV var3, endX
    MOV var4, endY

    ;also seeting the ending
    SUB var3, 10 ;will be expanded iteratively
    SUB var4, 5  ;giving padding from bottom
  
    ;counter
    MOV DX ,0

    ;will do for 8 times that makes a perfect triangle for 20x20
    .WHILE ( DX < 8 )

        drawPixel var1, var2, pageNo , color ;the left side
        drawPixel var3, var2, pageNo , color  ;the right side

        MOV AX, var1
        MOV BX, Var2
        MOV CX, var3

        ;svaing the values on stack
        PUSH var1
        PUSH var3

        ;to fill the pixels between the two extremes
        .WHILE (var1 < cx)

            drawPixel var1, var2, pageNo , color
            INC var1

        .ENDW

        ;poping the values from stack
        POP var3
        POP var1
        
        ;going back from var1 and going downward
        INC var2 ;increasing height
        INC var3 ;increasing from right side
        DEC var1 ;decreasing from leftside

        INC DX ;for counting

    .ENDW

    POP var4
    POP var3
    POP var2
    POP var1
    ;poping all of the general purpose registers pushed
    POPA



ENDM

;the fliped triangle
flipTriangle MACRO startX , endX , startY , endY , color , pageNo

    ;pushing all general purpose registers
    PUSHA

    PUSH var1
    PUSH var2
    PUSH var3
    PUSH var4

    ;the starting x
    MOV var1, startX
    
    ;moving this to indent
    ADD var1, 3

    ;the strating of y index
    MOV var2, startY
    ADD var2, 8 ;for bringing down

    ;having the dimensions of ending 
    MOV var3, endX
    MOV var4, endY

    ;also seeting the ending
    SUB var3, 3 ;will be contracted iteratively
    SUB var4, 5  ;giving padding from bottom
  
    ;counter
    MOV DX ,0

    ;will do for 8 times that makes a perfect triangle for 20x20
    .WHILE ( DX < 8 )

        drawPixel var1, var2, pageNo , color ;the left side
        drawPixel var3, var2, pageNo , color  ;the right side

        MOV AX, var1
        MOV BX, Var2
        MOV CX, var3

        ;svaing the values on stack
        PUSH var1
        PUSH var3

        ;to fill the pixels between the two extremes
        .WHILE (var1 < cx)

            drawPixel var1, var2, pageNo , color
            INC var1

        .ENDW

        ;poping the values from stack
        POP var3
        POP var1
        
        ;going back from var1 and going downward
        INC var2 ;increasing height
        DEC var3 ;decreasing from right side
        INC var1 ;increasing from leftside

        INC DX ;for counting

    .ENDW

    POP var4
    POP var3
    POP var2
    POP var1

    ;poping all of the general purpose registers pushed
    POPA

ENDM 

;formaking star
drawStar MACRO startX , endX , startY , endY , color , pageNo

    
    ;drawing the triangle
    drawTriangle  startX , endX , startY , endY , color , pageNo

    ;now drawing another fliped triangle on same level to make star
    flipTriangle startX , endX , startY , endY , color , pageNo

    
ENDM

;for making diamond --- rhombus
drawRohmbus MACRO startX , endX , startY , endY , color , pageNo

    ;pushing all general purpose registers
    PUSHA

    PUSH var1
    PUSH var2
    PUSH var3
    PUSH var4
    

    
    ;push the real y starting increment 3 move down 3 and print
    ;MOV BX, startY
    
    POP var4
    POP var3
    POP var2
    POP var1

    ;poping all of the general purpose registers pushed
    POPA

ENDM


;for making borders -- helping in showing geid
drawBorder MACRO startX , endX , startY , endY , color , pageNo

    ;pushing all general purpose registers
    PUSHA

    ;moving value of startingX in var1 so to avoid errors 
    MOV var1, startX
    MOV CX, var1 ;value was also saved for later use

    MOV VAR2, startY  ;moving value of starting y in var1 so to avoid errors

    ;loop
    .WHILE ( var2 <= endY )

         MOV var1, CX

         ;draw pixel --- the left line
         ;only first time otherwise will have right of previous
         
        drawPixel var1, var2 , pageNo , color
        
        
        .WHILE ( var1 < endX )

            ;the first line and last printing only horizontal
            ;if the first then print and if last then print
            .if( var2 == startY || var2 == endY)
            
                 ;draw pixel --- the left line
                drawPixel var1, var2 , pageNo , color

            .ENDIF

            INC var1

        .ENDW

        ;draw pixel ---- the right line
        drawPixel var1, var2 , pageNo , color
        
        INC var2
    .ENDW

    
    ;poping all of the general purpose registers pushed
    POPA

    
ENDM

;drawing the grid
drawGrid MACRO color, pageNo
    
    ;our dimensions are 320x200
    ;we have to make 49 boxes as 7x7 each of 20x20
    ;mean whole board will be of 140x140
    ;as intending from above side of 40 thus 180
    ;also giving from right of 60  making 200
    ;thus the board will be displaying on 200x180
    ; -------------------------------------------------------

    ;pushing all general purpose registers
    PUSHA

    ;count of rows
    MOV AX, 40
    MOV BX, 60

    ;count of columns
    MOV CX, 60
    MOV DX, 80

    ; will visit the ---- 7 rows
    .WHILE(AX<180)

        MOV CX, 60
        MOV DX, 80

      
        ;will print the borders of columns ... visit 7 coloumns
        .WHILE(CX<200)
            
        
            drawBorder CX, DX, AX, BX, color, 2
            ADD CX, 20
            ADD DX, 20

        .ENDW

        ADD AX, 20
        ADD BX, 20

    .ENDW

    
    ;poping all of the general purpose registers pushed
    POPA
    
ENDM

;draws the whole board with grids
drawBoard MACRO color, pageNo, dummyArray, originalArray

    PUSHA
    ;for drawind boder main
    drawBorder 0, 319, 0, 199,  04h, 02
    drawBorder 1, 318, 1, 198,  04h, 02
    drawBorder 2, 317, 2, 197,  04h, 02
    drawBorder 3, 316, 3, 196,  04h, 02


    ;drawing the grid
    drawGrid color, page

    makeCandies originalArray

    
    ;the score side with level
    drawBorder 210, 310, 60, 160, 04h, 02
    drawBorder 211, 309, 61, 159, 04h, 02

    nextLine
    
    ;setting the cursor
    setCursor 10 ,30,0

    ;printing player name
    printString playerN

    ;setting the cursor
    setCursor 12 ,28,0
    
    printString levelMsg

    MOV DX, Level
    MOV AH, 02
    INT 21H

    nextLine

    ;setting the cursor
    setCursor 14 ,27,0
    
    printString scoreMsg
    MOV AX, score
    displayMultidigit AX ;for displaying multi digits


    ;setting the cursor
    setCursor 16 ,27,0
    
    printString moveMsg
    MOV AX , Moves
    displayMultidigit AX ;for displaying multi digits

    POPA

ENDM

;for drawing the candies
makeCandies MACRO original

    PUSHA 

    ;setting registers to 0
    MOV AX, 0
    MOV BX, 0
    MOV CX, 0

    ;the address of the array
    MOV SI, OFFSET original

    ;setting the starting dimensions and will be row so in crease in that mannwe
    MOV var1, 60 ;the starting of first elemnt -- x
    MOV Var2, 80 ;the ending of first --- x

    MOV var3, 40 ;the starting of first elemnt -- y
    MOV Var4, 60 ;the ending of first --- y

    ;nesteed while loop to access all 49
    .WHILE (CX <7)

        MOV BX, 0 ;refereshing 
        MOV var3, 40 ;the starting of first elemnt -- x
        MOV Var4, 60 ;the ending of first --- x

        .WHILE (BX < 7)
        

            MOV AX, [SI]

            .IF ( AX == 0) ;the star candy

            PUSHA
                
                ;to make in the dimensions
                MOV AX, var1
                MOV BX, Var2
                MOV CX, var3
                MOV DX, Var4   


                drawStar AX , BX, CX, DX , 01h, 02

               

                POPA

            .ELSEIF ( AX == 1 ) ;squre
                
                PUSHA
                
                ;to make in the dimensions
                MOV AX, var1
                MOV BX, Var2
                MOV CX, var3
                MOV DX, Var4   

                ;for centering the square  
                ADD AX, 5
                SUB BX, 5
                ADD CX, 5
                SUB DX, 5


                makeSquareRecCandy AX , BX, CX, DX , 02h, 02

               

                POPA
                 
            .ELSEIF ( AX == 2 ) ;rectangle

            
                PUSHA
                
                ;to make in the dimensions
                MOV AX, var1
                MOV BX, Var2
                MOV CX, var3
                MOV DX, Var4   

                ;for making rectangle 
                ADD AX, 9
                SUB BX, 9
                ADD CX, 5
                SUB DX, 5


                makeSquareRecCandy AX , BX, CX, DX , 06h, 02

               

                POPA
                

            .ELSEIF  ( AX == 3 ) ;+ candy

                PUSHA
                
                ;to make in the dimensions
                MOV AX, var1
                MOV BX, Var2
                MOV CX, var3
                MOV DX, Var4   

                ;for making rectangle 
                ADD AX, 9
                SUB BX, 9
                ADD CX, 5
                SUB DX, 5


                makeSquareRecCandy AX , BX, CX, DX , 05h, 02

                ;to make in the dimensions
                MOV AX, var1
                MOV BX, Var2
                MOV CX, var3
                MOV DX, Var4   

                ;for making rectangle 
                ADD AX, 5
                SUB BX, 5
                ADD CX, 9
                SUB DX, 9

                makeSquareRecCandy AX , BX, CX, DX , 05h, 02


                POPA

            .ELSEIF  ( AX == 4 ) ;the triangle

             PUSHA
                
                ;to make in the dimensions
                MOV AX, var1
                MOV BX, Var2
                MOV CX, var3
                MOV DX, Var4   

                
                drawTriangle AX , BX, CX, DX , 03h, 02


                POPA

            .ELSEIF  ( AX == 5 ) ;the rhombus

            PUSHA
                
                ;to make in the dimensions
                MOV AX, var1
                MOV BX, Var2
                MOV CX, var3
                MOV DX, Var4   

                  ;drawing rhombus
  
                ;moving a little but up
                sub Cx, 2

                ;drawing the triangle
                drawTriangle AX , BX , CX , DX , 0cH , 02

                MOV CX, var3
                ADD CX, 3

                ;drawing the fliped one
                flipTriangle AX , BX , CX , DX , 04H , 02



                POPA
            .ELSEIF  ( AX == 6 ) ;the color bomb

                PUSHA

                ;to make in the dimensions
                MOV AX, var1
                MOV BX, Var2
                MOV CX, var3
                MOV DX, Var4   

                ;drawing rhombus
  
                ;moving a little but up
                sub Cx, 2

                ;drawing the triangle
                drawTriangle AX , BX , CX , DX , 0cH , 02

                MOV CX, var3
                ADD CX, 3

                ;drawing the fliped one
                flipTriangle AX , BX , CX , DX , 0CH , 02

                ;--------------------------------------------------------------------
                ; SECOND LAYER ---------------------------------------------------------
                ;----------------------------------------------------------------

                ;to make in the dimensions
                MOV AX, var1
                MOV BX, Var2
                MOV CX, var3
                MOV DX, Var4   

                ;drawing rhombus
  
                ;moving a little but up
                sub Cx, 0

                ;drawing the triangle
                drawTriangle AX , BX , CX , DX , 0BH , 02

                MOV CX, var3
                ADD CX, 1

                ;drawing the fliped one
                flipTriangle AX , BX , CX , DX , 0BH , 02


                ;--------------------------------------------------------------------
                ; thirrd LAYER ---------------------------------------------------------
                ;----------------------------------------------------------------

                ;to make in the dimensions
                MOV AX, var1
                MOV BX, Var2
                MOV CX, var3
                MOV DX, Var4   

                ;drawing rhombus
  
                ;moving a little but up
                ADD Cx, 2

                ;drawing the triangle
                drawTriangle AX , BX , CX , DX , 0DH , 02

                MOV CX, var3
               sub CX, 1

                ;drawing the fliped one
                flipTriangle AX , BX , CX , DX , 0DH , 02

                ;--------------------------------------------------------------------
            
                POPA
            .ELSEIF (AX == -1 ) ;the blockages

             PUSHA
                
                ;to make in the dimensions
                MOV AX, var1
                MOV BX, Var2
                MOV CX, var3
                MOV DX, Var4   

                sub dx, 1


                makeSquareRecCandy AX , BX, CX, DX , 07h, 02

               

            POPA

            .ENDIF
            
            ;accessing next elements
            ADD SI, 2 ;word size
            INC BX

            ADD var3, 20
            ADD var4, 20


        .ENDW
 

            ;accessing the next level
            ADD var1, 20
            ADD var2, 20 

        
        INC CX

    .ENDW

    POPA
    
ENDM

;for populating arrays at back end with candy id's
populateMe MACRO dummyArray, originalArray

    ;pushing all general purpose registers
    PUSHA

    PUSH var1
    PUSH var2
    PUSH var3
    PUSH var4

    ;The counter variables
    MOV var1, 0
    MOV var2, 0

    ;storing the addresses of arrays
    MOV SI, OFFSET originalArray
    MOV DI, OFFSET dummyArray

    ;moving 7x7 loop so to access each element
    .WHILE ( var1 < 14 )

        MOV var2, 0 ;refereshing bx

        ;Nested while
        .WHILE ( var2 < 14 )   

            ;a macro that will create delay with the help of nested loops so to have different time each time
            delay

            MOV AH, 00h  ; for getting the system time       
            INT 1AH      ; the time will be stored in cx and dx     


            mov  AX, DX 
            mov  DX, 0   ;clearing the dx to hold the remainder address
            mov  CX, 6   ;will take the mod like to get candies id as we have id's from 0 to 5
            div  CX      

            ;moving value to arrays 
            MOV WORD PTR [SI], DX
            MOV WORD PTR [DI], DX    

            ;going to next indexes
            ADD SI, 2
            ADD DI, 2

            ;for loop
            ADD var2, 2 

        .ENDW

        
        ADD var1, 2 ;the array is of word size 

    .ENDW

    POP var4
    POP var3
    POP var2
    POP var1

    ;poping all of the general purpose registers pushed
    POPA
    
ENDM


;for creating delay will help in randomizing
delay MACRO 
    
    ;pushing the all general purpose registers
    PUSHA

    ;moving the highest range and looping that
    MOV CX, 16383

    ;loop 1
    L1:

    ;saving the previous value
    PUSH CX

        ;for the nested loop moving 3 
        MOV CX, 7

        ;looping it 
        L2:

            ;will help to create delay so the time getting will be some change 
            ;and will help in random number generation
        LOOP L2
    
    ;getting the previous value
    POP CX

    LOOP L1

    
    ;poping the all general purpose registers
    POPA
    
ENDM

;for showing mouse on screen
showMouse MACRO xPos, yPos
    
    ;pushing the all general purpose registers
    PUSHA
    
    ;setting the display of mouse
    MOV AX, 1
    INT 33H

    ;now setting mouse position
    MOV AX, 4
    MOV CX, xPos ;X-axis
    MOV DX, yPos ;Y-axis

    INT 33H

    ;poping the all general purpose registers
    POPA


ENDM 

;for restricting the mouse
restrictMouse MACRO minX, maxX, minY, maxY
    
    ;pushing the all general purpose registers
    PUSHA

    ;for horizontal restriction 
    MOV AX, 7
    MOV CX, minX
    MOV DX, maxX
    Int 33h

    ;for vertical restriction
    MOV AX, 8
    MOV CX, minY
    MOV DX, maxY
    Int 33h

    ;poping the all general purpose registers
    POPA

ENDM

;for checking the co ordinates of mouse click
mouseClick MACRO x, y   ;will save cordinates of x and y in passsed variables
    
    ;pushing the all general purpose registers
    PUSHA

    ;initallay nothing
    MOV BL, 0

    .WHILE ( BL != 1 ) ;the loop checks when to break when left mouse

        ;for getting mouse value
        MOV AX, 3
        INT 33H

        ;if left click
        .IF ( BL == 1 )

            ;saving the x and y cordinate
            ;--- X and Y co ordinate
            MOV x, CX
            MOV y, DX
            

        .ENDIF

    .ENDW

    ;poping the all general purpose registers
    POPA


ENDM

;for checking the real co-ordinates the working
mapRealCod MACRO x, y , index ;will update  the reveersed variables that will contain the co ordinates also index

    ;pushing the all general purpose registers
    PUSHA
    ;local variables
    PUSH temp ;used for mapping correct vlue
    PUSH var1 ;the counting 

    MOV var1 , 0
    
    MOV AX, x
    SHR AX, 1 ;dividing with two
    MOV temp , AX  ;saving the value

    ;count of rows
    MOV AX, 40
    MOV BX, 60

    ;count of columns
    MOV CX, 60
    MOV DX, 80

    ; will visit the ---- 7 rows
    .WHILE(CX<200 && index == -1)

        MOV AX, 40
        MOV BX, 60
        
      
        ;will print the borders of columns ... visit 7 coloumns
        .WHILE(AX<180 && index == -1)
            
        
            ;now checking which co-ordinate
            .IF (temp > CX && temp < DX && y > AX && y < BX)

                

                ;the co ordinate values aslo saving
                mov xStart , CX
                mov xEnd   , DX
                mov yStart , AX
                mov yEnd   , BX

                
                ;saving values
                PUSHA

                    ;also saving the index
                    MOV AX, var1
                    MOV index, AX

                    ;reset mouse
                    MOV AX, 0
                    INT 33H 

                POPA ;getting back

                ;drawing boder against selected
                drawBorder CX, DX, AX, BX, 04h, 02


            .ENDIF 

            ADD AX, 20
            ADD BX, 20

            ;count var1
            INC var1

        .ENDW

        ADD CX, 20
        ADD DX, 20

        

    .ENDW

    ;restore old value
    POP var1
    POP temp

    ;poping the all general purpose registers
    POPA

ENDM 

;for swaping in the back end
swapUs MACRO element1, element2

    ;pushing the all general purpose registers
    PUSHA
    PUSH SI
    PUSH DI


    MOV SI, OFFSET candyBoard ;address of first element
    MOV DI, OFFSET candyBoard ;address of second element

    ;as word size adding two times
    ADD SI, element1 
    ADD SI, element1

    ;as word size adding two times
    ADD DI, element2
    ADD DI, element2

    ;swapping
    MOV AX, [SI]
    MOV DX, [DI]

    MOV [SI] ,DX
    MOV [DI] , AX 

    POP DI 
    POP SI      
    ;poping the all general purpose registers
    POPA
    
ENDM

;for checking whether i2 is in neighbours of i1 or not and will return decision in result parameter
checkNeighbourhood MACRO i1, i2, result

    PUSHA ;All general purpose registers saving

        
        
        MOV AX, i1 ;the index1
        SUB AX, 7  ;the left
        ;the conditions that satify neighbour hood
        .IF (AX == i2)     

            MOV result, 1 ;yes are neighbours
            JMP done ;don't check all if

        .ENDIF

        MOV AX, i1 ;the index1
        ADD AX, 7  ;the right

        .IF (AX == i2)     

            MOV result, 1 ;yes are neighbours
            JMP done ;don't check all if

        .ENDIF
        
        MOV AX, i1 ;the index
        SUB AX, 1  ;the upper

        .IF (AX == i2)     

            MOV result, 1 ;yes are neighbours
            JMP done ;don't check all if

        .ENDIF

        MOV AX, i1 ;index
        ADD AX, 1 ;the lower

        .IF (AX == i2)     

            MOV result, 1 ;yes are neighbours
            JMP done ;don't check all if

        .ENDIF

       MOV AX, i1 ;index
       SUB AX , 8 ;the left upper diagonal

         .IF (AX == i2)     

            MOV result, 1 ;yes are neighbours
            JMP done ;don't check all if

        .ENDIF

        MOV AX, i1
        SUB AX, 6 ;the left lower diagonal

        .IF (AX == i2)     

            MOV result, 1 ;yes are neighbours
            JMP done ;don't check all if

        .ENDIF

       MOV AX, i1
       ADD AX, 6 ;the right upper diagonal      

        .IF (AX == i2)     

            MOV result, 1 ;yes are neighbours
            JMP done ;don't check all if

        .ENDIF
           
      MOV AX, i1
      ADD AX, 8 ;the right bottom diagonal

        .IF (AX == i2)     

            MOV result, 1 ;yes are neighbours
            JMP done ;don't check all if

        .ENDIF

     
    MOV result, 0 ;no if visited

    done: ;the neigbour found 

    ;if the last row element selected
    .IF ( i1 == 6 || i1 == 13 || i1 == 20 || i1 == 27 || i1 == 34 || i1 == 41 || i1 == 48)

        ;and the swaping was from row 1
        .IF ( i2 == 0 || i2 == 7 || i2 == 14 || i2 == 21 || i2 == 28 || i2 == 35 || i2 == 42)

            ;then don't swap are not neighbours
            MOV result, 0

        .ENDIF

    .ENDIF


    ;if the first row element selected
    .IF ( i1 == 0 || i1 == 7 || i1 == 14 || i1 == 21 || i1 == 28 || i1 == 35 || i1 == 42 )
        ;and the swaping was from row 7
        .IF ( i2 == 6 || i2 == 13 || i2 == 20 || i2 == 27 || i2 == 34 || i2 == 41 || i2 == 48 )

            ;then don't swap are not neighbours
            MOV result, 0

        .ENDIF
        
    .ENDIF  

    POPA ;retreving the previous values



ENDM 

;for displaying scores and moves
displayMultidigit MACRO num

    ;pushing general purpose register
    PUSHA 
    PUSH temp
    PUSH decision

    MOV AX, num
    MOV decision, 0

    ;the first time the real result then each time updated version of that number
    MOV temp, AX
    
    .IF ( temp == 0 )

        MOV DX, 0
        ADD DX, 48
        MOV AH, 02
        INT 21H

    .ENDIF

    ;pushing that desired number to stack
    .While(temp != 0 )
    
        MOV DX, 0
        MOV AX, 0

        ;moving the desired display number to AX for division with 10 to push digit by digit in stack
        MOV AX, temp
        MOV BH, 0
        MOV BL, 10
        DIV BX

        ;if not mean digits are there
        PUSH DX

        ;keeping track how many are pushed in stack
        ADD decision, 1

        ;updating the value of desired number to display
        MOV temp, AX
        
    .ENDW


    ;for poping the data from stack
    .while(decision > 0 )

        POP DX
        ADD DX, 48 ;adjusting the ascii's
        MOV AH, 02
        INT 21H

        Sub decision, 1 ;decreasing that one digit is being poped

    .ENDW

    POP decision
    POP temp
    ;poping the registers
    POPA         

ENDM 

;for checking combinations and poping 
checkCombination MACRO i1, i2, result

    PUSHA
    PUSH decision
    
    MOV decision, 0
    MOV AX, decision

    MOV SI, OFFSET candyBoard
    ADD SI, i1
    ADD SI, i1

    MOV DI, OFFSET candyBoard
    ADD DI, i2
    ADD DI, i2

    ;getting values of index
    MOV AX, WORD PTR [SI]
    MOV BX, WORD PTR [DI]

    ;if swapped with bomb
    .IF( AX == 6 )

        ;bomb was swaped
        bomInitiate i1, BX, decision
        
    .ENDIF

    ;if swapped with bomb
    .IF( BX == 6 )

        ;candy was swaped with bomb
        bomInitiate i2, AX, decision
        
    .ENDIF

    ;checking for horizontal bomb
    makeBombHorizontal i1, decision
    makeBombHorizontal i2, decision

    ;checking for vertical bomb
    makeBombVertical i1, decision
    makeBombVertical i2, decision

    ;will check combination and will pop if combination made also add scores accordingly
    checkComboLeft i1, decision
    checkComboLeft i2, decision

    ;the right
    checkComboRight i1, decision
    checkComboRight i2, decision

    ;The top
    checkComboTop i1, decision
    checkComboTop i2, decision

    ;the bottom
    checkComboBottom i1, decision
    checkComboBottom i2, decision
    
    ;the between 3 combination
    checkcomboinbetweenHorizontal i1, decision
    checkcomboinbetweenHorizontal i2, decision

    checkcomboinbetweenVertically i1, decision
    checkcomboinbetweenVertically i2, decision

    ;returning poping was done or not
    MOV AX, decision
    MOV result, Ax

    POP decision
    POPA

ENDM 

;for checking combinations
checkComboLeft MACRO element, result

    PUSHA
    PUSH SI
    PUSH DI     

        MOV SI, OFFSET candyBoard
        ADD SI, element
        ADD SI, element
        MOV BX, WORD PTR [SI]

        ;initallay the result was 0 so change only if combination exist
        ;if first two columns return result 0 because not possible two that two 
        .IF ( element != 0 && element != 1 && element != 2 && element != 3 && element != 4 && element != 5 && element != 6 && element != 7 && element != 8 && element != 9 && element != 10 && element != 11 && element != 12 && element != 13  )

             
            MOV DI, SI
            SUB DI, 14 ;a previous than calling

            MOV DX, WORD PTR [DI]

            ;the more previous
            SUB DI, 14

            MOV CX, WORD PTR [DI]

            .IF ( BX == DX && CX == DX)

                ;also poping ;7 means no candy
                MOV WORD PTR [DI] , 7
                MOV WORD PTR [DI + 14], 7
                MOV WORD PTR [DI + 28], 7

                MOV result, 1
                ADD score, 3

            .ENDIF
             

        .ENDIF

    POP DI   
    POP SI      
    POPA

ENDM

;for checking combinations
checkComboRight MACRO element, result

    PUSHA
    PUSH SI
    PUSH DI     

        MOV SI, OFFSET candyBoard
        ADD SI, element
        ADD SI, element
        MOV BX, WORD PTR [SI]

        ;initallay the result was 0 so change only if combination exist
        ;if last two columns return result 0 because not possible two that two 
        .IF ( element != 35 && element != 36 && element != 37 && element != 38 && element != 39 && element != 40 && element != 41 && element != 42 && element != 43 && element != 44 && element != 45 && element != 46 && element != 47 && element != 48  )

             
            MOV DI, SI
            ADD DI, 14 ;a next than calling

            MOV DX, WORD PTR [DI]

            ;the more next
            ADD DI, 14

            MOV CX, WORD PTR [DI]

            .IF ( BX == DX && CX == DX)

                ;also poping ;7 means no candy
                MOV WORD PTR [DI] , 7
                MOV WORD PTR [DI - 14], 7
                MOV WORD PTR [DI - 28], 7

                MOV result, 1
                ADD score, 3

            .ENDIF
             

        .ENDIF

    POP DI   
    POP SI      
    POPA

ENDM

;for checking combinations
checkComboTop MACRO element, result

    PUSHA
    PUSH SI
    PUSH DI     

        MOV SI, OFFSET candyBoard
        ADD SI, element
        ADD SI, element
        MOV BX, WORD PTR [SI]

        ;initallay the result was 0 so change only if combination exist
        ;if first two rows return result 0 because not possible two that two 
        .IF ( element != 0 && element != 7 && element != 14 && element != 21 && element != 28 && element != 35 && element != 42 && element != 1 && element != 8 && element != 15 && element != 22 && element != 29 && element != 36 && element != 43  )

             
            MOV DI, SI
            SUB DI, 2 ;a top than calling

            MOV DX, WORD PTR [DI]

            ;the more upper
            SUB DI, 2

            MOV CX, WORD PTR [DI]

            .IF ( BX == DX && CX == DX)

                ;also poping ;7 means no candy
                MOV WORD PTR [DI] , 7
                MOV WORD PTR [DI + 2], 7
                MOV WORD PTR [DI + 4], 7

                MOV result, 1
                ADD score, 3

            .ENDIF
             

        .ENDIF

    POP DI   
    POP SI      
    POPA

ENDM

;for checking combinations
checkComboBottom MACRO element, result

    PUSHA
    PUSH SI
    PUSH DI     

        MOV SI, OFFSET candyBoard
        ADD SI, element
        ADD SI, element
        MOV BX, WORD PTR [SI]

        ;initallay the result was 0 so change only if combination exist
        ;if last two rows return result 0 because not possible two that two 
        .IF ( element != 5 && element != 12 && element != 19 && element != 26 && element != 33 && element != 40 && element != 47 && element != 6 && element != 13 && element != 20 && element != 27 && element != 34 && element != 41 && element != 48  )

             
            MOV DI, SI
            ADD DI, 2 ;a bottom than calling

            MOV DX, WORD PTR [DI]

            ;the more bottom one
            ADD DI, 2

            MOV CX, WORD PTR [DI]

            .IF ( BX == DX && CX == DX)

                ;also poping ;7 means no candy
                MOV WORD PTR [DI] , 7
                MOV WORD PTR [DI - 2], 7
                MOV WORD PTR [DI - 4], 7

                MOV result, 1
                ADD score, 3

            .ENDIF
             

        .ENDIF

    POP DI   
    POP SI      
    POPA

ENDM

;the 5 combinations in row
makeBombHorizontal MACRO element, result

    PUSHA
    PUSH SI
    PUSH DI     

    MOV AX, 0 ;the result

    ;cckeing left combo first from given candy
    MOV SI, OFFSET candyBoard
    ADD SI, element
    ADD SI, element
    MOV BX, WORD PTR [SI]

    ;initallay the result was 0 so change only if combination exist
    ;if first two columns return result 0 because not possible two that two 
    .IF ( element != 0 && element != 1 && element != 2 && element != 3 && element != 4 && element != 5 && element != 6 && element != 7 && element != 8 && element != 9 && element != 10 && element != 11 && element != 12 && element != 13  )

            
        MOV DI, SI
        SUB DI, 14 ;a previous than calling

        MOV DX, WORD PTR [DI]

        ;the more previous
        SUB DI, 14

        MOV CX, WORD PTR [DI]

        .IF ( BX == DX && CX == DX)

            MOV AX, 1 ;the left combo is there

        .ENDIF
            

    .ENDIF

    ;if left combo is there than finding for the right combo
    .IF ( AX == 1 )

    MOV SI, OFFSET candyBoard

        ADD SI, element
        ADD SI, element
        MOV BX, WORD PTR [SI]

        ;initallay the result was 0 so change only if combination exist
        ;if last two columns return result 0 because not possible two that two 
        .IF ( element != 35 && element != 36 && element != 37 && element != 38 && element != 39 && element != 40 && element != 41 && element != 42 && element != 43 && element != 44 && element != 45 && element != 46 && element != 47 && element != 48  )

             
            MOV DI, SI
            ADD DI, 14 ;a next than calling

            MOV DX, WORD PTR [DI]

            ;the more next
            ADD DI, 14

            MOV CX, WORD PTR [DI]

            .IF ( BX == DX && CX == DX)

                ;if it also exist start poping right and left
                ;also poping ;7 means no candy
                MOV WORD PTR [DI] , 7
                MOV WORD PTR [DI - 14], 7
                MOV WORD PTR [DI - 28], 7
                
                ;now poping left side
                MOV DI, SI

                MOV WORD PTR [DI - 14], 7
                MOV WORD PTR [DI - 28], 7

                MOV WORD PTR [SI] , 6 ;the bomb candy

                MOV result, 1
                ADD score, 5

            .ENDIF
             

        .ENDIF


    .ENDIF

    POP DI   
    POP SI      

    POPA

ENDM

;the 5 commbination in column
makeBombVertical MACRO element, result

    PUSHA
    PUSH SI
    PUSH DI     

    MOV AX, 0 ;the result

    ;cckeing top combo first from given candy
    MOV SI, OFFSET candyBoard
    ADD SI, element
    ADD SI, element
    MOV BX, WORD PTR [SI]

    ;initallay the result was 0 so change only if combination exist
    ;if first two rows return result 0 because not possible two that two 
    .IF ( element != 0 && element != 7 && element != 14 && element != 21 && element != 28 && element != 35 && element != 42 && element != 1 && element != 8 && element != 15 && element != 22 && element != 29 && element != 36 && element != 43  )

            
        MOV DI, SI
        SUB DI, 2 ;a previous than calling

        MOV DX, WORD PTR [DI]

        ;the more previous
        SUB DI, 2

        MOV CX, WORD PTR [DI]

        .IF ( BX == DX && CX == DX)

            MOV AX, 1 ;the left combo is there

        .ENDIF
            

    .ENDIF

    ;if left combo is there than finding for the bottom combo
    .IF ( AX == 1 )

    MOV SI, OFFSET candyBoard

        ADD SI, element
        ADD SI, element
        MOV BX, WORD PTR [SI]

        ;initallay the result was 0 so change only if combination exist
        ;if last two rows return result 0 because not possible two that two 
        .IF ( element != 5 && element != 12 && element != 19 && element != 26 && element != 33 && element != 40 && element != 47 && element != 6 && element != 13 && element != 20 && element != 27 && element != 34 && element != 41 && element != 48  )

             
            MOV DI, SI
            ADD DI, 2 ;a next than calling

            MOV DX, WORD PTR [DI]

            ;the more next
            ADD DI, 2

            MOV CX, WORD PTR [DI]

            .IF ( BX == DX && CX == DX)

                ;if it also exist start poping right and left
                ;also poping ;7 means no candy
                MOV WORD PTR [DI] , 7
                MOV WORD PTR [DI - 2], 7
                MOV WORD PTR [DI - 4], 7
                
                ;now poping left side
                MOV DI, SI

                MOV WORD PTR [DI - 2], 7
                MOV WORD PTR [DI - 4], 7

                MOV WORD PTR [SI] , 6 ;the bomb cnady

                MOV result, 1
                ADD score, 5
            .ENDIF
             

        .ENDIF


    .ENDIF

    POP DI   
    POP SI      

    POPA

ENDM

;in between combination horizontallay
checkcomboinbetweenHorizontal MACRO element, result

    PUSHA
    PUSH SI 
    PUSH DI 

    ;not possible in last column or first column
    .IF ( element != 0 && element != 1 && element != 2 && element != 3 && element != 4 && element != 5 && element != 6 && element != 42 && element != 43 && element != 44 && element != 45 && element != 46 && element != 47 && element != 48 )
        
        ;going to desired point
        MOV SI, OFFSET candyBoard
        ADD SI, element
        ADD SI, element

        MOV AX, WORD PTR [SI]     ;element itself
        MOV BX, WORD PTR [SI - 14] ;left
        MOV CX, WORD PTR [SI + 14] ;right

        .IF ( AX == BX && AX == CX )
            
            MOV result, 1 ;the combo exist

            ;poing
            MOV WORD PTR [SI] , 7
            MOV WORD PTR [SI - 14], 7
            MOV WORD PTR [SI + 14], 7
            
            ADD score, 3

        .ENDIF
    
    .ENDIF 

    POP DI     
    POP SI     
    POPA         

ENDM

;in between combination vertically
checkcomboinbetweenVertically MACRO element, result

    PUSHA
    PUSH SI 
    PUSH DI 

    ;not possible in last row or first row
    .IF ( element != 0 && element != 7 && element != 14 && element != 21 && element != 28 && element != 35 && element != 42 && element != 6 && element != 13 && element != 20 && element != 27 && element != 34 && element != 41 && element != 48 )
        
        ;going to desired point
        MOV SI, OFFSET candyBoard
        ADD SI, element
        ADD SI, element

        MOV AX, WORD PTR [SI]     ;element itself
        MOV BX, WORD PTR [SI - 2] ;top
        MOV CX, WORD PTR [SI + 2] ;bottom

        .IF ( AX == BX && AX == CX )
            
            MOV result, 1 ;the combo exist

            ;poing
            MOV WORD PTR [SI] , 7
            MOV WORD PTR [SI - 2], 7
            MOV WORD PTR [SI + 2], 7
            
            ADD score, 3

        .ENDIF
    
    .ENDIF 
    
    POP DI     
    POP SI     
    POPA         

ENDM

;the bomb when swaped
bomInitiate MACRO element, value, result

    ;will traverse in whole and where 7 caught the upper candy will be bringed down
    ;pushing all general purpose registers
    PUSHA

    ;storing the addresses of arrays
    MOV SI, OFFSET candyBoard
    ADD SI, element
    ADD SI, element
    MOV WORD PTR [SI], 7

    MOV DI, OFFSET candyBoard
    MOV AX, 0

    .WHILE(AX<49)
        MOV WORD PTR [DI], 0
        ADD DI, 2
        INC AX
    .ENDW
    
    POPA

ENDM

;for updating and bringing candies down 
updateBoard  MACRO return

    ;will traverse in whole and where 7 caught the upper candy will be bringed down
    ;pushing all general purpose registers
    PUSHA

    PUSH var1
    PUSH var2
    PUSH var3
    PUSH var4

    ;The counter variables
    MOV var1, 0
    MOV var2, 0

    ;storing the addresses of arrays
    MOV SI, OFFSET candyBoard

    MOV CX, 98 ;the last index

    ;moving 7x7 loop so to access each element
    .WHILE ( var1 < 14 )


        MOV var2, 0 ;refereshing bx
        MOV SI, OFFSET candyBoard
        ADD SI, CX

        ;Nested while
        .WHILE ( var2 < 14 )   

          
            MOV AX, WORD PTR [SI]

            ;if empty then bring upper
            .IF( AX == 7 )

                MOV return, 1
            
                MOV DI, SI
                SUB DI, 2

                ;taking upper candy and making upper box empty
                MOV BX, WORD PTR [DI]
                MOV WORD PTR [SI], BX
                MOV WORD PTR [DI], 7
                
            .ENDIF

            ;going to next indexes
            SUB SI, 14
           

            ;for loop
            ADD var2, 2 

        .ENDW

        
        SUB CX, 2
        ADD var1, 2 ;the array is of word size 

    .ENDW

    
    ;traversing for whole first row to enter random values
    MOV SI, OFFSET candyBoard
    
    MOV AX, 0
    MOV BX, 0
    MOV DX, 0

    .WHILE ( AX < 7) 
        
        MOV BX, WORD PTR [SI] ;si data
        
            ;if empty creating random candy
            .IF ( BX == 7 )
                
                PUSH AX ;saving

                ;a macro that will create delay with the help of nested loops so to have different time each time
                delay

                MOV AH, 00h  ; for getting the system time       
                INT 1AH      ; the time will be stored in cx and dx     

                mov  AX, DX 
                mov  DX, 0   ;clearing the dx to hold the remainder address
                mov  CX, 6   ;will take the mod like to get candies id as we have id's from 0 to 5
                div  CX

                MOV WORD PTR [SI] , DX

                POP AX ;to use it with original value

            .ENDIF
            

        ADD SI, 14 ;going to next ibdexes
        ADD AX, 1  ;adding the count


    .ENDW

    POP var4
    POP var3
    POP var2
    POP var1

    ;poping all of the general purpose registers pushed
    POPA

ENDM 


updateforLevelTwo MACRO 

;pushing the all general purpose registers
    PUSHA
    ;local variables
    PUSH temp ;used for mapping correct vlue
    PUSH var1 ;the counting 

    MOV var1 , 0
    
    MOV SI, OFFSET candyBoard

    ;count of rows
    MOV AX, 40
    MOV BX, 60

    ;count of columns
    MOV CX, 60
    MOV DX, 80

    ; will visit the ---- 7 rows
    .WHILE(CX<200 )

        MOV AX, 40
        MOV BX, 60
        
      
        ;will print the borders of columns ... visit 7 coloumns
        .WHILE(AX<180)
            
        
             ;if empty then bring upper
            .IF( var1 == 0 || var1 == 1 || var1 == 3 || var1 == 5 || var1 == 6 || var1 == 27 || var1 == 21 || var1 ==48 || var1 == 47 || var1 == 42 || var1 == 43 || var1 == 45)

                
                ;the bloakage
                MOV WORD PTR [SI], -1
                
            .ENDIF

            ADD AX, 20
            ADD BX, 20
            ADD SI, 2
            ;count var1
            INC var1

        .ENDW

        ADD CX, 20
        ADD DX, 20

        

    .ENDW

    ;restore old value
    POP var1
    POP temp

    ;poping the all general purpose registers
    POPA

    
ENDM

updateforLevelThree MACRO 

;pushing the all general purpose registers
    PUSHA
    ;local variables
    PUSH temp ;used for mapping correct vlue
    PUSH var1 ;the counting 

    MOV var1 , 0
    
    MOV SI, OFFSET candyBoard

    ;count of rows
    MOV AX, 40
    MOV BX, 60

    ;count of columns
    MOV CX, 60
    MOV DX, 80

    ; will visit the ---- 7 rows
    .WHILE(CX<200 )

        MOV AX, 40
        MOV BX, 60
        
      
        ;will print the borders of columns ... visit 7 coloumns
        .WHILE(AX<180)
            
        
             ;if empty then bring upper
            .IF( var1 == 21 || var1 == 22 || var1 == 23 || var1 == 24 || var1 == 25 || var1 == 26 || var1 == 27 || var1 ==3 || var1 == 10 || var1 == 17 || var1 == 24 || var1 == 31 || var1 == 38 || var1 == 45)

                
                ;the bloakage
                MOV WORD PTR [SI], -1
                
            .ENDIF

            ADD AX, 20
            ADD BX, 20
            ADD SI, 2
            ;count var1
            INC var1

        .ENDW

        ADD CX, 20
        ADD DX, 20

        

    .ENDW

    ;restore old value
    POP var1
    POP temp

    ;poping the all general purpose registers
    POPA

    
ENDM

writeInfile MACRO

    PUSHA 

    mov AH,3ch
    mov CL,2
    mov DX,offset fname
    int 21h

    mov AH,3dh
    int 21h

    mov CX,lengthof array
    mov BX,AX
    mov DX,offset array
    mov AH,40h
    int 21h

    mov CX,lengthof file1
    mov DX,offset file1
    mov AH,40h
    int 21h

    mov CX,lengthof file2
    mov DX,offset file2
    mov AH,40h
    int 21h

    mov CX,lengthof file3
    mov DX,offset file3
    mov AH,40h
    int 21h

    mov CX,lengthof file4
    mov DX,offset file4
    mov AH,40h
    int 21h


    POPA

ENDM

; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
; :::::::::::::::::::::: Macros ended :::::::::::::::::::::::
; :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

.MODEL SMALL
.STACK 100H
.386
.DATA

    gameName DB "CANDY CRUSH!!! $"
    msg0     DB "Enter your name : "
    playerN  DB 50 dup('$')
    msg1     DB "Welcome $"
    msg2     DB "RULES FOR THE GAME!!! $"
    decor    DB "**** $"
    rule1    DB "1) In this game rules will be limited      for each level. $"
    rule2    DB "2) You can combine 3,4 or 5 candies        together to pop them. $"
	rule3    DB "3) Combining 5 candies would make a        COLOUR BOMB. $"
	rule4    DB "4) Swapping the colour bomb with any       candy would pop all of that candy. $"
    rule5    DB "5) Levels passing criteria is :  $"
    append1  DB  "- Level 1 = 20  $"
    append2  DB  "- Level 2 = 30  $"
    append3  DB  "- Level 3 = 46 $" 

    levelMsg DB  "Level : $"
    scoreMsg DB  "Score : $"
    moveMsg  DB  "Moves : $"

    victorMsg DB " You Won $"
    loosedMsg DB " You Lost $"


    ;for storing score
    score    DW  0

    ;for moves
    moves    DW  15

    ;for level
    Level    DW  '1'

    ;for holding the co-ordinates
    var1     DW  0
    var2     DW  0
    var3     DW  0
    var4     DW  0
    temp     DW  0

    decision DW 0

    ;the board 7*7 -- the dummy
    candyBoardDummy DW 49 dup(0)

    ;the board 7*7 -- the main
    candyBoard DW 49 dup(0)

    ;the one clicked to swap
    xCod    DW  0
    yCod    DW  0


    ;swapped with the one
    xCodS   DW  0
    yCodS   DW  0

    ;the mapped orignial co-ordinates
    xStart  DW 0
    xEnd    DW 0
    yStart  DW 0
    yEnd    DW 0


    index1   DW -1 ;swap this
    indeX2   DW -1 ;swap with


.CODE 

    JMP Main

       ;for page 1 --- the name of game --- developers name
    ;taking the name of the player
    page1 PROC
		
	
        ;designing  
        ;making aquares of red color
        drawBorder 0,  319,  0, 199,  01h, 00
        drawBorder 5,  314,  5, 195,  02h, 00

        drawBorder 10, 309,  10, 189, 03h, 00
        drawBorder 15, 304,  15, 184, 04h, 00

        drawBorder 20, 299, 20, 179,  05h, 00
        drawBorder 25, 295, 25, 175,  0Eh, 00

       
        
        ;setting the cursor
        setCursor 8 ,15,0

        ;displaying the game name
        printString gameName
    
        ;for moving to next line
        nextLine

        ;setting the cursor
        setCursor 12,5,0

        ;taking input
        printString msg0
        inputName playerN
        printString playerN

        ;setting the background again black
        setBackground 00h
		
			
        ;again setting the design
         drawBorder 25, 295, 25, 175,  0Eh, 00
        drawBorder 20, 299, 20, 179,  05h, 00

        drawBorder 15, 304,  15, 184, 04h, 00
        drawBorder 10, 309,  10, 189, 03h, 00

        drawBorder 5,  314,  5, 195,  02h, 00
        drawBorder 0,  319,  0, 199,  01h, 00



        ;setting the cursor
        setCursor 8 ,15,0

        ;displaying the game name
        printString gameName
    
        ;for moving to next line
        nextLine

        ;setting the cursor
        setCursor 12,7,0
        
        ;for welcominf user
        printString decor
        printString msg1
        printString playerN
        printString decor


        ;waiting for keyboard input
        MOV AH, 0 ;keyboard reading
        INT 16H

        setBackground 00h
    

        RET ;returning
        
    page1 ENDP

    ;the page 2 control all rules printing and all that
    page2 PROC

        ;setting background to white
        setBackground 00h
		
        ;making design

        drawBorder 0,   0,  0, 200,  0Eh, 01
        drawBorder 10,  0,  0, 200,  05h, 01
        drawBorder 20,  0,  0, 200,  04h, 01
        drawBorder 30,  0,  0, 200,  03h, 01
        drawBorder 40,  0,  0, 200,  02h, 01
        drawBorder 50,  0,  0, 200,  01h, 01
        drawBorder 60,  0,  0, 200,  05h, 01
        drawBorder 70,  0,  0, 200,  04h, 01
        drawBorder 80,  0,  0, 200,  03h, 01
        drawBorder 90,  0,  0, 200,  02h, 01
        drawBorder 100,  0,  0, 200,  05h, 01
        drawBorder 110,  0,  0, 200,  04h, 01
        drawBorder 120,  0,  0, 200,  03h, 01
        drawBorder 130,  0,  0, 200,  02h, 01
        drawBorder 140,  0,  0, 200,  01h, 01
        drawBorder 150,  0,  0, 200,  0Ch, 01
        drawBorder 160,  0,  0, 200,  0Ch, 01
        drawBorder 170,  0,  0, 200,  01h, 01
        drawBorder 180,  0,  0, 200,  02h, 01
        drawBorder 190,  0,  0, 200,  03h, 01
        drawBorder 200,  0,  0, 200,  04h, 01
        drawBorder 210,  0,  0, 200,  05h, 01
        drawBorder 220,  0,  0, 200,  01h, 01
        drawBorder 230,  0,  0, 200,  02h, 01
        drawBorder 240,  0,  0, 200,  03h, 01
        drawBorder 250,  0,  0, 200,  04h, 01
        drawBorder 260,  0,  0, 200,  05h, 01
        drawBorder 270,  0,  0, 200,  01h, 01
        drawBorder 280,  0,  0, 200,  02h, 01
        drawBorder 290,  0,  0, 200,  03h, 01
        drawBorder 300,  0,  0, 200,  04h, 01
        drawBorder 310,  0,  0, 200,  05h, 01
        drawBorder 319,  0,  0, 200,  0Eh, 01
		

        ;for drawind boder main
		comment ! 
		drawBorder 0, 319, 0, 199,  0Eh, 02
        drawBorder 1, 318, 1, 198,  05h, 02
        drawBorder 2, 317, 2, 197,  04h, 02
        drawBorder 3, 316, 3, 196,  03h, 02
        drawBorder 4, 315, 4, 195,  02h, 02
        drawBorder 5, 314, 5, 194,  01h, 02
        drawBorder 6, 313, 6, 193,  0Eh, 02
        drawBorder 7, 312, 7, 192,  05h, 02
        drawBorder 8, 311, 8, 191,  04h, 02
        drawBorder 9, 310, 9, 190,  03h, 02
		!


        ;setting the cursor
        setCursor 5,3,0

        ;displaying the rules
        printString decor
        printString msg2
        printString decor

        ;for moving to next line
        nextLine

        ;setting the cursor
        setCursor 10,2,0

        ;Rule 1
        printString rule1

        ;for moving to next line
        nextLine

        ;setting the cursor
        setCursor 12,2,0

        ;Rule 2
        printString rule2

        ;for moving to next line
        nextLine

        ;setting the cursor
        setCursor 14,2,0

        ;Rule 3
        printString rule3
		
		;setting the cursor
        setCursor 16,2,0

        ;Rule 3
        printString rule4
		
		
		setCursor 18,2,0
		
		; rule 5
		printString rule5
		
		
		
        ;setting the cursor
        setCursor 19,2,0

        ;Rule 1
        printString append1

        ;for moving to next line
        nextLine
        ;setting the cursor
        setCursor 20,2,0

        ;Rule 1
        printString append2

        ;for moving to next line
        nextLine
        ;setting the cursor
        setCursor 21,2,0

        ;Rule 1
        printString append3

        ;populates the random numbers in the board and dummy board
        populateMe candyBoardDummy, candyBoard

        ;waiting for keyboard input
        MOV AH, 0 ;keyboard reading
        INT 16H

        RET ;returning
        
    page2 ENDP

    ;the page 3 control all candies and board
    page3 PROC
        
        CALL levelOne
        CALL levelTwo
        CALL levelThree

        RET ;returning
    page3 ENDP

    ;level 1
    levelOne PROC

        PUSHA

            ;setting the cursor
            setCursor 2 ,10,0

            ;displaying the game name
            printString gameName

            ;drawing the board with candies
            drawBoard 07h, 02, candyBoardDummy, candyBoard
            
        
            ;for showing mouse
            showMouse 160, 100

            ;the restricion
            restrictMouse 0, 620, 0, 185
            
        
            MOV moves, 15
            MOV score, 0
        
            .WHILE (moves > 0 && score <= 20)

                ;setting the cursor
                setCursor 2 ,10,0

                ;displaying the game name
                printString gameName

                ;showing again
                showMouse xCodS, yCodS
                ;the restricion again
                restrictMouse 0, 620, 0, 185

                ;taking the mose click
                mouseClick xCod, yCod

                ;refereshing
                Mov index1, -1

                ;for looking the originallay the co-ordinates to see the index
                mapRealCod xCod, yCod, index1
                
                showMouse xCod, yCod ;again showing mouse

                ;for giving delay
                MOV CX, 08H
                MOV DX, 0FFFFH ;delay
                MOV AH, 86H
                INT 15H

                ;taking the mose click to 
                mouseClick xCodS, yCodS

                ;refereshing
                MOV index2, -1
                
                ;for looking the originallay the co-ordinates to see the index
                mapRealCod xCodS, yCodS, index2
            
                showMouse xCodS, yCodS ;again showing
            
            ;the valid moves not out of range nor same
            ;if invalid jmp to nothing --- do not swap
            ;if same box swaping request again no swaping
            
                MOV AX, index1
                MOV bx, index2 

                .IF(AX == BX )
                    ;for giving delay
                    MOV CX, 08H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H
                
                    ;resetting mouse
                    MOV AX, 0
                    INT 33H

                    ;only recolor the selected grid
                    drawGrid 07h, 02
                    JMP doNothing

                .ENDIF

                
                .IF( (index1 == -1 || index2 == -1) )

                    ;for giving delay
                    MOV CX, 08H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H
                
                    ;resetting mouse
                    MOV AX, 0
                    INT 33H

                    ;only recolor the selected grid
                    drawGrid 07h, 
                    
                    JMP doNothing
                
                .ENDIF 

            

                MOV decision, 0
                MOV AX, index1
                MOV Bx, index2 

                ;now checking that if not neighbours then don't swap onlu neighbours can be swapped
                ;the below macro will check neighbours and will return temp with 0 if not neighbours and 1 if they are
                checkNeighbourhood index1, index2, decision ;will check index2 was in neighbour of index1 or not

                ;the result will be in cx thus
                .IF ( decision == 0 )

                    ;for giving delay
                    MOV CX, 08H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H
                
                    ;resetting mouse
                    MOV AX, 0
                    INT 33H

                    ;only recolor the selected grid
                    drawGrid 07h, 
                    
                    JMP doNothing

                .ENDIF

                ;now swap
                swapUs index1, index2
                SUB moves, 1 ;1 move gone

                ;checking for combinations after swaping and if combo is there pops candy
                ;if bomb formed also make the bomb
                ;also update score if poped
                PUSH moves

                    MOV decision, 0
                    checkCombination index1, index2, decision

                POP moves
                ;for giving delay
                MOV CX, 08H
                MOV DX, 0FFFFH ;delay
                MOV AH, 86H
                INT 15H
            
                ;resetting mouse
                MOV AX, 0
                INT 33H

            
                setBackground 00h
                drawBoard 07h, 02, candyBoardDummy, candyBoard

                ;for giving delay
                MOV CX, 03H
                MOV DX, 0FFFFH ;delay
                MOV AH, 86H
                INT 15H

            
                ;at starting making ax = 1 that to check combination
                .REPEAT 

                    MOV decision, 0 ;update made or not

                    ;for giving delay
                    MOV CX, 01H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H
                    
                    ;updating the board bringing candies down
                    MOV AX, 0

                    .While( AX < 7 )

                        updateBoard decision ;bring candies down
                        INC AX

                    .ENDW

                    ;there was no update
                    .IF ( decision == 0 )

                        JMP doNothing

                    .ENDIF
            
                    setBackground 00h
                    drawBoard 07h, 02, candyBoardDummy, candyBoard

                    ;ax will contain the more combintions after droping that down created or not
                    ;checkCombinwholeboard AX ;a mcro that will check the whole board to see the combination

                .UNTIL( decision == 1 )
            
                doNothing:

            

            .ENDW

            ;showing again
            showMouse xCodS, yCodS
            ;the restricion again
            restrictMouse 0, 620, 0, 185

            ;adding moves to score
            .IF(moves > 0 )
                SHL moves, 1
                MOV AX, Moves
                ADD score, AX
            .ENDIF

            ;for giving delay
                    MOV CX, 03H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H

            ;msg
            .IF(score >= 20 )
                setBackground 00h
                printString victorMsg
            .ELSE 
                setBackground 00h
                printString loosedMsg
            .ENDIF

        POPA

        RET

    levelOne ENDP


    
    ;the level 2 
    levelTwo PROC

         PUSHA

            updateforLevelTwo

            ;setting the cursor
            setCursor 2 ,10,0

            ;displaying the game name
            printString gameName

            ;drawing the board with candies
            drawBoard 07h, 02, candyBoardDummy, candyBoard
            
        
            ;for showing mouse
            showMouse 160, 100

            ;the restricion
            restrictMouse 0, 620, 0, 185
            
        
            MOV moves, 15
            MOV score, 0
        
            .WHILE (moves > 0 && score <= 30)

                ;setting the cursor
                setCursor 2 ,10,0

                ;displaying the game name
                printString gameName

                ;showing again
                showMouse xCodS, yCodS
                ;the restricion again
                restrictMouse 0, 620, 0, 185

                ;taking the mose click
                mouseClick xCod, yCod

                ;refereshing
                Mov index1, -1

                ;for looking the originallay the co-ordinates to see the index
                mapRealCod xCod, yCod, index1
                
                showMouse xCod, yCod ;again showing mouse

                ;for giving delay
                MOV CX, 08H
                MOV DX, 0FFFFH ;delay
                MOV AH, 86H
                INT 15H

                ;taking the mose click to 
                mouseClick xCodS, yCodS

                ;refereshing
                MOV index2, -1
                
                ;for looking the originallay the co-ordinates to see the index
                mapRealCod xCodS, yCodS, index2
            
                showMouse xCodS, yCodS ;again showing
            
            ;the valid moves not out of range nor same
            ;if invalid jmp to nothing --- do not swap
            ;if same box swaping request again no swaping
            
                MOV AX, index1
                MOV bx, index2 

                .IF(AX == BX )
                    ;for giving delay
                    MOV CX, 08H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H
                
                    ;resetting mouse
                    MOV AX, 0
                    INT 33H

                    ;only recolor the selected grid
                    drawGrid 07h, 02
                    JMP doNothing

                .ENDIF

                
                .IF( (index1 == -1 || index2 == -1) )

                    ;for giving delay
                    MOV CX, 08H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H
                
                    ;resetting mouse
                    MOV AX, 0
                    INT 33H

                    ;only recolor the selected grid
                    drawGrid 07h, 
                    
                    JMP doNothing
                
                .ENDIF 

            

                MOV decision, 0
                MOV AX, index1
                MOV Bx, index2 

                ;now checking that if not neighbours then don't swap onlu neighbours can be swapped
                ;the below macro will check neighbours and will return temp with 0 if not neighbours and 1 if they are
                checkNeighbourhood index1, index2, decision ;will check index2 was in neighbour of index1 or not

                ;the result will be in cx thus
                .IF ( decision == 0 )

                    ;for giving delay
                    MOV CX, 08H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H
                
                    ;resetting mouse
                    MOV AX, 0
                    INT 33H

                    ;only recolor the selected grid
                    drawGrid 07h, 
                    
                    JMP doNothing

                .ENDIF

                ;now swap
                swapUs index1, index2
                SUB moves, 1 ;1 move gone

                ;checking for combinations after swaping and if combo is there pops candy
                ;if bomb formed also make the bomb
                ;also update score if poped
                PUSH moves

                    MOV decision, 0
                    checkCombination index1, index2, decision

                POP moves
                ;for giving delay
                MOV CX, 08H
                MOV DX, 0FFFFH ;delay
                MOV AH, 86H
                INT 15H
            
                ;resetting mouse
                MOV AX, 0
                INT 33H

            
                setBackground 00h
                drawBoard 07h, 02, candyBoardDummy, candyBoard

                ;for giving delay
                MOV CX, 03H
                MOV DX, 0FFFFH ;delay
                MOV AH, 86H
                INT 15H

            
                ;at starting making ax = 1 that to check combination
                .REPEAT 

                    MOV decision, 0 ;update made or not

                    ;for giving delay
                    MOV CX, 01H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H
                    
                    ;updating the board bringing candies down
                    MOV AX, 0

                    .While( AX < 7 )

                        updateBoard decision ;bring candies down
                        INC AX

                    .ENDW

                    ;there was no update
                    .IF ( decision == 0 )

                        JMP doNothing

                    .ENDIF
            
                    setBackground 00h
                    drawBoard 07h, 02, candyBoardDummy, candyBoard

                    ;ax will contain the more combintions after droping that down created or not
                    ;checkCombinwholeboard AX ;a mcro that will check the whole board to see the combination

                .UNTIL( decision == 1 )
            
                doNothing:

            

            .ENDW

            ;showing again
            showMouse xCodS, yCodS
            ;the restricion again
            restrictMouse 0, 620, 0, 185

            ;adding moves to score
            .IF(moves > 0 )
                SHL moves, 1
                MOV AX, Moves
                ADD score, AX
            .ENDIF

            ;for giving delay
                    MOV CX, 03H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H

            ;msg
            .IF(score >= 20 )
                setBackground 00h
                printString victorMsg
            .ELSE 
                setBackground 00h
                printString loosedMsg
            .ENDIF

        POPA


         RET
        
    levelTwo ENDP

    ;third level
    levelThree PROC
        PUSHA
            updateforLevelThree

            MOV Level, '3'
            ;setting the cursor
            setCursor 2 ,10,0

            ;displaying the game name
            printString gameName

            ;drawing the board with candies
            drawBoard 07h, 02, candyBoardDummy, candyBoard
            
        
            ;for showing mouse
            showMouse 160, 100

            ;the restricion
            restrictMouse 0, 620, 0, 185
            
        
            MOV moves, 15
            MOV score, 0
        
            .WHILE (moves > 0 && score <= 46)

                ;setting the cursor
                setCursor 2 ,10,0

                ;displaying the game name
                printString gameName

                ;showing again
                showMouse xCodS, yCodS
                ;the restricion again
                restrictMouse 0, 620, 0, 185

                ;taking the mose click
                mouseClick xCod, yCod

                ;refereshing
                Mov index1, -1

                ;for looking the originallay the co-ordinates to see the index
                mapRealCod xCod, yCod, index1
                
                showMouse xCod, yCod ;again showing mouse

                ;for giving delay
                MOV CX, 08H
                MOV DX, 0FFFFH ;delay
                MOV AH, 86H
                INT 15H

                ;taking the mose click to 
                mouseClick xCodS, yCodS

                ;refereshing
                MOV index2, -1
                
                ;for looking the originallay the co-ordinates to see the index
                mapRealCod xCodS, yCodS, index2
            
                showMouse xCodS, yCodS ;again showing
            
            ;the valid moves not out of range nor same
            ;if invalid jmp to nothing --- do not swap
            ;if same box swaping request again no swaping
            
                MOV AX, index1
                MOV bx, index2 

                .IF(AX == BX )
                    ;for giving delay
                    MOV CX, 08H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H
                
                    ;resetting mouse
                    MOV AX, 0
                    INT 33H

                    ;only recolor the selected grid
                    drawGrid 07h, 02
                    JMP doNothing

                .ENDIF

                
                .IF( (index1 == -1 || index2 == -1) )

                    ;for giving delay
                    MOV CX, 08H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H
                
                    ;resetting mouse
                    MOV AX, 0
                    INT 33H

                    ;only recolor the selected grid
                    drawGrid 07h, 
                    
                    JMP doNothing
                
                .ENDIF 

            

                MOV decision, 0
                MOV AX, index1
                MOV Bx, index2 

                ;now checking that if not neighbours then don't swap onlu neighbours can be swapped
                ;the below macro will check neighbours and will return temp with 0 if not neighbours and 1 if they are
                checkNeighbourhood index1, index2, decision ;will check index2 was in neighbour of index1 or not

                ;the result will be in cx thus
                .IF ( decision == 0 )

                    ;for giving delay
                    MOV CX, 08H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H
                
                    ;resetting mouse
                    MOV AX, 0
                    INT 33H

                    ;only recolor the selected grid
                    drawGrid 07h, 
                    
                    JMP doNothing

                .ENDIF

                ;now swap
                swapUs index1, index2
                SUB moves, 1 ;1 move gone

                ;checking for combinations after swaping and if combo is there pops candy
                ;if bomb formed also make the bomb
                ;also update score if poped
                PUSH moves

                    MOV decision, 0
                    checkCombination index1, index2, decision

                POP moves
                ;for giving delay
                MOV CX, 08H
                MOV DX, 0FFFFH ;delay
                MOV AH, 86H
                INT 15H
            
                ;resetting mouse
                MOV AX, 0
                INT 33H

            
                setBackground 00h
                drawBoard 07h, 02, candyBoardDummy, candyBoard

                ;for giving delay
                MOV CX, 03H
                MOV DX, 0FFFFH ;delay
                MOV AH, 86H
                INT 15H

            
                ;at starting making ax = 1 that to check combination
                .REPEAT 

                    MOV decision, 0 ;update made or not

                    ;for giving delay
                    MOV CX, 01H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H
                    
                    ;updating the board bringing candies down
                    MOV AX, 0

                    .While( AX < 7 )

                        updateBoard decision ;bring candies down
                        INC AX

                    .ENDW

                    ;there was no update
                    .IF ( decision == 0 )

                        JMP doNothing

                    .ENDIF
            
                    setBackground 00h
                    drawBoard 07h, 02, candyBoardDummy, candyBoard

                    ;ax will contain the more combintions after droping that down created or not
                    ;checkCombinwholeboard AX ;a mcro that will check the whole board to see the combination

                .UNTIL( decision == 1 )
            
                doNothing:

            

            .ENDW

            ;showing again
            showMouse xCodS, yCodS
            ;the restricion again
            restrictMouse 0, 620, 0, 185

            ;adding moves to score
            .IF(moves > 0 )
                SHL moves, 1
                MOV AX, Moves
                ADD score, AX
            .ENDIF

            ;for giving delay
                    MOV CX, 03H
                    MOV DX, 0FFFFH ;delay
                    MOV AH, 86H
                    INT 15H

            ;msg
            .IF(score >= 20 )
                setBackground 00h
                printString victorMsg
            .ELSE 
                setBackground 00h
                printString loosedMsg
            .ENDIF

        POPA

    levelThree ENDP

    Main PROC

    MOV AX, @DATA
    MOV DS, AX
  
    ;setting our console to video mode
    MOV AH, 00H
    MOV AL, 13H  ; 320x200 pixels, 16 colors
    INT 10H
    
   
    ;calling page 1 functionality 
    CALL page1

    ;switching to next page -- page 2
    switchPage 1
    
    ;setting our console to video mode again
    MOV AH, 00H
    MOV AL, 13H  ; 320x200 pixels, 16 colors
    INT 10H

    ;calling page 2 functionality 
    CALL page2

    ;switching to next page -- page 3
    switchPage 2
  
   ;setting our console to video mode again
    MOV AH, 00H
    MOV AL, 13H  ; 320x200 pixels, 16 colors
    INT 10H
    
    ;calling page 3 functionality 
    CALL page3

Main ENDP

Exit:
    MOV AH, 4CH
    INT 21H
    END