; Author: Bar Ben Hamou
;Bmp pics
IDEAL
MODEL small
STACK 100h
DATASEG
; --------------------------
start1 db 'start.bmp',0
inst db 'instr.bmp',0
rules db 'rules.bmp', 0
GameOver db 'gameO.bmp', 0
filehandle dw ?
Header db 54 dup (0)
Palette db 256*4 dup (0)
palette_backup db 256*3 dup(?)
ScrLine db 320 dup (0)
wdth db 5
dir db 1 ;Enum that determines which direction the snake is.
over db 0 ;Boolean which determines whether the game is over.
colors db 3,5,6,7,8,2
colorPos db 0
colorSnake db 2
colorPortal db 6
snakeLen db 2
portalLen db 2
foodLoc dw 0
portalLoc db 10, 11, 55, 35
snakeLoc db 31, 19, 32, 19, 200 dup(0)

; --------------------------
CODESEG


;----Screen Cleaner----paints the screen with the color black
proc ClearScreen
push ax
push bx

mov ax, 0 ;Contains the color 
mov bx, 0 ;Contains the first byte that will be colored

clear:
	mov [es:bx], ax ;Painting the screen 
	inc bx
	cmp bx, 64000
	jne clear

pop bx
pop ax  
ret
endp ClearScreen
;----------------------

;----Delay----
proc delay
push ax
push cx
push dx

mov ah, 86h
mov cx, 1
xor dx, dx
int 15h

pop dx
pop cx
pop ax
ret
endp delay
;-------------

;-----Painting Snake Squares----the snake is assembelled with grids of 5x5, the screen is total of 64x40 grids
proc square ;(x [bp+4], y [bp+6], wdth [bp+8], color[bp+10])
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push di
	
	
	mov dx, [bp+6]
	mov cx, [bp+4]
	mov dh, cl ; dl = y, dh = x (of square on grid)
	
	mov ax, [bp+8]
	mul dh
	mov bx, ax
	
	mov ax, [bp+8]	
	mul dl
	mov dl, al
	
	; bx = x, dl = y (of point on screen)
	mov cx, [bp+8]
	mov ch, cl
	mov dh, 0

	outer: ;Each pixel in the greed is painted in green
		inner:
			mov di, bx
			
			mov ax, 320
			push dx
			mul dx
			pop dx
			add di, ax
			
			xor ax, ax
			mov al, [bp+10]
			mov [byte ptr es:di], al ;Coloring the snake squares

			
			inc bx
		
		dec ch
		jnz inner
		
		mov ax, [bp+8]
		mov ch, al
		sub bx, ax
		inc dl
		
	dec cl
	jnz outer
	
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	
ret 8
endp square
;-------------------------------

;----Mechanics----the settings of the movment of the snake and his state
proc mechanics ;(snakeLoc[bp+4], dir[bp+6], snakeLen[bp+8], over[bp+10], wdth[bp+12], snakeColor[bp+14], foodLoc[bp+16], *offset* snakeLen[bp+18], portalLoc[bp+20], portalLen[bp+22])
push bp
mov bp, sp
push ax
push si
push bx
push dx

	mov si, [bp+6]
	xor ax, ax
	mov ah, 1
	int 16h
	jne dontContinueMovment
	jmp continueMovment
	dontContinueMovment:
	mov ah, 0
	int 16h
	cmp al, 'q' ;Check if the 'q' was pressed - quit
	jne dontExit
	mov si, [bp+10]
	mov [byte ptr si], 1
	dontExit:
		checkLeft:
			cmp al, 'a' ;Check if 'a' was pressed - movment to the left.
			jne checkRight
			cmp [byte ptr si], 2 ;Check if the snake current direction is the oppsite to the wanted
			je continueMovment
			mov [byte ptr si], 1
			jmp continueMovment
		checkRight:
			cmp al, 'd' ;Check if 'd' was pressed - movment to the left.
			jne checkUp
			cmp [byte ptr si], 1 ;Check if the snake current direction is the oppsite to the wanted
			je continueMovment
			mov [byte ptr si], 2
			jmp continueMovment
		checkUp:
			cmp al, 'w' ;Check if 'w' was pressed - movment to the left.
			jne checkDown
			cmp [byte ptr si], 4 ;Check if the snake current direction is the oppsite to the wanted
			je continueMovment
			mov [byte ptr si], 3
			jmp continueMovment
		checkDown:
			cmp al, 's' ;Check if 's' was pressed - movment to the left.
			jne checkC
			cmp [byte ptr si], 3 ;Check if the snake current direction is the oppsite to the wanted
			je continueMovment
			mov [byte ptr si], 4
			jmp continueMovment
		checkC:
			cmp al, 'c'
			jne continueMovment
			push [bp+26]
			push [bp+14]
			push [bp+24]
			call changeColor
	continueMovment:
		;-----------Each time the snake moves, need to earase the squares in his last position-----------------------
			push ax
		
			mov bx, [bp+8] 
			mov al, [byte ptr bx]
			mov ah, 0
			mov bx, [bp+4]
			shl ax, 1
			add bx, ax
			sub bx, 2
			mov ax, 0
			push ax
			push bx
			mov ax, 1
			push ax
			push [bp+12]
			call draw
			
			pop ax
		;---------------------------------------------------------------------------------------------------------------
			mov bx, [bp+8]
			mov al, [byte ptr bx]
			mov ah, 0
			push ax
			push [bp+4]
			;According to the direction the snake moves.
			cmp [byte ptr si], 1
			jne moveR
			call moveLeft
			push [bp+10]
			mov bx, [bp+8]
			mov bl, [bx]
			mov bh, 0
			push bx
			push [bp+4]
			call checkCollisionWithSnake
			push [bp+22]
			push [bp+20]
			push [bp+18]
			push [bp+4]
			push [bp+16]
			call collisionWithFood
			
			moveR:
			cmp [byte ptr si], 2
			jne moveU
			call moveRight
			push [bp+10]
			mov bx, [bp+8]
			mov bl, [bx]
			mov bh, 0
			push bx
			push [bp+4]
			call checkCollisionWithSnake
			push [bp+22]
			push [bp+20]
			push [bp+18]
			push [bp+4]
			push [bp+16]
			call collisionWithFood
			
			moveU:
			cmp [byte ptr si], 3
			jne moveD
			call moveUp
			push [bp+10]
			mov bx, [bp+8]
			mov bl, [bx]
			mov bh, 0
			push bx
			push [bp+4]
			call checkCollisionWithSnake
			push [bp+22]
			push [bp+20]
			push [bp+18]
			push [bp+4]
			push [bp+16]
			call collisionWithFood
			
			moveD:
			cmp [byte ptr si], 4
			jne notD
			call moveDown
			push [bp+10]
			mov bx, [bp+8]
			mov bl, [bx]
			mov bh, 0
			push bx
			push [bp+4]
			call checkCollisionWithSnake
			push [bp+22]
			push [bp+20]
			push [bp+18]
			push [bp+4]
			push [bp+16]
			call collisionWithFood
			
			
			notD:
			
	rePaintSnake:
		push [bp+20]
		push [bp+6]
		push [bp+4]
		call collisionWithPortals
		push [bp+6]
		push [bp+10]
		push [bp+4]
		call collisionWithBorders
		mov bx, [bp+10]
		cmp [byte ptr bx], 1
		je endProc7
		mov bx, [bp+14]
		mov ah, 0
		mov al, [bx]
		push ax
		push [bp+4]
		mov bx, [bp+8]
		mov al, [byte ptr bx]
		mov ah, 0
		push ax
		push [bp+12]
		call draw

	endProc7:
pop dx
pop bx
pop si
pop ax
pop bp
ret 24
endp mechanics
;-----------------

;----Move to the Left----Moving the snake one step to the left by decreasing the x coordinate of the head.
proc moveLeft;(snakeLoc[bp+4], snakeLen[bp+6])
push bp
mov bp, sp
push bx
push cx
push si
push ax

	mov bx, [bp+4]
	mov si, [bp+6]
	add si, [bp+6]
	mov cx, [bp+6]
	dec cx
	loopMovmentLeft: ;Each square in the snake is moving to the place of the square that is in front of him.
		dec cx
		mov al, [bx+si-4] ;Replacing the x coordinates.
		mov [bx+si-2], al ;
		mov al, [bx+si-3] ; Replacing the y coordinates.
		mov [bx+si-1], al ;
		sub si, 2
		cmp cx, 0
		jne loopMovmentLeft
		
	dec [byte ptr bx]

pop ax
pop si
pop cx	
pop bx
pop bp
ret 4
endp moveLeft
;------------------------

;----Move to the Right----Moving the snake one step to the right by increasing the x coordinate of the head.
proc moveRight;(snakeLoc[bp+4], snakeLen[bp+6])
push bp
mov bp, sp
push bx
push cx
push si
push ax

	mov bx, [bp+4]
	mov si, [bp+6]
	add si, [bp+6]
	mov cx, [bp+6]
	dec cx
	loopMovmentRight: ;Each square in the snake is moving to the place of the square that is in front of him.
		dec cx
		mov al, [bx+si-4] ;Replacing the x coordinates.
		mov [bx+si-2], al ;
		mov al, [bx+si-3] ; Replacing the y coordinates.
		mov [bx+si-1], al ;
		sub si, 2
		cmp cx, 0
		jne loopMovmentRight
		
	inc [byte ptr bx]
	
pop ax
pop si
pop cx	
pop bx
pop bp
ret 4
endp moveRight
;------------------------

;----Move Upwards----Moving the snake one step upwards by decreasing the y coordinate of the head.
proc moveUp;(snakeLoc[bp+4], snakeLen[bp+6])
push bp
mov bp, sp
push bx
push cx
push si
push ax

	mov bx, [bp+4]
	mov si, [bp+6]
	add si, [bp+6]
	mov cx, [bp+6]
	dec cx
	loopMovmentUp: ;Each square in the snake is moving to the place of the square that is in front of him.
		dec cx
		mov al, [bx+si-4] ;Replacing the x coordinates.
		mov [bx+si-2], al ;
		mov al, [bx+si-3] ; Replacing the y coordinates.
		mov [bx+si-1], al ;
		sub si, 2
		cmp cx, 0
		jne loopMovmentUp
		
	dec [byte ptr bx+1]

pop ax
pop si
pop cx	
pop bx
pop bp
ret 4
endp moveUp
;------------------------

;----Move Downwards----Moving the snake one step upwards by increasing the y coordinate of the head.
proc moveDown;(snakeLoc[bp+4], snakeLen[bp+6])
push bp
mov bp, sp
push bx
push cx
push si
push ax

	mov bx, [bp+4]
	mov si, [bp+6]
	add si, [bp+6]
	mov cx, [bp+6]
	dec cx
	loopMovmentDown: ;Each square in the snake is moving to the place of the square that is in front of him.
		dec cx
		mov al, [bx+si-4] ;Replacing the x coordinates.
		mov [bx+si-2], al ;
		mov al, [bx+si-3] ; Replacing the y coordinates.
		mov [bx+si-1], al ;
		sub si, 2
		cmp cx, 0
		jne loopMovmentDown
		
	inc [byte ptr bx+1]

pop ax
pop si
pop cx	
pop bx
pop bp
ret 4
endp moveDown
;------------------------

;----Draw Snake----
proc draw ;(wdth[bp+4], snakeLen[bp+6], snakeLoc[bp+8], color[bp+10])
push bp
mov bp, sp
push cx
push si
push ax

	mov bx, 0
	mov ax, 0
	mov si, [bp+8]
	mov cx, [bp+6]
	painting:
		mov ax, [bp+10]
		push ax
		mov al, 5
		push ax
		mov al, [si+1]
		push ax
		mov al, [si]
		push ax
		call square
		add si, 2
	dec cx
	jnz painting

pop ax
pop si
pop cx
pop bp
ret 8
endp draw
;------------------

;----Check Collision with its self----
proc checkCollisionWithSnake ;(snakeLoc[bp+4], snakeLen[bp+6], over[bp+8])
push bp
mov bp, sp
push ax
push bx
push cx
push dx
push di
push si

	mov si, [bp+8] ;The boolean that determines whether the snake is dead.
	mov bx, [bp+4] 
	mov di, bx ;di now holding the head of the snake.
	mov ax, [bp+6]
	sal ax, 1
	sub ax, 2 
	add bx, ax ;bx now holding the tail of the snake.
	mov dx, [di]
	lopCollision:
		mov cx, [bx] 
		cmp dx, cx
		je collision
		sub bx, 2
		cmp bx, di
		jne lopCollision
		jmp exitProc
	collision:
		mov [byte ptr si], 1
		call startsound
		call delaySound
		call endsound
	exitProc:
	
pop si
pop di
pop dx
pop cx
pop bx
pop ax		
pop bp
ret 6 
endp checkCollisionWithSnake
;-------------------------------------

proc randomNumber ;(max[bp+4])
push bp
mov bp, sp
push ax
push cx
push dx	
		
		mov ah, 00h  	   	   ; Interrupts to get system time        
		int 1ah      		   ; CX:DX now hold number of clock ticks since midnight      

		mov cx, dx			;------------------------
		sal dx, 7           ;  
		xor dx, cx			;
							;
		mov cx, dx			;
		sar dx, 9			;creates a random number
		xor dx, cx			;
							;
		mov cx, dx			;
		sal dx, 8			;
		xor dx, cx          ;------------------------

		mov ax, dx

		xor dx, dx
		mov cx, [bp+4]
		div cx 					; Here dx contains the remainder of the division - from 0 to max-1
		
		mov [bp+4], dx
		
pop dx
pop cx
pop ax
pop bp
ret 
endp randomNumber

;----Generate New Random Food----Creates new food and check if it on one of the portals or oon the snake -> if it does so regenerate.
proc generateNewFood ;(foodLoc[bp+4], snakeLoc[bp+6], portalLoc[bp+8], snakeLen[bp+10], portalLen[bp+12])
push bp
mov bp, sp
push ax
push bx
push cx
push dx
push di	
push si	
	
	mov di, [bp+4]

	reGenerate:
		push 64
		call randomNumber
		pop dx
		mov [di], dl			; The x coordinate of the food is transfered to the data segment.
	
		push 40
		call randomNumber
		pop dx
		mov [di+1], dl			; The y coordinate of the food is transfered to the data segment.
		
	checkIfOnSnake:
		xor ax, ax
		xor si, si
		xor cx, cx
		mov bx, [bp+6]
		mov cx, [bp+10]
		loopCheckingSnake: 				;This loop is checking if the food is located on the snake, if it does so reGenerate location.
			dec cx
			mov si, [bx]
			mov ax, [di]
			cmp si, ax
			jne continueChecking1
			jmp reGenerate
			continueChecking1:
				add bx, 2
				cmp cx, 0
				jne loopCheckingSnake
			
	checkIfOnPortal:
		xor ax, ax
		xor si, si
		xor cx, cx
		mov bx, [bp+8]
		mov cx, [bp+12]
		loopCheckingSnake2:			;This loop is checking if the food is located on the snake, if it does so reGenerate location.
			dec cx
			mov si, [bx]
			mov ax, [di]
			cmp si, ax
			jne continueChecking
			jmp reGenerate
			continueChecking:
				add bx, 2
				cmp cx, 0
				jne loopCheckingSnake2
	

	printFood:
		xor ax, ax
		mov al, 4
		push ax
		mov al, 5
		push ax
		mov al, [di+1]
		push ax
		mov al, [di]
		push ax
		call square
		
pop si
pop di	
pop dx	
pop cx	
pop bx	
pop ax	
pop bp	
ret 10
endp generateNewFood
;--------------------------------

;----Collision With Food----
proc collisionWithFood ;(foodLoc[bp+4], snakeLoc[bp+6], *offset* snakeLen[bp+8], portalLoc[bp+10], portalLen[bp+12])
push bp
mov bp, sp
push bx
push si
push ax
push di
push si
push cx

	mov bx, [bp+4]
	mov si, [bp+6]
	
	mov ax, [bx]
	mov di, [si]
	
	cmp ax, di ;Checking if the snake has reached the food - if yes then grow and generate new food , if not then continue mainLoop.
	jne endProc3
	mov bx, [bp+8]
	inc [byte ptr bx]
	mov cl, [bx]
	dec cl	
	
	mov bx, [bp+6] ;
	mov ch, 0      ;
	mov di, cx	   ;
	shl cx, 1      ;Moving the coordinates of the old tail to the new tail so in the next movement the snake will be shown bigger.
	add bx, cx     ;
	
	mov ax, [bx-2] ;
	mov [bx], ax   ;
	
	
	push [bp+12]
	push di
	push [bp+10]
	push [bp+6]
	push [bp+4]
	call generateNewFood
	
	mov ah, 86h
	mov cx, 1
	mov dx, 24h
	int 15h
	
	push [bp+12]
	push di
	push [bp+10]
	push [bp+6]
	push [bp+4]
	call generateNewPortals
	endProc3:

pop cx
pop si	
pop di
pop ax
pop si
pop bx
pop bp
ret 10
endp collisionWithFood
;---------------------------

;----Coliision With Portals----
proc collisionWithPortals;(snakeLoc[bp+4], dir[bp+6], portalLoc[bp+8])
push bp
mov bp, sp
push di
push bx
push si
push ax

	mov si, [bp+4]
	mov bl, [byte ptr si] ;x of snake
	mov bh, [byte ptr si+1] ;y of snake
	mov si, [bp+8]
	mov al, [byte ptr si] ; x of portal1
	mov ah, [byte ptr si+1] ; y of portal1

	cmp ax, bx
	jne noCollisionWithPortal1
	mov si, [bp+8]
	mov al, [byte ptr si+2]
	mov ah, [byte ptr si+3]
	mov si, [bp+6]
	mov bl, [byte ptr si]
	mov si, [bp+4]
	mov [byte ptr si], al
	mov [byte ptr si+1], ah
	dirCheck:
			cmp bl, 1
			jne right1
			dec [byte ptr si]
		right1:
			cmp bl, 2
			jne up1
			inc [byte ptr si]
		up1:
			cmp bl, 3
			jne down1
			dec [byte ptr si+1]
		down1:
			cmp bl, 4
			jne endProc4
			inc [byte ptr si+1]
			jmp endProc4
			
	noCollisionWithPortal1:
	mov si, [bp+8]
	mov al, [byte ptr si+2]
	mov ah, [byte ptr si+3]
	cmp ax, bx
	jne endProc4
	mov si, [bp+8]
	mov al, [byte ptr si]
	mov ah, [byte ptr si+1]
	mov si, [bp+6]
	mov bl, [byte ptr si]
	mov si, [bp+4]
	mov [byte ptr si], al
	mov [byte ptr si+1], ah
	dirCheck2:
			cmp bl, 1
			jne right2
			dec [byte ptr si]
		right2:
			cmp bl, 2
			jne up2
			inc [byte ptr si]
		up2:
			cmp bl, 3
			jne down2
			dec [byte ptr si+1]
		down2:
			cmp bl, 4
			jne endProc4
			inc [byte ptr si+1]
		
		
		endProc4:
pop ax
pop si
pop bx
pop di
pop bp
ret 6
endp collisionWithPortals
;----Coliision With Portals----

;----Collision With Borders----
proc collisionWithBorders ;(snakeLoc[bp+4], over[bp+6], dir[bp+8])
push bp
mov bp, sp
push bx
push si
push di	
	
	mov di, [bp+6]
	mov si, [bp+8]
	mov bx, [bp+4]
	mov ax, [bx]
	
	checkLeftBorder: ;Checking whether the direction of the snake is left and his x coordinate is lower than zero.
		cmp [byte ptr si], 1
		jnz checkRightBorder
		cmp al, 0
		jge checkRightBorder
		mov [byte ptr di], 1
		call startsound
		call delaySound
		call endsound
		jmp endp6
	checkRightBorder: ;Checking whether the direction of the snake is right and his x coordinate is greater than 63.
		cmp [byte ptr si], 2
		jnz checkUpBorder
		cmp al, 63
		jle checkUpBorder
		mov [byte ptr di], 1
		call startsound
		call delaySound
		call endsound
		jmp endp6
	checkUpBorder: ;Checking whether the direction of the snake is up and his y coordinate is lower than zero.
		cmp [byte ptr si], 3
		jnz checkDownBorder
		cmp ah, 0
		jge checkDownBorder
		mov [byte ptr di], 1
		call startsound
		call delaySound
		call endsound
		jmp endp6
	checkDownBorder: ;Checking whether the direction of the snake is down and his y coordinate is grater than 3.
		cmp [byte ptr si], 4
		jnz endp6
		cmp ah, 39
		jle endp6
		mov [byte ptr di], 1
		call startsound
		call delaySound
		call endsound
endp6:
pop di	
pop si
pop bx
pop bp
ret 6
endp collisionWithBorders
;------------------------------

;---- Change Snake Color----If c is pressed color the snake with colors.
proc changeColor ;(colors[bp+4], snakeColor[bp+6], colorPos[bp+8])
push bp
mov bp, sp
push bx
push di
push si
push ax

	mov bx, [bp+4]
	mov si, [bp+6]
	mov di, [bp+8]
	
	add bl, [di]
	mov ah, 0
	mov al, [byte ptr bx]
	mov [byte ptr si], al
	
	inc [byte ptr di]
	cmp [byte ptr di], 6
	jne endProc5
	mov [byte ptr di], 0
		
	
	endProc5:
	
pop ax
pop si
pop di
pop bx
pop bp	
ret 6
endp changeColor
;---------------------------

;----Generate New Random Food----Creates new food and check if it on one of the portals or oon the snake -> if it does so regenerate.
proc generateNewPortals ;(foodLoc[bp+4], snakeLoc[bp+6], portalLoc[bp+8], snakeLen[bp+10], portalLen[bp+12])
push bp
mov bp, sp
push ax
push bx
push cx
push dx
push di	
push si	
	
	mov di, [bp+8]
	
	earasePortals:
	push 0
	push di
	push [bp+12]
	push 5
	call draw
		
	mov cx, 2
	lopPortals:
	push cx
		reGenerate1:
			push 32
			call randomNumber
			pop dx
			dec cx
			sal cx, 5
			add dx, cx
			mov [di], dl			; The x coordinate of the food is transfered to the data segment.
		
			push 40
			call randomNumber
			pop dx
			mov [di+1], dl			; The y coordinate of the food is transfered to the data segment.
			
		checkIfOnSnake1:
			xor ax, ax
			xor si, si
			xor cx, cx
			mov bx, [bp+6]
			mov cx, [bp+10]
			loopCheckingSnake1: 				;This loop is checking if the food is located on the snake, if it does so reGenerate location.
				dec cx
				mov si, [bx]
				mov ax, [di]
				cmp si, ax
				jne continueChecking2
				jmp reGenerate
				continueChecking2:
					add bx, 2
					cmp cx, 0
					jne loopCheckingSnake1
					
			checkIfOnFood:
				xor ax, ax
				xor si, si
				xor cx, cx
				mov bx, [bp+4]
				mov si, [bx]
				mov ax, [di]
				cmp si, ax
				je reGenerate1
	add di, 2
	mov ah, 86h
	mov cx, 1
	mov dx, 10h
	int 15h
	pop cx
	dec cx
	jne lopPortals
	
	
	printPortals:
		push 6
		push [bp+8]
		push [bp+12]
		push 5
		call draw
		
pop si
pop di	
pop dx	
pop cx	
pop bx	
pop ax	
pop bp	
ret 10
endp generateNewPortals
;--------------------------------

proc startsound
	;when the snake dies a sound will start for a couple seconds
	push ax
	in al, 61h
	or al, 00000011b
	out 61h, al
	mov al, 0b6h
	out 43h, al
	mov ax, 8609
	out 42h, al
	mov al, ah
	out 42h, al
	pop ax
	ret
endp startsound

proc startsound1
	;when the game begin a sound will start for a couple seconds
	push ax
	in al, 61h
	or al, 00000011b
	out 61h, al
	mov al, 0CCh
	out 43h, al
	mov ax, 8609
	out 42h, al
	mov al, ah
	out 42h, al
	pop ax
	ret
endp startsound1

proc delaySound
	push ax
	push cx
	push dx
	mov ah, 86h              ;;;;;;;;;;;;delay
	mov cx, 5                ;;;;;;;;;;;;delay
	mov dx,0                 ;;;;;;;;;;;;delay
	int 15h                  ;;;;;;;;;;;;delay
	pop dx
	pop cx
	pop ax
ret
endp delaySound

proc endsound
	;when the snake dies or the game begins a sound will start so we need to end it
	push ax
	in al, 61h
	and al, 11111100b
	out 61h, al
	pop ax
	ret
endp endsound


proc printimage
push bp
mov bp, sp
push ax
push bx
push cx
push dx
push si
push di
	;;;;;;;;;;;;;;;
	;the function is getting the name of the file
	mov dx,[bp+12]
	;;;;;;;;;;;;;;;
	mov ah, 3Dh
	xor al, al
	int 21h
	mov [bp+10], ax
	mov ah,3fh
	mov bx, [bp+10]
	mov cx,54
	mov dx, [bp+8]
	int 21h
	mov ah,3fh
	mov cx,400h
	mov dx, [bp+6]
	int 21h
	mov si, [bp+6]
	mov cx,256
	mov dx,3C8h
	mov al,0
	out dx,al
	inc dx
	PalLoop:
		mov al,[si+2] 
		shr al,2 
		out dx,al 
		mov al,[si+1] 
		shr al,2
		out dx,al 
		mov al,[si]
		shr al,2
		out dx,al 
		add si,4
		loop PalLoop
		mov ax, 0A000h
		mov es, ax
		mov cx,200
	PrintBMPLoop :
		push cx
		mov di,cx
		shl cx,6
		shl di,8
		add di,cx
		mov ah,3fh
		mov cx,320
		mov dx, [bp+4]
		int 21h
		cld 
		mov cx,320
		mov si, [bp+4]
		rep movsb 
		pop cx
	loop PrintBMPLoop
	mov bx, dx
	mov ah, 3eh
	int 21h
;;;;;;;;;;;;;;;
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp
ret 10
endp printimage

start:
	mov ax, @data
	mov ds, ax
	
;----Graphic Mode----	
mov ax, 0a000h
mov es, ax
mov ax, 13h
int 10h
;-------------------- 

;----Start----
firstScreen:
		
	; mov dx, 03c7h
	; xor al, al
	; mov di, offset palette_backup
	; mov cl, 0ffh
	; backup_palette_loop:
	; out dx, al
	; push ax dx
	; mov dx, 03c9h
	; in al, dx
	; mov [di], al
	; in al, dx
	; mov [di+1], al
	; in al, dx
	; mov [di+2], al
	; pop dx ax
	; add di, 3
	; inc al
	; dec cl
	; jnz backup_palette_loop
	
	push offset start1
	mov ax, [filehandle]
	push ax
	push offset Header
	push offset Palette
	push offset ScrLine
	call printimage

	b:
		mov ah,0
		int 16h
		cmp al,'b'
		jne q
		jmp firstPaint
	q:
		cmp al, 'q'
		jne i
		jmp gameOver1
		
	i:
		cmp al, 'i'
		jne r
		push offset inst
		mov ax, [filehandle]
		push ax
		push offset Header
		push offset Palette
		push offset ScrLine
		call printimage
	m:
		mov ah, 0
		int 16h
		cmp al, 'm'
		jne m
		jmp firstScreen
	
	r:
			cmp al, 'r'
			jne b
			push offset rules
			mov ax, [filehandle]
			push ax
			push offset Header
			push offset Palette
			push offset ScrLine
			call printimage
	m2:
		mov ah, 0
		int 16h
		cmp al, 'm'
		jne m2
		jmp firstScreen
;--------------
	firstPaint: ;Creates the intialized snake, food and portals.
		; mov dx, 03c8h
		; xor al, al
		; mov di, offset palette_backup
		; mov cl, 0ffh
		; restore_palette_loop:
		; out dx, al
		; push ax dx
		; mov dx, 03c9h
		; mov al, [di]
		; out dx, al
		; mov al, [di+1]
		; out dx, al
		; mov al, [di+2]
		; out dx, al
		; pop dx ax
		; add di, 3
		; inc al
		; dec cl
		; jnz restore_palette_loop
		; 	
		; call clearScreen
		mov ax, 0
		mov ax, 13h
		int 10h
		
		mov ax, 0
		mov al, [colorSnake]
		push ax
		push offset snakeLoc
		mov al, [snakeLen]
		push ax
		mov al, [wdth]
		push ax
		call draw
		
		mov al, [colorPortal]
		push ax
		push offset portalLoc
		mov al, [portalLen]
		push ax
		mov al, [wdth]
		push ax
		call draw	
		
		mov al, [portalLen]
		push ax
		mov al, [snakeLen]
		push ax	
		push offset portalLoc	
		push offset snakeLoc		
		push offset foodLoc
		call generateNewFood
		
	call startsound1
	call delaySound
	call endsound
		
xor ax,ax
run:
	push offset colorPos
	push offset colors
	mov al, [portalLen]
	push ax
	push offset portalLoc
	push offset snakeLen
	push offset foodLoc
	;mov al, [colorSnake]
	;push ax
	push offset colorSnake
	push offset wdth
	push offset over
	push offset snakeLen
	push offset dir
	push offset snakeLoc
	call mechanics
	call delay
	cmp [byte ptr over], 1
	jnz run





;----Game Over----
gameOver1:
	push offset GameOver
	push [filehandle]
	push offset Header
	push offset Palette
	push offset ScrLine
	call printimage
	loopEnd: ;Waiting for the player to exit the game
		mov ah, 0
		int 16h
		cmp al, 'q'
		jne hi2
		jmp hi
		hi2:
		cmp al, 'p'
		jne loopEnd
		
	resetData:
		mov [byte ptr dir], 1
		mov [byte ptr over], 0
		mov [byte ptr colorPos], 0
		mov [byte ptr colorSnake], 2
		mov [byte ptr snakeLen], 2
		mov [word ptr foodLoc], 0
		mov [byte ptr portalLoc], 10
		mov [byte ptr portalLoc+1], 11
		mov [byte ptr portalLoc+2], 55
		mov [byte ptr portalLoc+3], 35
		mov [byte ptr snakeLoc], 31
		mov [byte ptr snakeLoc+1], 19
		mov [byte ptr snakeLoc+2], 32
		mov [byte ptr snakeLoc+3], 19
		mov cx, 200
		mov bx, offset snakeLoc
		lopReset:
			mov [byte ptr bx+4], 0
			inc bx
		loop lopReset
		mov dx, 03c8h
		xor al, al
		mov di, offset palette_backup
		mov cl, 0ffh
		restore_palette_loop2:
		out dx, al
		push ax dx
		mov dx, 03c9h
		mov al, [di]
		out dx, al
		mov al, [di+1]
		out dx, al
		mov al, [di+2]
		out dx, al
		pop dx ax
		add di, 3
		inc al
		dec cl
		jnz restore_palette_loop2
		jmp firstScreen
;-----------------
;----Back To Text Mode----
hi:
mov ah, 0
mov al, 2
int 10h
;-------------------------
exit:
	mov ax, 4c00h
	int 21h
END start


