IDEAL
MODEL small
STACK 100h


DATASEG
	filename db 'file.txt', 0 ; debug
	filename_len dw ?
	filehandle dw ?
	filecontent db 1200 dup(0) ; debug

	freqArr dw 128 dup(0)
	freqChars dw 128 dup(0)
	freqCharsCount dw 128 dup(0)

	log_OpenError db '[ERROR] Program could not open the given file$'
	log_InputFilename db 'Filename (include extension): $' 


CODESEG
start:
	mov ax, @data
	mov ds, ax

	call buildFreqArr
	call splitFreqArr
	call findMins

exit:
	mov ax, 4c00h
	int 21h

; [TODO] use buffered input instead
; proc getFilename prompts the user to enter a string representing
;	the filename to be compressed. It stores the input at [filename].
; params: null
; result: [filename] <-- user input
proc getFilename
	; prompting the user to enter the filename
	mov dx, offset log_InputFilename
	mov ah, 9
	int 21h

	; getting user input until the `enter` char (13, in ASCII)
	mov si, 0
	loop_getFilename:
		mov ah, 1
		int 21h

		cmp al, 13
		je end_getFilename

		mov [filename+si], al

		inc si
		jmp loop_getFilename

	end_getFilename:
	ret
endp getFilename

; proc buildFreqArr reads the content of filename (given) and stores
; each char's number of appearences at the corresponding index of that char.
; i.e. if 'a' was found at the filecontent then increment freqArr[97] ('a' = 97).
; params: null
; assumes: filename
; result: [freqArr(dw, 128 cells)] containing the appearences of each char in filecontent.
proc buildFreqArr
	; open file
	mov ah, 3Dh
	xor al, al
	lea dx, [filename]
	int 21h
	jc openError
	mov [filehandle], ax

	; read file and store content in filecontent
	mov ah, 3Fh
	mov bx, [filehandle]
	mov cx, 1200
	mov dx, offset filecontent
	int 21h

	; incrementing the element in freqArr corresponding to the current ASCII char value as index
	; i.e. to increment the appearences of the char 'a', increment freqArr[97] by 1.
	mov si, 0
	loop_genFreqArr:
		mov bx, 0
		mov bl, [filecontent+si]
		; add bl, [filecontent+si] ; since freqArr is initialized as DW
		inc [freqArr+bx]

		inc si
		cmp [filecontent+si], 0 ; read until the null terminator
		jne loop_genFreqArr

	; close file
	mov ah, 3Eh
	mov bx, [filehandle]
	int 21h

	ret

	; log error msg if the file couldn't be opened
	openError:
		mov dx, offset log_OpenError
		mov ah, 9
		int 21h
		jmp exit
	ret
endp buildFreqArr


; proc splitFreqArr "splits" freqArr into 2 arrays:
; * one contains the chars that appeared in filecontent (freqChars, dw, 128 cells)
; * and the second contains the NOA of that chars correspondingly (freqCharsCount, dw, 128 cells)
; params: null
; assumes: freqArr
; result: [freqChars], [freqCharsCount]
proc splitFreqArr
	mov bp, sp

	xor si, si
	xor bx, bx

	splitChars:
		xor cx, cx
		mov cl, [(offset freqArr)+si]
		cmp cx, 0
		jnz addChar

		; loop iteration & condition
		splitCharsIter:
			inc si
			cmp si, 128
			jne splitChars
			je endSplitArr

		addChar:
			; insert the current char to freqChars(dw, 128)
			mov [freqChars+bx], 0
			mov [freqChars+bx], si
			; insert the current NOA of this char to freqCharsCount(dw, 128)
			mov [freqCharsCount+bx], 0
			mov [freqCharsCount+bx], cx
			add bx, 2
			jmp splitCharsIter

	endSplitArr:
	ret
endp splitFreqArr

; proc findMins returns the indexes of the 2 minimal values at freqCharsCount(array).
; params: null
; assumes: freqCharsCount
; returns:
;		* ax - first minimal value's index
;		* cx - second minimal value's index
proc findMins
	mov bp, sp

	xor si, si
	xor bx, bx

	; finding the first minimal value's index from freqCharsCount (array, dw)
	firstMinLoop:
		cmp [freqCharsCount+si], 0
		je endFirstMin

		mov cx, [(offset freqCharsCount)+si]
		mov dx, [(offset freqCharsCount)+bx]

		cmp cx, dx
		jl newFirstMin

		add si, 2 ; for loading dw
		jmp firstMinLoop

	endFirstMin:
		; since si is increasing by 2, divide by 2 to get the wanted index
		; store the first minimal value's index at ax.
		mov ax, bx
		xor bx, bx
		mov bl, 2
		div bl
		mov ah, 0 ; ax - first result


	xor si, si
	xor bx, bx
	; finding the second minimal value's index from freqCharsCount (array, dw)
	secondMinLoop:
		cmp [freqCharsCount+si], 0
		je endSecondMin

		mov cx, [(offset freqCharsCount)+si]
		mov dx, [(offset freqCharsCount)+bx]

		cmp cx, dx

		jl newSecondMin

		add si, 2 ; for loading dw
		jmp secondMinLoop

	endSecondMin:
		; since si is increasing by 2, divide by 2 to get the wanted index
		; store the first minimal value's index at cx.
		push ax
		mov ax, bx
		xor bx, bx
		mov bl, 2
		div bl
		mov ah, 0
		xor cx, cx
		mov cx, ax ; cx - second result
		pop ax


	; ----------
	ret
	newFirstMin:
		; found a new minimal value, update bx and iterate.
		mov bx, si
		add si, 2
		jmp firstMinLoop
	newSecondMin:
		; converting si to normal indexing(byte indexing): si /= 2
		push si
		push ax
		push bx

		mov ax, si
		mov bl, 2
		div bl
		mov ah, 0
		mov si, ax

		pop bx
		pop ax

		cmp si, ax
		pop si ; retrieving si (for word indexing)
		je skipNewMin
		mov bx, si

		; when current_min_index=first_min_index,
		; don't assign current_min_index equal to the second_min_index(result)
		skipNewMin:
			add si, 2
		jmp secondMinLoop
endp findMins


END start
