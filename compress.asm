IDEAL
MODEL small
STACK 100h

; TODO:
;	[compress]
;	buildFreqArr (DONE)
;	buildMinHeap
;	buildEncodedTable
;	outCompFile

DATASEG
	filename db ?
	filename_len dw ?
	filehandle dw ?
	filecontent db 1200 dup(0)

	freqArr dw 128 dup(0)

	log_OpenError db 'Error: Program could not open the given file$'
	log_InputFilename db 'File name (include extension): $' 

CODESEG
start:
	mov ax, @data
	mov ds, ax

	call getFilename
	call buildFreqArr

exit:
	mov ax, 4c00h
	int 21h

proc getFilename
	; prompting the user to enter the filename
	mov dx, offset log_InputFilename
  mov ah, 9
  int 21h

	; getting user input until the newline char (13, in ASCII)
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
		add bl, [filecontent+si] ; since freqArr is initialized as DW
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


END start