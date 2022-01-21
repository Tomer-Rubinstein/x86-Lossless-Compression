IDEAL
MODEL small
STACK 100h

; TODO:
;	[compress]
;	buildFreqArr
;	buildMinHeap
;	buildEncodedTable
;	outCompFile

DATASEG
	filename db 'file.txt', 0 ; [TODO] take filename as input
	filename_len dw ?
	filehandle dw ?
	filecontent db 1200 dup(0)

	freqArr dw 128 dup(0)

	log_OpenError db 'Error: Program could not open the given file$'

CODESEG
start:
	mov ax, @data
	mov ds, ax

	call buildFreqArr

exit:
	mov ax, 4c00h
	int 21h


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

	mov si, 0
	loop_genFreqArr:
		mov bx, 0
		mov bl, [filecontent+si]
		add bl, [filecontent+si] ; since freqArr is initialized as DW
		inc [freqArr+bx]

		inc si
		cmp [filecontent+si], 0
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