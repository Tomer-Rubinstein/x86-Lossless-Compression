; TODO:
;	[compress]
;	buildSeqTable
;	buildMinHeap
;	buildEncodedTable (not necessary but can make the outCompFile proc easier)
;	outCompFile


IDEAL
MODEL small
STACK 100h

DATASEG
	filename db 'file.txt', 0
	filename_len dw ?
	filehandle dw ?
	filecontent db 1200 dup(0)
	
	freqTableChars db 256 dup(0)
	freqTableCount db 256 dup(0)

	log_OpenError db 'Error: Program could not open the given file$'

CODESEG
start:
	mov ax, @data
	mov ds, ax

	call buildSeqTable


exit:
	mov ax, 4c00h
	int 21h


; strlen procedure returns the length of the filecontent string in ax
; parameters:
; 	none
; returns:
; - ax, length of the string
proc strlen
	push si
	push bx

	mov ax, 0
	mov si, 0
	strlen_l1:
		mov bl, [(offset filecontent) + si] ; NOT FILENAME!!!!!!!!
		cmp bl, 0
		je strlen_end

		inc ax
		inc si
		
		jmp strlen_l1

	strlen_end:
	pop bx
	pop si
	ret
endp strlen

; all procedurs should be implemented here
proc buildSeqTable
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

	mov di, 0 ; i
	mov si, 0 ; j, freqTableIndex
	call strlen
	mov cx, ax

	l1:
		call strlen ; can use the stack instead of calling strlen over and over again
		mov di, ax
		sub di, cx

		push si

		mov si, 0
		l2:
			call strlen
			cmp si, ax ; can use the stack instead of calling strlen over and over again
			je continue

			mov ax, 0
			mov bx, 0
			mov al, [(offset filecontent) + di]
			mov bl, [(offset freqTableChars) + si]
			cmp al, bl

			je equal

			inc si
			jmp l2

			equal:
				mov ax, [(offset freqTableCount) + si]
				inc ax
				mov [(offset freqTableCount) + si], ax

				pop si

				dec cx
				cmp cx, 0
				jne l1
				; ret
				je hello


		continue:
		pop si
		mov bl, 0
		mov al, 0
		
		mov bl, [(offset filecontent) + di]
		mov [(offset freqTableChars) + si], bl

		mov al, 1
		mov [(offset freqTableCount) + si], al

		inc si
		; loop l1
		dec cx
		jnz l1


	

	openError:
	  mov dx, offset log_OpenError
		mov ah, 9
		int 21h
		jmp exit
	


	; DEBUG
	hello:
	call strlen
	mov cx, ax
	mov si, 0
	l4:
	  mov dl, [(offset freqTableChars) + si]
    mov ah, 2h
    int 21h

		inc si
		dec cx
		jnz l4

	mov dl, 10
  mov ah, 2h
  int 21h

	call strlen
	mov cx, ax
	mov si, 0
	l3:
	  mov dl, [(offset freqTableCount) + si]
		add dl, 48
    mov ah, 2h
    int 21h

		inc si
		dec cx
		jnz l3

	ret

endp buildSeqTable



END start
