IDEAL
MODEL small
STACK 100h

; TODO:
;	[compress]
;	buildSeqTable (DONE)
;	buildMinHeap
;	buildEncodedTable
;	outCompFile

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
END start


; strlen procedure returns the length of the filecontent string in ax
; parameters:
; 	none
; returns:
; - ax, length of the string
; [TODO] take string as parameter from the stack
proc strlen
	push si
	push bx

	mov ax, 0
	mov si, 0

	strlen_l1:
		mov bl, [(offset filecontent) + si]
		cmp bl, 0
		je strlen_end

		inc si
		
		jmp strlen_l1

	strlen_end:
	mov ax, si
	pop bx
	pop si
	ret
endp strlen

; buildSeqTable procedure initializes two arrays in the data segment:
; * freqTableChars - which contains all the characters in the given file content
; * freqTableCount - which contains how many times each character appears in the file content
; both arrays are synchronized, meaning, freqTableCounts[i] holds the count for the character at freqTableChars[i] 
; parameters:
; 	none
; return:
;   none
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
		sub di, cx ; converting to ascending indexing (0,1,...)

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

			; current char of filecontent already exists in freqTableChars
			equal:
				mov ax, [(offset freqTableCount) + si]
				inc ax
				mov [(offset freqTableCount) + si], ax

				pop si

				dec cx
				cmp cx, 0
				jne l1
				ret

		; current char doesn't exist in the freqTable
		continue:
		pop si
		mov bl, 0
		mov al, 0
		
		; adding the char to freqTableChars
		mov bl, [(offset filecontent) + di]
		mov [(offset freqTableChars) + si], bl

		; setting the corresponding count
		mov al, 1
		mov [(offset freqTableCount) + si], al

		inc si
		dec cx
		jnz l1


	; log error msg if the file couldn't be opened
	openError:
	  mov dx, offset log_OpenError
		mov ah, 9
		int 21h
		jmp exit

	ret
endp buildSeqTable
