IDEAL
MODEL small
STACK 100h


DATASEG
	filename db 'compress.hf', 0 ; debug
	inputFilename db 'file.txt', 0 ; debug
	outputFilename db 'output.txt', 0 ; debug
	filename_len dw ?
	filehandle dw ?
	newFilehandle dw ?
	filecontent db 1400 dup(0) ; debug

	freqArr dw 128 dup(0)
	freqChars dw 128 dup(0)
	freqCharsCount dw 128 dup(-1)

	parentCount db 128
	blockSize db 8
	codebook db 2048 dup(2)

	log_OpenError db '[ERROR] Program could not open the given file$'
	log_InputFilename db 'Filename (include extension): $'
	byteToWrite db 0
	bitsCount db 0


CODESEG
start:
	mov ax, @data
	mov ds, ax

	call compress

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

		mov [inputFilename+si], al

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
	lea dx, [inputFilename]
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


; proc findMins returns the indexes of the 2 minimal values at freqCharsCount(word array).
; params: null
; assumes: freqCharsCount
; returns:
;		* ax - first minimal value's index
;		* cx - second minimal value's index
proc findMins
	mov bp, sp

	xor si, si
	xor bx, bx
	xor di, di

	firstMinLoop:
		cmp [freqCharsCount+si], -1 ; end-of-freqCharsCount
		je endFirstMin
	
		mov cx, [freqCharsCount+si]
		mov dx, [freqCharsCount+bx]

		cmp cx, dx
		jl newFirstMin

		add si, 2 ; word indexing
		jmp firstMinLoop
	endFirstMin:

	; bx now holds the minimal value's index
	cmp di, 0
	je firstResult
	jne secondResult

	firstResult:
		mov ax, [freqCharsCount+bx]
		push ax
		mov ax, bx ; ⟹ ax - first result
		mov [freqCharsCount+bx], 199Ah ; max value
		; find the second minimal value
		xor si, si
		xor bx, bx
		inc di
		jmp firstMinLoop

	secondResult:
		mov cx, bx ; ⟹ cx - second result
		; retrieve the first minimal value to freqCharsCount
		pop dx
		mov si, ax
		mov [freqCharsCount+si], dx

	ret
	newFirstMin:
		mov bx, si
		add si, 2
		jmp firstMinLoop
endp findMins


; proc addCells sums 2 cells in freqCharsCount by given indexes(params)
; and zeros the grater index cell.
; It also changes freqChars by zeroing the grater index, and incrementing parent count & replace (parentCount).
; params:
;	* [bp+2], the first index
;	* [bp+4], the second index
; assumes: [freqChars], [freqCharsCount]
; returns: null
proc addCells
	mov bp, sp

	mov bx, [bp+2] ; i
	mov si, [bp+4] ; j

	; make it so: i < j
	cmp bx, si
	je exitAddCells
	jl skipSwap
	; i > j, swap
	mov bx, [bp+4] ; i
	mov si, [bp+2] ; j

	skipSwap:
	; if we are summing a parent node(s)
	cmp [freqChars+bx], 128
	jge incParentCount

	cmp [freqChars+si], 128
	jge incParentCount

	; sum freqCharsCount cells
	mov dx, [freqCharsCount+si]
	add [freqCharsCount+bx], dx
	mov [freqCharsCount+si], 199Ah

	; change freqChars
	mov [freqChars+si], 0
	xor dx, dx
	mov dl, [parentCount]
	mov [freqChars+bx], dx
	; inc [parentCount]

	jmp exitAddCells
	incParentCount:
		; inc [parentCount]
		xor dx, dx
		mov dl, [parentCount]
		mov [freqChars+bx], dx
		mov [freqChars+si], 0
		mov dx, [freqCharsCount+si]
		add [freqCharsCount+bx], dx
		mov [freqCharsCount+si], 199Ah
		jmp exitAddCells

	exitAddCells:
	ret 4
endp addCells


; proc insertCodebook inserts/updates a (so called) data block into the codebook.
; data block := {
;		1 byte (ACSII char) ; 12 bytes (reversed huffman code) ; 1 byte (parent)
; } 
; * ASCII char -  represents the encoded char
; * reversed huffman code - the huffman code generated in reversed order
; * parent - the parent of that "node" (data block)
; params:
;	* [bp+2], parent no.
;	* [bp+4], result bit (to insert to the huffman code)
;	* [bp+6], the (ASCII) char huffman algorithm encodes in the process
; assumes: [codebook], [blockSize] (size of the huffman code)
; result: [codebook], As mentioned above.
proc insertCodebook
	mov bp, sp

	mov ax, [bp+2] ; new parentCount = al
	xor ah, ah
	mov bx, [bp+4] ; result bit (0 or 1) = ah
	mov ah, bl
	xor bx, bx
	mov bx, [bp+6] ; (ASCII) char = bl

	cmp bl, 80h
	jae incChilds

	xor si, si
	searchCodebook:
		cmp si, 2048
		jge initDataBlock ; else init a new data block
		cmp [codebook+si], bl
		je updateDataBlock ; if char in codebook, update data block

		xor dx, dx
		mov dl, [blockSize]

		add si, dx
		add si, 2
		jmp searchCodebook

	updateDataBlock:
	push si ; head of data block
	updateDataBlockLoop:
		cmp [codebook+si], 2 ; only 12 bit-space
		jne udb_iteration
		mov [codebook+si], ah ; insert the new result bit

		; going to the parentCount cell's index
		pop si
		xor dx, dx
		mov dl, [blockSize]
		add si, dx
		add si, 1

		; replacing the old parentCount with the new one
		mov [codebook+si], al

		jmp end_insertCodebook
		udb_iteration:
			inc si
			jmp updateDataBlockLoop

	jmp end_insertCodebook
	; ----------
	initDataBlock:
	xor si, si

	; finding the first available data block to insert to
	searchPlace:
		cmp [codebook+si], 2
		je insertDataBlock

		xor dx, dx
		mov dl, [blockSize]

		add si, dx
		add si, 2
		jmp searchPlace

	insertDataBlock:
		mov [codebook+si], bl ; char
		inc si
		mov [codebook+si], ah ; result bit

		xor dx, dx
		mov dl, [blockSize]

		add si, dx
		mov [codebook+si], al ; new parent count

	end_insertCodebook:
	ret 6
	; ----------
	incChilds:
		mov si, -1
		incChildsLoop:
			xor dx, dx
			mov dl, [blockSize]

			; looping through the parents of each data block
			add si, dx
			add si, 2

			; end of codebook
			cmp si, 2048
			jge end_insertCodebook

			; found a match, add the result bit to the huffman code of that data block
			cmp [codebook+si], bl
			je addResultBit
			
			jmp incChildsLoop

	; found a match for the parent, inserting the result bit in the correct cell of the result huffman code.
	addResultBit:
		push si ; saving si to search for other matches later on
		mov [codebook+si], al

		; iterating backwards (dec index) in the codebook
		addResultBitLoop:
			cmp [codebook+si-1], 2 ; searching for an available cell
			je ARB_iter

			; found an available cell
			mov [codebook+si], ah ; insert the result bit
			pop si
			jmp incChildsLoop ; searching for other matches...
			
			ARB_iter:
			dec si
			jmp addResultBitLoop
endp insertCodebook


; proc getFreqLength returns the number of non-nullable chars in the frequency table.
; It does that by rather using the [freqCharsCount] array instead of the [freqChars].
; params: null
; assumes: [freqCharsCount]
; returns: dx - the wanted length
proc getFreqLength
	mov bp, sp

	xor dx, dx
	xor si, si
	GFL_loop:
		cmp [freqCharsCount+si], -1
		je GFL_end

		cmp [freqCharsCount+si], 199Ah
		je GFL_iter

		inc dx

		GFL_iter:
			add si, 2
			jmp GFL_loop

	GFL_end:
	ret
endp getFreqLength


; proc buildCodebook generates the huffman codebook for the given file.
; This procedure is the "conductor" of this project, it builds the codebook using the other needed procedures.
; params: null
; assumes: [codebook], [freqCharsCount], [freqChars], [parentCount], proc getFreqLength, proc findMins, proc insertCodebook, proc addCells
; result: [codebook], the wanted huffman codebook
proc buildCodebook
	mov bp, sp

	call buildFreqArr
	call splitFreqArr

	BC_loop:
		; until 1 node is left
		call getFreqLength
		cmp dx, 1
		je end_buildCodebook

		; finding the two minimums
		call findMins
		mov bx, ax ; i
		mov si, cx ; j

		; inserting to the codebook
		; finding the cell with the higher frequency and assign it's index to si
		; the other cell goes to bx
		mov dx, [freqCharsCount+bx]
		cmp dx, [freqCharsCount+si]
		jle BC_loop_continue
		mov dx, bx ; tmp
		mov bx, si
		mov si, dx

		BC_loop_continue:
		push ax
		push bx
		push cx

		; bx gets 0 as result bit
		push si
		mov ax, [freqChars+bx]
		xor bx, bx
		mov bx, 0
		xor cx, cx
		mov cl, [parentCount]
		push ax ; [bp+6] = bl = char
		push bx ; [bp+4] = ah = bit
		push cx ; [bp+2] = al = count
		call insertCodebook
		pop si

		; si gets 1 as result bit
		mov ax, [freqChars+si]
		xor bx, bx
		mov bx, 1
		xor cx, cx
		mov cl, [parentCount]
		push ax ; [bp+6] = bl = char
		push bx ; [bp+4] = ah = bit
		push cx ; [bp+2] = al = count
		call insertCodebook

		; adding the cells
		pop cx
		pop bx
		pop ax
		push ax
		push cx
		call addCells
		inc [parentCount]

		jmp BC_loop
	end_buildCodebook:

	ret
endp buildCodebook


; proc tidyCodebook reverses all of the huffman codes of each char in the codebook
; since the compression process inserts the huffman codes in reversed order.
; params: null
; assumes: [codebook], [blockSize]
; result: reversed huffman codes in [codebook]
proc tidyCodebook
	mov bp, sp

	; reverse the huffman code of each data block
	xor si, si
	l1:
		cmp [codebook+si], 2
		je end_reverseHuffman

		mov bx, si
		; find the index tail of the huffman code = bx
		l2:
			cmp [codebook+bx+1], 2
			je end_l2

			inc bx
			jmp l2
		end_l2:
		push si
		inc si
		; si < bx
		l3:
			cmp si, bx
			jge end_l3

			mov al, [codebook+bx]
			mov ah, [codebook+si]
			mov [codebook+bx], ah
			mov [codebook+si], al

			inc si
			dec bx
			jmp l3
		
		end_l3:
		pop si

		xor ax, ax
		mov al, [blockSize]
		add si, ax
		add si, 2
		jmp l1

	end_reverseHuffman:
	ret
endp tidyCodebook


; proc outputByte outputs a single byte, [byteToWrite] to the compressed file
; params: null
; assumes: [newFilehandle], [byteToWrite]
; result: [byteToWrite] >> [newFilehandle] (compressed file)
proc outputByte
	mov bp, sp

	; output [byteToWrite] to the compressed file
	mov ah, 40h
	mov bx, [newFilehandle]
	mov cx, 1
	mov dx, offset byteToWrite
	int 21h

	ret
endp outputByte


; proc outputHuffmanCode outputs the huffman codes (in the same order as input filecontent)
; to the compressed file.
; params: [bp+2], char to output it's huffman code to the output file
; assumes: [filecontent], [codebook], [blockSize], proc outputByte
proc outputHuffmanCode
	mov bp, sp
	mov cx, [bp+2] ; cl - holds the char

	xor si, si ; index

	; lookup cl char in the codebook
	xor bx, bx
	t2:
		cmp [codebook+bx], cl
		je end_t2

		add bx, 2
		add bl, [blockSize]
		jmp t2
	end_t2:

	; now read the huffman code and append to [byteToWrite]
	inc bx
	t3:
		cmp [bitsCount], 8 ; byteToWrite is full
		je writeByte
		cmp [codebook+bx], 2
		je end_t3 ; done writing the huffman code to [byteToWrite]

		; append huffman code bit-by-bit to [byteToWrite]
		mov al, [codebook+bx]
		shl [byteToWrite], 1
		add [byteToWrite], al

		inc [bitsCount] ; bits count
		inc bx
		jmp t3
	end_t3:

	ret 2
	writeByte:
		push bx
		call outputByte
		mov [byteToWrite], 0
		mov [bitsCount], 0
		pop bx
		jmp t3
endp outputHuffmanCode


; proc outputCodebooks outputs the codebook to the output file, as following:
; |(char as BYTE) (huffman code of that char as BYTE)| ...
; params: null
; assumes: [codebook], proc outputHuffmanCode
proc outputCodebook
	mov bp, sp

	xor bx, bx
	oc_loop:
		xor ch, ch
		mov cl, [codebook+bx]
		mov [byteToWrite], cl
		push bx
		push cx
		call outputByte ; write char as *byte* to output file
		mov [bitsCount], 0
		mov [byteToWrite], 0
		call outputHuffmanCode
		
		cmp [bitsCount], 0
		je skip_output_byte
		call outputByte

		skip_output_byte:
		mov [byteToWrite], 0
		; skip to the next char in the codebook
		pop bx
		add bx, 2
		add bl, [blockSize]
		cmp [codebook+bx], 2
		jne oc_loop

	ret
endp outputCodebook


proc compress
	mov bp, sp

	call buildFreqArr
	call splitFreqArr
	call buildCodebook
	call tidyCodebook

	; create new file named after [filename]
	mov ah, 3Ch
	xor cx, cx
	mov dx, offset filename
	int 21h
	mov [newFilehandle], ax	; save new file handle

	; output length of codebook
	xor bx, bx
	xor si, si
	lc_loop:
		cmp [codebook+si], 2
		je end_lc_loop

		inc bx

		add si, 2
		xor dx, dx
		mov dl, [blockSize]
		add si, dx
		jmp lc_loop
	end_lc_loop:
	; bx now holds the number of different chars in the given text
	mov ax, bx
	xor bx, bx
	mov bl, 2
	mul bl
	; ax now holds the length of the codebook
	mov [byteToWrite], al
	call outputByte

	call outputCodebook

	; output huffman codes
	mov [bitsCount], 0
	mov [byteToWrite], 0
	xor si, si
	ofc_loop:
		xor cx, cx
		mov cl, [filecontent+si]
		push si
		push cx
		call outputHuffmanCode
		pop si
		inc si
		cmp [filecontent+si], 0
		jne ofc_loop
	; output the final byte
	mov ax, 8
	sub al, [bitsCount] ; bits count
	_finalByte:
		shl [byteToWrite], 1
		dec ax
		cmp ax, 0
		jne _finalByte
	call outputByte

	; close the file handles
	mov ah, 3Eh
	mov bx, [newFilehandle]
	int 21h

	mov ah, 3Eh
	mov bx, [filehandle]
	int 21h

	ret
endp compress


proc findPattern
	mov bp, sp

	mov ax, [bp+2]
	
	xor si, si
	fp_loop:
		cmp [codebook+si+1], al
		je found
		cmp [codebook+si], 2 ; end-of-codebook
		je notFound

		xor bx, bx
		mov bl, [blockSize]
		add si, bx

		add si, 2
		jmp fp_loop

	found:
		xor ax, ax
		mov al, [codebook+si]
	ret 2
	notFound:
		mov ax, 0
	ret 2
endp findPattern


proc decompress
	mov bp, sp

	; create new file named after [outputFilename]
	mov ah, 3Ch
	xor cx, cx
	mov dx, offset outputFilename
	int 21h
	mov [newFilehandle], ax	; save new file handle

	; open file
	mov ah, 3Dh
	xor al, al
	lea dx, [filename]
	int 21h
	jnc continue_decompress
	
	; log error msg if the file couldn't be opened
	mov dx, offset log_OpenError
	mov ah, 9
	int 21h
	mov ax, 4c00h
	int 21h

	continue_decompress:
	mov [filehandle], ax

	; read file and store content in filecontent
	mov ah, 3Fh
	mov bx, [filehandle]
	mov cx, 1200
	mov dx, offset filecontent
	int 21h

	xor dx, dx
	mov dl, [filecontent+0] ; dl := length[codebook]
	inc dl ; for 1..n indexing

	xor si, si
	inc si ; for reading the codebook
	; insert the codebook from the file to [codebook]
	df_loop:
		cmp si, dx ; end-of-codebook
		jae end_df_loop
		push dx
		push si

		xor ax, ax
		xor bx, bx
		xor cx, cx
		mov al, [filecontent+si]
		mov bl, [filecontent+si+1]
		mov cl, [parentCount]
		push ax
		push bx
		push cx
		call insertCodebook

		pop si
		pop dx
		add si, 2
		jmp df_loop
	end_df_loop:

	; although si=start-of-huffman, for safety use, re-calculate
	; the start index of the huffman code
	xor bx, bx
	mov bl, [filecontent+0]
	inc bx
	xor cx, cx ; curr pattern
	dc_loop:
		mov al, [filecontent+bx]
		cmp al, 5 ; EOF
		je end_dc_loop

		; lookup current huffman code
		xor dx, dx ; bit count
		f1:
			cmp dx, 8 ; done with this byte
			je end_f1
			; [TODO]
			; cmp si, ORG_FILECONTENT_LENGTH
			; je end_dc_loop

			; add 1 or 0 to the LSB of cx
			shl cx, 1
			clc
			shl al, 1
			push ax
			jnc f1_continue
			
			add cx, 1
			f1_continue:
				push bx
				push dx
				push cx

				; lookup the pattern cx represents in the codebook
				push cx
				call findPattern

				; pattern not found, need more bits to determine
				cmp ax, 0
				jz f1_iter

				; pattern found, output to the result file
				mov [byteToWrite], al
				call outputByte
				; search for a new pattern
				pop cx
				xor cx, cx
				jmp keep_cx
			f1_iter:
				pop cx
				keep_cx:
				pop dx
				pop bx
				pop ax

				inc dx
				jmp f1
		end_f1:
		inc bx
		jmp dc_loop
	end_dc_loop:

	ret
endp decompress

END start