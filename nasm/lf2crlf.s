;
; Under DOS, cat a file from stdin to stdout, replacing LF's with CRLF's.
;

org             0100h
bits            16
cpu             8086

WhileFile:
                call    ReadChar
                jc      Error           ; carry set: file error
                cmp     ax, 0           ; no bytes read: EOF
                je      EndFile

                cmp     byte [buffer], 10
                je      HandleLF

                call    WriteChar
                jc      Error           ; carry set: file error

                jmp     WhileFile

HandleLF:
                mov     byte [buffer], 13
                call    WriteChar
                jc      Error           ; carry set: file error

                mov     byte [buffer], 10
                call    WriteChar
                jc      Error           ; carry set: file error

                jmp     WhileFile

Error:          mov     al, 32
                jmp     GlobalExit

EndFile:        xor     al, al
                ; *fallthrough*

GlobalExit:     mov     ah, 4ch         ; exit to DOS
                int     21h

ReadChar:                               ;;;;;;;;;;;;;;;;;;;;;;;;;;;
                mov     ah, 3fh         ; read data from filehandle
                mov     bx, 0           ; #0 -> stdin
                mov     cx, 1           ; 1 byte
                mov     dx, buffer      ; to buffer
                int     21h
                ret

WriteChar:                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;
                mov     ah, 40h         ; write data to filehandle
                mov     bx, 1           ; #1 -> stdout
                mov     cx, 1           ; 1 byte
                mov     dx, buffer      ; from buffer
                int     21h
                ret

section .bss

buffer:         resb    1
