# hexasm

Connect asm and hexl buffers

https://github.com/ebanner/hexasm/assets/2068912/3d321c7a-0fdc-4350-9da8-4b75d8fb61b1

## Features

- hexl address → line number mapping
- line number → hexl address mapping

## How hexasm works

hexasm parses a listing file (output from `nasm` with `-l`) to get a map between source code lines and hex addresses e.g.

```
     1 00000000 B041                    mov al, 'A'
     2                                  loop:
     3 00000002 3C5B                      cmp al, 'Z' + 1
     4 00000004 0F84F801                  je end
     5                                  
     6                                    ;;
     7                                    ;; Make the system call to the BIOS video services
     8                                    ;;
     9 00000008 B40E                      mov ah, 0x0e
    10 0000000A CD10                      int 0x10                      ; looks at `ah` to know which BIOS operation to perform
    11                                  
    12 0000000C FEC0                      inc al
    13                                  
    14 0000000E EBF2                      jmp loop
    15                                  
    16                                  
    17                                  ;;
    18                                  ;; Write enough zeros to get us to the boot sector identifier
    19                                  ;;
    20 00000010 00<rep 1EEh>            times 512-2-($-$$) db 0
    21                                  
    22                                  
    23                                  ;;
    24                                  ;; Write 0x55aa boot sector identifier
    25                                  ;;
    26 000001FE 55AA                    db 0x55, 0xaa
    27                                  
    28                                  end:
```

## Other

I am also experimenting with ways to make my process of OS development more interactive

![image](https://github.com/ebanner/hexasm/assets/2068912/63626b07-c757-4ac6-b5a9-da4851df5f27)

## Related projects

- [ebanner/pynt](https://github.com/ebanner/pynt)
