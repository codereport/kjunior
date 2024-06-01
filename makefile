k: a.[ch] makefile
	clang-13 -Os -ok a.c -w -fno-builtin -funsigned-char -fno-unwind-tables -mavx2 -mfma -mpclmul -mbmi2 -nostdlib -s

