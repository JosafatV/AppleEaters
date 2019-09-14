.global	main

.text
main:
	movl  $4, %eax		# load syscall write
	movl  $1, %ebx		# arg: fd==1: console?
	movl  $msg, %ecx	# arg: buffer to be written
	movl  $len, %edx	# arg: size of buffer
	int   $0x80			# execute syscall

_input:
	movl $3, %eax		# load syscall read
	movl $1, %ebx		# arg: fd
	movl $usrIn, %ecx	# arg: buffer
	movl $1, %edx		# arg: size of buffer
	int $0x80			# execute syscall

_choice:
	movl  $4, %eax		# load syscall write
	movl  $1, %ebx		# arg: fd==1: console?
	movl  $usrIn, %ecx	# arg: buffer to be written
	movl  $1, %edx		# arg: size of buffer
	int   $0x80			# execute syscall

_exit:
	movl  $1, %eax		# load syscall exit
	movl  $0, %ebx		# arg: exit status
	int   $0x80			# execute syscall

.data
msg:
	.asciz  "Want to play? (y/n) \n"
	len =   . - msg

usrIn:
	.asciz "res"
