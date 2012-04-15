# Makefile

RM = rm -f

all:
	xmonad --recompile && xmonad --restart


clean:
	${RM} *.o *.hi *.errors
