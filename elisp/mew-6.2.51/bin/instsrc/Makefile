#
# Makefile for Mew installer
#

CC       = cl
CFLAGS   = -nologo -O2 -W3
RC       = rc
LINK     = link

INCLUDES = common.h env.h
LIBS     = kernel32.lib user32.lib gdi32.lib advapi32.lib comdlg32.lib imm32.lib

OBJS     = mew.obj env.obj common.obj
RESS     = mew.res

all : mew.exe

mew.exe : $(OBJS) $(RESS)
	$(LINK) -subsystem:windows -machine:I386 -out:mew.exe $(OBJS) $(LIBS) $(RESS)
mew.res : mew.rc resource.h
	$(RC) mew.rc
mew.obj : mew.c $(INCLUDES) resource.h
	$(CC) $(CFLAGS) -c mew.c
env.obj : env.c $(INCLUDES)
	$(CC) $(CFLAGS) -c env.c
common.obj : common.c $(INCLUDES)
	$(CC) $(CFLAGS) -c common.c
