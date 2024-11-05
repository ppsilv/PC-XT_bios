.DELETE_ON_ERROR:
.PHONY: print_name clean fclean re
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules
.RECIPEPREFIX =

SHELL := zsh
SRC_EXTENSION= asm
SRCS = $(wildcard *.$(SRC_EXTENSION))
TMPS = $(patsubst %.$(SRC_EXTENSION), %.tmp, $(SRCS))
OBJS = $(patsubst %.tmp, %.o, $(TMPS))
HEADERS = \
   -imacros sys/socket.h \
   -imacros sys/syscall.h \
   -imacros unistd.h
#	cpp -w -P $(HEADERS) $^ | sed '/^[[:space:]]*$$/d' > $@

   #end
AS  = nasm
ASFLAGS += -O9 -f obj
LINKER = cc
LDFLAGS +=  -static -nostdlib -mno-red-zone
NAME =  main

all:  $(NAME)

$(NAME): $(OBJS)
	$(LINKER) $(LDFLAGS) $(OBJS) -o $@

%.o:  %.asm
	${AS} ${ASFLAGS} -o $@ $^

.SECONDARY: $(wildcard *.tmp)

#%.tmp:  %.asm
#	cpp -w -P  $^  > $@

print_name:
	@echo $(NAME)

clean:
	rm -rf *.o(N) *.tmp(N)

fclean:  clean
	rm -f ${NAME}

re:	fclean all