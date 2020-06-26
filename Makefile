##
## EPITECH PROJECT, 2020
## funEvalExpr
## File description:
## Makefile
##

BIN_PATH = $(shell stack path --local-install-root)

EXEC = funEvalExpr

all:
	stack build
	cp "$(BIN_PATH)"/bin/$(EXEC) .

clean:
	stack clean

fclean:
	stack purge
	rm -f $(EXEC)

re:	fclean all

.PHONY:
	all clean fclean re

.SILENT:
	all clean fclean re
