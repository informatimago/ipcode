all: help
help:
	@echo 'make distribution'

SRC=$(shell basename $$(pwd))
DISTFILES=\
	$(SRC)/README \
	$(SRC)/Makefile \
	$(SRC)/lisp*.sh \
	$(SRC)/*.lisp 

ARCHIVE=ipl-$(shell cd .. ; . $(SRC)/VERSION ; echo $${major}.$${minor}-$${extension}).tar.gz

distrib distribution:access-rights
	@echo SRC=$(SRC)
	@cd .. ; tar -zcf  $(ARCHIVE) $(DISTFILES)
	@cd .. ; tar -ztvf $(ARCHIVE)

access-rights:
	-@chmod -R a+r *

publish:distrib
	@cp ../$(ARCHIVE) /larissa/root/local/html/users/pascal/
	@echo  http://www.informatimago.com/users/pascal/$(ARCHIVE)

