AM_CFLAGS = -Wall -std=gnu99 -g -fPIC
AM_LDFLAGS = -ldl -shared -fPIC

bin_PROGRAMS = geocode-emacs.so
lisp_LISP = geocode-emacs.so geocode.el
geocode_emacs_so_SOURCES = geocode-emacs.c

AM_V_CC = @echo "  CC        " $@;
AM_V_CCLD = @echo "  CCLD      " $@;
AM_V_ELC = @echo "  ELC       " $@;

.el.elc: geocode-emacs.so
	$(AM_V_ELC)$(EMACS) -Q -batch \
	-l bytecomp -f byte-compile-refresh-preloaded \
	-f batch-byte-compile $<
