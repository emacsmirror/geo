lisp_LISP = geo.el geo-fallback.el geo-ip.el \
            geo-xdg.el geo-solar.el geo-nm.el \
            geo-xdg-solar.el geo-simulate.el

AM_V_ELC = @echo "  ELC       " $@;

.el.elc:
	$(AM_V_ELC)$(EMACS) -Q -batch \
	-l ./compload.el -f byte-compile-refresh-preloaded \
	-f batch-byte-compile $< &> /dev/null
