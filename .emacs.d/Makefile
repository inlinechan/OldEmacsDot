SUBDIRS =\
	anything-config \
	ecb-snap \
	magit \
	emacs-w3m \
	git-emacs \
	cedet-1.0.1 \
	auto-complete-1.3.1

EMACS	:= emacs -Q -batch
EL		:= $(wildcard *.el)
ELC		:= $(EL:.el=.elc)

NUMCPU=$(cat /proc/cpuinfo  | grep "cpu cores" | head -1 | awk '{print $4}')

.PHONY: common common-batch-compile subdirs $(SUBDIRS)

all: common subdirs

$(ELC): %.elc: %.el
	$(EMACS) -f batch-byte-compile $<
	# init.elc cause error
	rm -f init.elc

common: $(ELC)

common-batch-compile:
	$(EMACS) -f batch-byte-compile $(EL)

subdirs: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) auto -j $(NUMCPU) -C $@
