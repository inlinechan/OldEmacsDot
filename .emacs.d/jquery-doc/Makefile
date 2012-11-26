BATCH=emacs --batch -q -no-site-file \
	--eval="(add-to-list 'load-path \"./../auto-complete-1.3.1\")" \
	--eval="(add-to-list 'load-path \".\")"

.PHONY: all compile update-doc-data update-api
auto: all
all: compile

compile:
	 $(BATCH) -f batch-byte-compile jquery-doc-data.el
	 $(BATCH) -f batch-byte-compile jquery-doc.el

update-doc-data: compile
	$(BATCH) --eval="(require 'jquery-doc)" \
	--eval='(jquery-doc-generate-data "jquery.api.xml")'

update-api:
	curl http://api.jquery.com/api/ > jquery.api.xml
	dos2unix jquery.api.xml
