.PHONY: dotemacs update_hugo
dotemacs:
	make -C .emacs.d

update_hugo:
	cd ~/src/github.com/gohugoio/hugo; \
	git pull; \
	go install

