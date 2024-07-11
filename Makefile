LISP ?= ros run --

ncurses:
	qlot install
	$(LISP) --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp --load scripts/build-ncurses.lisp

sdl2:
	qlot install
	$(LISP) --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp --load scripts/build-sdl2.lisp

sdl2-ncurses:
	qlot install
	$(LISP) --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp --load scripts/build-sdl2-ncurses.lisp

test:
	qlot install
	.qlot/bin/rove lem-tests.asd

doc:
	qlot install
	$(LISP) --noinform --no-sysinit --no-userinit --load .qlot/setup.lisp --load scripts/generate-documentation-tests.lisp --eval '(progn (lem-documentation-mode/tests::generate-markdown-file "test.md" :test) (quit))'

update:
	git pull
	qlot install

lint:
	.qlot/bin/sblint lem.asd
	.qlot/bin/sblint lib/lisp-syntax/lem-lisp-syntax.asd
	.qlot/bin/sblint extensions/lisp-mode/lem-lisp-mode.asd
