
all: meld

meld:
	buildapp --asdf-path $(PWD) \
		--load-system cl-meld \
		--eval '(defun main (args) (cl-meld:meld-compile (first args) (second args)))' \
		--entry main \
		--output meld
