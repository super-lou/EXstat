# build package documentation
doc:
	R -e 'devtools::document()'

check:
	R -e 'devtools::check()'

install:
	R -e 'remotes::install_github("super-lou/EXstat")'

help:
	R -e 'help(process_extraction, EXstat, help_type="pdf")'
