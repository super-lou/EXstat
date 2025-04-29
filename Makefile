# build package documentation
doc:
	R -e 'devtools::document()'

check:
	R -e 'devtools::check()'

install:
	R -e 'remotes::install_github("louis-heraut/EXstat")'

install-dev:
	R -e 'remotes::install_github("louis-heraut/EXstat@dev")'

help:
	R -e 'help(process_extraction, EXstat, help_type="pdf")'

github_check:
	R -e 'usethis::use_github_action_check_standard()'
