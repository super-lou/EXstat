# ashes

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
![](https://img.shields.io/github/last-commit/super-lou/ashes)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md) 
<!-- badges: end -->

[<img src="https://github.com/super-lou/MAKAHO/blob/0fad3c354954ebc2d8605a2ec9dd3d4f11a36920/www/screen.png" width="600">](https://makaho.sk8.inrae.fr/)</br>

`ashes` stand for Hydrological Stationarity Analysis of Surface Flows which means Analyse de Stationnarité Hydrologique des Écoulements de Surface in french.

### Installation

For latest development version

``` r
remotes::install_github('super-lou/ashes')
```

### Documentation

Data came from [Hydroportail](https://www.hydro.eaufrance.fr/) and the selection of stations follows the Reference Network for Low Water Monitoring (Réseau de référence pour la surveillance des étiages, [RRSE](https://geo.data.gouv.fr/en/datasets/29819c27c73f29ee1a962450da7c2d49f6e11c15) in french).</br>

This project was carried out for National Research Institute for Agriculture, Food and the Environment (Institut National de Recherche pour l’Agriculture, l’Alimentation et l’Environnement, [INRAE](https://agriculture.gouv.fr/inrae-linstitut-national-de-recherche-pour-lagriculture-lalimentation-et-lenvironnement) in french).


### Help


### FAQ

*I have a question.*

-   **Solution**: Search existing issue list and if no one has a similar question create a new issue.

*I found a bug.*

-   **Good Solution**: Search existing issue list and if no one has reported it create a new issue.
-   **Better Solution**: Along with issue submission provide a minimal reproducible code sample.
-   **Best Solution**: Fix the issue and submit a pull request. This is the fastest way to get a bug fixed.


### Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.






README
	Ash is a toolbox of R code that perfoms a stationarity
	analysis of the low water regime which in french is "analyse
	de stationnarité du régime des étiages". In this
	configuration, this analysis is centered on the Adour-Garonne
	hydrological basin which is located in the south-west part of
	France.

	The plan of a first use is developed below :

	1. In order to manage to use this code, you firstly need to
	install R (more info here : https://www.r-project.org) but
	also for R studio if you want a user-friendly text editor
	(more info here : https://www.rstudio.com/).

	2. Then if you fill confident you can directly jump to part 3.
	Otherwise, open the 'script_install.R' file. This file guide
	you throught the different package you need to install to make
	this toolbox functionnal. You can run this entire code
	(RStudio : Ctrl+Alt+R).

	3. Open the 'main.R' file. This script is your script of
	interaction with the rest of the code. You have to put here
	your settings and execute the entire file when you are ready.
	(RStudio : Ctrl+Alt+R) It is possible that some packages will
	be missing. Take the time to install them before re-execute
	the 'main.R' file.

	4. Figures will be stored in the 'figures' repository and
	results in the 'results' repository. All the rest of the code
	is located in the 'Rcode' repository and is divided between
	'processing' and 'plotting'. The 'data' repository is where
	you can add, remove or modify data you want to analyse and the
	'resources' repository is for fixed 'resources' that the code
	needed.


CONTACT
	If you have problems, questions, ideas or suggestions, please
	contact us. Contact first Louis Héraut who is the main
	developer. If it is not possible, Éric Sauquet is the main
	referent at INRAE to contact :


GIT
	To download the very latest source off the GIT server do this:
	git clone https://gitlab.irstea.fr/louis.heraut/ash.git

	(you will get a directory named ash created, filled with the
	source code)


NOTICE
	Feel free to use all the code or only some parts but it would
	be nice to at least mention the name of the authors.
	
	Ash stand for "Analyse de Stationnarité Hydrologique".
