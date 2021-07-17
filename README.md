### Source code of the Master thesis: 
# A comparison of selected statistical and  machine-learning approaches for spatiotemporal modeling of nitrogen dioxide  across Switzerland: with LUR and satellite-derived data
<img src="https://www.swisstph.ch/typo3temp/assets/_processed_/c/6/csm_Logo_SwissTPH_print_ae656f8658.png" height=60> <img src="https://upload.wikimedia.org/wikipedia/de/f/f4/Albert-Ludwigs-Universit%C3%A4t_Freiburg_2009_logo.svg" height=80>

***
* Master thesis project
* Author: Tze-Li Liu  
* Supervisors: Dr. Kees de Hoogh, Prof. Dr. Carsten Dormann  
* @ Swiss TPH, University of Freiburg
* `README.md`
***

### Folder structure: 
* `./analysis`: analysis-related files
	* `0_DMP`: data management plan and documentation
	* `1_data`
		* `raw`
		* `processed`: the dataset that is ready to be analyzed
	* `2_scripts`
		* the scripts for data collection, processing, and analyses
		* `login-authentication` contains the R scripts for the authentication of the APIs (NASA EarthData, ESA Copernicus Open Access Hub, Earth Observation Group...) for several datasets. To avoid leeking user credentials, these are kept separately are will not be synced to shared drive. `source()` is used in the respective scripts to call authentication. 
	* `3_results`
		* `datasets`
		* `figures`
	* `Masterarbeit.Rproj`

 
