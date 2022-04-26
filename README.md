# Jay yield
Analysis of the "Jay" yield trials conducted between 2019-2021. This is a rework of the analysis I laid out in the [YieldAnalysis_2022 repository](https://github.com/jhgille2/YieldAnalysis_2022). The overall analysis and objectives are still the same but in the couple of years since the project began, I've cleaned up the data considerably and decided to move this relatively clean data into this new repository so that this workflow can be focused more on analysis and summarization and less on the cleaning steps that were necessary and laid out in the other repository. I've also taken steps to make the workflow more streamlined and made a heavier use of the functions from the [metan](https://tiagoolivoto.github.io/metan/index.html) package as I've gotten more comfortable with using it. 

## Directory  
**\_targets.R**: The "make" control script of the workflow **START HERE**  
**R**: Folder to hold the R functions used in the workflow.  
**\_targets**: Automatically generated folder that the targets package uses to control the workflow.  
**data**: Input datasets, separated by year with an extra folder to hold various utility spreadsheets.  
**doc**: Folder to hold write ups/manuscript documents. 
**exports**: Folder to hold plots and formatted tables.   
**graphviz_docs**: Folder to hold graphviz files.  
**sandbox**: Folder to hold in-development r scripts and functions that are not used in the workflow.
