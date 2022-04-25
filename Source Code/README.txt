###############################################################################################################
Title: A Novel Algorithmic Multiple Trading Strategy Using Data-Driven Random Weights Innovation Volatility
Date: June, 2020
###############################################################################################################

Software: R software, version: r/3.3.1


# Supplementary Data:

	The Supplementary Data file contains three R scripts (.R) and one RMarkdown (.Rmd) files such as:

	a. RATS.aux.R: this contains all auxiliary self written R functions (.R) that will use frequently in this work.
	b. RATS_training.R: this contains the self written R functions (.R) that will use for the training data set used in the paper 
	c. RATS_test.R: this contains the self written R functions (.R) that will use for the test data set used in the paper. 

	d. Evaluation_RATS.Rmd: this file contains RMarkdown files (.Rmd) with html complied version.
	    
	      RMarkdown files: The R-code for the data example is contained in the R Markdown file "Evaluation_RATS.Rmd" 
		   	       (use RStudio for opening/modifying/compiling). 
			       The compiled version is available as an html ("Evaluation_RATS.html"). 
			       The code is well commented and directly evaluated (see "Evaluation_RATS.html"). 
			       To perform the analysis by yourself simply run the code line by line (after having installed all R-packages 
			       that are loaded at the very beginning - if needed and load all the source R scripts (provided in a, b and c).

# Description of steps:

	1. Upload the packages and read/load the data

	2. Load all the source R functions

	3. Used cointegration test for only the training sample

	4. Used the training sample to do the robust pairs trading and traditional pairs trading to determine optimal value pf p $p_{opt}$. 
	   $delta = 0.0001$ and $sigma_{epsilon}^2 = 0.001$ are set to be initial values for the provided example implementation. Initial values can be changed flexibily.

	5. Used the $p_{opt}$ from the training sample to do the robust pairs trading and traditional pairs trading for the test sample.

	  

# Output: 

	Running the RMarkdown files (Evaluation_RATS.Rmd) will produce all the graphs used in the paper and will be saved in the directory where the all the files (.R and .Rmd) are stored and also provides the part of results presented in paper (Table 2, Table 3) and 	supplementary of paper (Table 4).  



# Note:

	For producing the other experiment outputs one needs to change the value of $delta$ ('delta' argument in R function), $sigma_{epsilon}^2 ('Ve' argument in R function).






 







