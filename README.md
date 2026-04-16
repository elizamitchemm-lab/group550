# Group 2 Midterm Project: NBA

This group project is an analysis of NBA player statistics. 

## How to Build our Report

1. Change your working directory to match the current repository ("group550")

2. Run "make install" in the terminal to ensure all required packages are installed. 

3. Ensure that your organizational structure matches the below. Run "make clean"
in the terminal to ensure everything is set up correctly. 

4. Decide which category you would like to view in the report. Open report.RMD
change the section:"" in the report to the category you would like. Save the report. 

5. Now that your structure is correct and clean, run "make" in the terminal to
run all our code and produce the report.html file showing your category of interest!

6. Open the report.html file and enjoy your report!

## Structure of Repository

`code` folder houses all code required to create outputs shown in our report.

  - `PE_code/` holds code for our analysis of Personal Fouls
  -  `sl_code/` holds code for our analysis of Position and 3-Point Field Goal Attempts
  - `code_em/` holds code for our analysis of All Star Awards by Position
  - `code_mm/` hold code for our analysis of Games Started and 3-Pointers & Free Throws

`output` folder holds all outputs produced by the code. outputs are read in from
this folder into the RMarkdown document. 

  - `PE_output/` holds the outputs of `PE_code`
  - `sl_output/` holds the outputs of `sl_code`
  - `em_output/` holds the outputs of `code_em`
  - `output_mm/` holds the outputs of `code_mm`

`data` folder holds the excel spreadsheet that hold all of the data used
for the project. Data is read into the code and RMarkdown document through that
folder. We also have the NBA codebook spreadsheet in this folder for reference

`Makefile` contains rules for building the final report, as well as a make clean

`Report.RMD` is the RMarkdown report that combines all of the data into a nicely
formatted report

This `README.md` document with instructions

## Categories for the Report
This goes in the yamal header of the report.Rmd

- `all` All categories of interest shown
- `pf`  Only Personal Fouls Per 36 Minutes by Team
- `fg`  Only Position and 3-Point Field Goal Attempts
- `asa` Only All Star Awards by Position
- `games` Only Games Started and 3-Pointers & Free Throws