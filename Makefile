# Compiling final html report - need to add other code!
report.html: report.Rmd code/render_report.R
	Rscript code/render_report.R
	
# SL Outputs
.PHONY: all_sl
all_sl: output/sl_output/boxplot.rds \
        output/sl_output/table_one.rds \
        output/sl_output/tukey_table.rds

output/sl_output/table_one.rds \
output/sl_output/tukey_table.rds: code/sl_code/01_sl_table.R
	Rscript code/sl_code/01_sl_table.R

output/sl_output/boxplot.rds: code/sl_code/02_sl_plot.R
	Rscript code/sl_code/02_sl_plot.R

# PE Outputs
.PHONY: all_pe
all_pe: output/PE_output/PFbT_anova_tbl.rds \
        output/PE_output/PFbT_sum_tbl.rds \
        output/PE_output/PFbT_boxplot.rds \
        output/PE_output/PFbT_bar.rds

output/PE_output/PFbT_anova_tbl.rds \
output/PE_output/PFbT_sum_tbl.rds \
output/PE_output/PFbT_boxplot.rds \
output/PE_output/PFbT_bar.rds: code/PE_code/01_PE_code.R
	Rscript code/PE_code/01_PE_code.R

#EM outputs
.PHONY: all_em
all_em: output/em_output/Position_AS_barchat.png \
table1_summary.csv

output/em_output/table1_summary.csv: code/em_code/em_code_01.R
	Rscript code/em_code/em_code_01.R
	
output/em_output/Position_AS_barchat.png: code/em_code/em_code_01.R
	Rscript code/em_code/em_code_01.R

# Clean Command - needs to be edited to account for other folders
.PHONY: clean
clean:
	rm -f output/PE_output/*.rds && rm -f report.html \
	&& rm -f output/sl_output/*.rds \
	rm -f output/em_output/*csv && rm -f output/em_output/*.png

# Renv install command - delete #s once ready
#.PHONY: install
#install:
#	Rscript -e "renv::restore(prompt = FALSE)"



#em Outputs
.PHONY: all_em clean_em

all_em: output/em_output/table1_summary.csv \
        output/em_output/chi_square_results.csv \
        output/em_output/chi_square_table.csv \
        output/em_output/Position_AS_barchart.png

output/em_output/table1_summary.csv \
output/em_output/chi_square_results.csv \
output/em_output/chi_square_table.csv \
output/em_output/Position_AS_barchart.png: code/em_code/em_code_01.R
	Rscript code/em_code/em_code_01.R

clean_em:
	rm -rf output/em_output