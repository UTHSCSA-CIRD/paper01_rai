# paper01_rai
The data analysis portion of the RAI code.

## Usage

If you are preparing a plot, edit `DataVisualization.R` and if you are preparing a table, edit `SummaryTables.R`. Use `dat1`, `dat1subs`, or one of the other objects created by `run.R` as your data-source. If none of them meet your needs, please update `run.R` using the information below. The `v()` function should be used to grab multiple columns of interest. To create the working environment in an interactive console, all you need to do is source `run.R`. You don't even need to do that when adding to an existing script because they should all already source it in the beginning.

In the Espinoza series of branches, the thing to run is ForPoster.R, which should be run as an R-Notebook from within RStudio. If the Word output format is chosen when doing this, there is an additional step to turn it into a presentable final format rather than the default Pandoc styles:

    pandoc -s -S ForPoster.docx --reference-docx ./styletemplate.docx -o Formatted_ForPoster.docx
    
For the AMIA series of branches, the following command will build the poster as an OpenOffice odt file with only a little format tweaking needed afterward:

    R -e "library(rmarkdown); render('ForPoster.R',output_file='AMIA_Frailty.odt',output_format=odt_document(reference_odt='poster_42x60_style.odt',template='./pdefault.opendocument',keep_md=T,pandoc_args=c('--filter','pandoc-crossref')));"

After that there is one manual step, unfortunately, where you have to select each table and set the size of its columns to "Optimal". ~~On the other hand this isn't required for generating content for the poster-- there we just copy-paste the text and some figures from Word, but the tables come from PDF anyway. The way to get PDF tables to behave is to open them from LibreOffice Draw, drag to select entire columns at a time, and use arrow keys to move them closer together as a unit. Draw opens PDFs as if they are vector images in which almost every item is ungrouped.~~

## What file does what.

* `global.R`: Loads/installs all needed libraries. Turns on just-in-time (JIT) compilation for speed. Sets names of output files that will be created by scripts or input files *that are part of the repository* (i.e. metadata... the actual data is maintained separately and your local name/path should be set in your local `config.R`). This script sources `config.R` (see below) and `functions.R`. In turn, `run.R` sources this script, so you don't need to source them both.
* `functions.R`: All the custom functions we use go in this script. **If you create a new function, please put it only in this script.** It is sourced by `global.R`, so you don't need to source it directly. Note especially the `v()` function. Its purpose is to grab groups of columns (or groups of CPT codes) in a modular, standardized manner. If you are manually making lists of CPT codes or column names that is a sign that you should be using v() instead.
* `run.R`: The data import and preparation script. First it sources `global.R`. Then it reads the `inputdata`, `dctfile`, and `cptfile` (as set in `global.R` or your `config.R`) into `dat0`, `dct0`, and `dct1` respectively. `dat0` remains in its original state but a copy named `dat1` is made, and all the data transformations are done to `dat1`. **Sorry to belabor the point, but unless there is a good reason, please do not modify individual variables in files derived from `dat1`-- modify them here so the changes propagate out to the subset files.** If you create a new variable, `dat1` is also the place to do that if possible. If you add new columns to `dat1` somewhere other than the portion of  `run.R` prior to the creation of `dat1subs`, please include a comment about why it was necessary. **When creating new variables in `dat1` please give them names that start with `a_` to distinguish them from variables that were present in the sources.**. `dat1subs` is created from `dat1` by subsetting cases matching various criteria. **If you are doing a (non aggregating, see below) subset of the data, please just add an argument to the `ssply()` call instead of creating another top-level data.frame (or include a comment with your code explaining why not).** `dat2` is created from `dat1` and has only the first NSQIP case for each patient (i.e. it only has unique patients unlike `dat1`). This is an aggregated subset, so it would be complicated to create with `ssply()` so it needs to be a separate top-level data.frame. `dat3` is a subset of `dat2` that has just known Hispanic or non-Hispanic white patients. This is derived from `dat2` rather than `dat1` prior to implementation of `ssply()` but it will probably be better to create a new `ssply()` for `dat2` if/when we come back to needing unique patients. The update of `hispanic_ethnicity` on `dat3` is done because if converted to a factor prior to that point `hispanic_ethnicity` would have a different set of levels and therefore would still need to be re-converted to a factor to get rid of the no-longer-existant levels. `pat_samp` is supposed to be a random but reproducible set of patients, but the random seed is not being initialized correctly. When we start needing this again, in addition to correcting our usage of `set.seed()`, `dat4` created in an `ssply()` call on `dat2` along with the Hispanic/NHW subset instead of being a top-level data.frame.
* (`config.R`): Does not exist in the repo, but each of us should have a local version of it that has the same variables declared as does `example.config.R`. 
* `example.config.R`: Local settings, e.g. paths to raw data files.
* `DataVisualization.R`: 
* `SummaryTables.R`: 
* `ForPoster.R`: Draft of the AMIA 2019 poster if we end up submitting it.
* Non-code
    * `cpt_dictionary.csv`: A table matching CPT codes with any information we need to have associated with them. Currently these are:
        * `dataset_column_names` : a misnomer, put in for compatibility with our function `v()`. What it actually contains is the actual CPT code. When there's time we'll update `v()` to expect a different index column name. 
        * `Procedure`: Human-readable description.
        * `Surgeries`: Total count of this CPT code in NSQIP
        * `c_all_colon`: If `T` then this CPT is a type of colectomy
        * `c_open_colon`: If `T` then this CPT is an open colectomy
        * `c_lapa_colon`: If `T` then this CPT is a laprascopic colectomy
    * `VariableNamesFromUHSNSQIP.csv`: A table matching column names with various metadata. Each row in this table corresponds to a column in `dat0` and its derivatives. This file is read by `run.R` in order to create the `dct0` object. *If you have a non-trivial set of columns that need to be operated on together, please specify it here. If you originally specified those columns in *`dct0`* and you want to preserve the code in case you need to update it, you can save out the modified *`dct0`* to this file and then comment out the code for creating those columns in the script.* Please note that columns that are used to specify new groups of columns in NSQIP need to have names that start with `c_` in order for the v() function to find them. **This is potentially a part of the repo that domain experts who are not programmers might need to review, so it is important that collections of related columns are easily accessible to them, and that is why we have this separate table for them, in addition to making our own day-to-day work easier.** Here are the current columns as of this commit:
        * `dataset_column_names`: Name of the column in `dat0`, `dat1`, etc.
        * `NSQIP_NAMES`: Name of the column in the original ACS NSQIP spreadsheet
        * `c_cd4`: If `T` then this column is considered a Clavien-Dindo level 4 complication.
        * `c_postop`: If `T` then this column is a post-operative complication.
        * `c_canbepatos`: If `T` then this column has a corresponding version that is PATOS (present at time of surgery), and the number in the PATOS version of the column should be subtracted from this one to get the true post-op number of complications (`run.R` already does this). `c_canbepatos` and `c_patos` are mutually exclusive.
        * `c_patos`: If `T` then this *is* the PATOS variant of a `c_canbepatos` column. `c_canbepatos` and `c_patos` are mutually exclusive.
        * `c_date`: If `T` then this is a date column.
        * `c_rai`: If `T` then this column is used in calculating the RAI-A score.
        * `c_noyes`: If `T` then this column has `no`/`yes` values
        * `c_count`: If `T` then this column is a count rather than an indicator.
        * `c_num`: If `T` then this column is (or should be) numeric.
        * `c_tabsie`: If `T` then this column is one of the ones which we currently export to our visualization web-app.
    * `README.md`: The file you are reading!
    * `LICENSE`: The license that says you can use it and modify it as long as you let others do the same to the derivative work you create with it, and you keep this license attached to it.
* The following files are obsolete. Objects they create are not used elsewhere (or need to be removed). They might have some useful comments though. What I try to do in such cases is create a tagged commit or branch for them and then in case we need them later and then delete them from integration.
    * `explore.R`
    * `metadata.R`: This one gets sourced by `global.R` so we need to update `global.R` before removing it.
    * `random_seed.R`: This is a mistake on my part. The goal is reproducible randomization, but sourcing a dump of a previous random seed isn't how it's done turns out. What we do instead is `set.seed(123)` immediately before we call some function that uses randomness (can replace 123 with any integer). Should eventually set it someplace at the beginning, and somehow use several, but let's worry about that later. I plan to remove `random_seed.R` in the integration branch after I also remove the call to it in `run.R`
* The following files are empty. To avoid confusion, let's please delete them and create later as needed when we have specific code to put into them.
    * `DataCleaning.R`:  Also, data cleaning seems like it belongs in `run.R`.
    * `ModelingAssessment.R`
    * `Modeling.R`
    * `Production.R`


## Storage area for useful snippets

* How PanDoc does Word: /usr/bin/pandoc +RTS -K512m -RTS ForPoster.utf8.md --to docx --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output ForPoster.docx --smart --highlight-style tango --reference-docx styletemplate.docx --filter /usr/bin/pandoc-citeproc 
* How PanDoc does PDF: * PDF: /usr/bin/pandoc +RTS -K512m -RTS ForPoster.utf8.md --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output ForPoster.pdf --template /home/a/R/x86_64-pc-linux-gnu-library/3.4/rmarkdown/rmd/latex/default-1.17.0.2.tex --highlight-style tango --latex-engine pdflatex --variable graphics=yes --variable 'geometry:margin=1in' --filter /usr/bin/pandoc-citeproc 
* How PanDoc does HTML: /usr/bin/pandoc +RTS -K512m -RTS ForPoster.utf8.md --to html --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output ForPoster.html --smart --email-obfuscation none --self-contained --standalone --section-divs --template /home/a/R/x86_64-pc-linux-gnu-library/3.4/rmarkdown/rmd/h/default.html --no-highlight --variable highlightjs=1 --variable 'theme:bootstrap' --include-in-header /tmp/Rtmp7wH6Et/rmarkdown-str4b4e79a6a7e7.html --mathjax --variable 'mathjax-url:https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --filter /usr/bin/pandoc-citeproc 

