# UDS Enrolled Report
Generate table summarizing cumulative UDS enrollment

## How to use

Be sure you have [R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/) installed.

### \*Nix Environments (macOS, Linux, Unix)

1. Download the REDCap report titled "UDS Enrollment 3/2017 Table".

2. Place the downloaded csv report into the accompanying `input_csv` folder.

    a. If you didn't download or clone the repository, make sure you also have a folder called `output_csv`.

3. Run the script.

    a. From \*Nix environments, type this command from a terminal 
    
    ```./UDS_Enrolled_Report_table.R ./input_csv/[UMMAPMindsetRegistry_file].csv```,
    
    replacing `[UMMAPMindsetRegristy_file]` with the appropriate file name.

4. An output table (as a csv file) will appear in the `output_csv` folder.

### Windows Environments

*coming soon*