# OpjPeakAnalyzer

**An interactive Shiny application for processing .opj files, performing peak analysis, and visualizing results with dynamic plots.**

## Demo

![ezgif-1-fd7d6c11b2](https://github.com/user-attachments/assets/25b5e736-f402-447e-9aaf-6d31aa92bdc5)


## Features

- **Batch File Upload:** Upload multiple `.opj` files simultaneously for efficient processing.
- **Dynamic Column Selection:** Easily select `X` and `Y` columns from your data for analysis.
- **Data Cleaning:** Automatically removes phantom coordinates (outliers) based on median and Median Absolute Deviation (MAD) without user intervention.
- **Peak Analysis:** Identifies and counts peaks in the `Y`-axis data using customizable parameters such as minimum peak height, distance between peaks, and threshold.
- **Comprehensive Visualization:** Generates clear and informative plots for each processed file, highlighting detected peaks.
- **Error Handling:** Skips corrupted or improperly formatted files and notifies users of any issues.
- **Results Export:** Download analysis results in `.csv` format for further use or record-keeping.

## Installation

### Prerequisites

- [R](https://www.r-project.org/) (version 4.0 or higher)
- [RStudio](https://rstudio.com/) (optional but recommended)

### Clone the Repository


```bash
git clone https://github.com/flawedme/OpjPeakAnalyzer.git
```

### Navigate to the Project Directory

```bash
cd OpjPeakAnalyzer
```

### Install Required Packages
Open R or RStudio and run the following command to install necessary packages:
```bash
install.packages(c("shiny", "Ropj", "dplyr", "pracma", "DT", "ggplot2"))
```

## Usage
### Launch the Application
In R or RStudio, set the working directory to the project folder and run:

```bash
shiny::runApp("app.R")
```
