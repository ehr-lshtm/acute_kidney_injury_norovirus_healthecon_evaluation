# EHR group template for R Projects

## Purpose
This is the analysis code for the paper 'cost-effectiveness of potential norovirus vaccination in England: a dynamic model-based health economic evaluation including acute kidney injury outcomes'

## Untracked files
This repository contains only non-disclosive files, that is, code without file paths, and summary statistics. This template is set up so only files that are safe to upload to Github, such as code, are uploaded by default. The underlying data used for this study was from the Clinical Practice Research Datalink, which includes UK Primary Care Data, and linked data such as Hospital Episode Statistics. Access to this data is subject to protocol approval via CPRD’s Research Data Governance (RDG) Process. Given the sensitive nature of these data, this is not uploaded to the repository. Access to data is available on request to the Clinical Practice Research Datalink (https://cprd.com/how-access-cprd-data).

## File tree
We recommend that you use this file structure as the `.gitignore` is set up to ignore specific folders. You can add subfolders as needed, for example for different types of outputs (logs, images, tables), or remove folders that are not used. 


```
template-r/
├── codelists/
│   ├── README.md
│   ├── codelist_1.csv
│   └── codelist_1_metadata.txt
├── data/
│   ├── README.md
│   ├── cleaned_data_1.csv (untracked)
│   └── cleaned_data_2.csv (untracked)
├── docs/
│   ├── README.md
│   ├── document1.docx
│   ├── document1.html
│   └── document1.Rmd
├── paths/
│   ├── README.md
│   └── paths.R (untracked)
├── R/
│   ├── README.md
│   └── functions.R
├── results/
│   ├── README.md
│   └── result_1.csv
├── r-template.Rproj
└── README.md
```

## Publishing the repository
Once you are ready to make the contents of this repository public:
1. Create a new repository, e.g. "study_name_public"
2. Copy the contents of this private repository to the new repository (make sure to copy the .gitignore file, but do not copy the hidden .git folder; what is hidden may vary by operating system)
3. Before publishing the new repository make sure it doesn't contain:
	* any raw data
	* any derrived data (e.g. cleaned intermediate data)
	* any disclosive results
	* any file paths to locations on secure network drives
4. Commit the changes and publish the new repository (e.g. in Github Desktop first "Commit to main", then "Publish repository" and untick "Keep this code private"
5. See [this guide](https://docs.github.com/en/repositories/archiving-a-github-repository/referencing-and-citing-content) on issuing a DOI and making your new public repository citable 
