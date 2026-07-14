# Chestnut Ridge — Marketing Analytics Case Study (R)

> Customer-segmentation study for **Chestnut Ridge** retail using hierarchical + K-means clustering in **R**, followed by a **GE-matrix** segment-attractiveness evaluation and strategic recommendations.
>
> **Marketing Analytics**

---

## 🎯 Business Question

Chestnut Ridge wants to sharpen its retail positioning. Based on 200 customer responses covering store attributes and demographics, **how many meaningful segments exist, and which are the most attractive to target?**

## 📦 Dataset

- **Source:** `retailer.csv` — 200 respondents × 9 variables
- **Store attributes (6):** variety of choice, electronics, furniture, quality of service, low prices, return policy (all 1–10 scales)
- **Demographics:** income (thousands USD), age
- No missing values

## 🧪 Methodology

The analysis walks through 15 sequenced tasks (see report):

1. **EDA** — descriptive stats + histograms for each variable
2. **Normalisation** — z-score scaling
3. **Distance matrix** — Euclidean
4. **Hierarchical clustering** — Ward.D2 with dendrogram
5. **K-means** — 3-cluster and 4-cluster solutions
6. **Cluster-count validation** — `NbClust` (30 indices), silhouette, WSS elbow
7. **Segment profiling** — attribute means + demographic means + radar chart
8. **Segment attractiveness** — GE matrix (market attractiveness × competitive strength)
9. **Strategic recommendations** per segment

## 📈 Key Findings

- **4-segment** solution selected as the actionable business view (3 clusters win on statistical majority rule; 4 clusters give a strategically richer segmentation — trade-off discussed in the report)
- Segments differentiate on **price-sensitivity vs service-quality** and **age / income**
- GE-matrix ranking highlights the priority segment for near-term marketing spend

*Full radar charts, silhouette plots and per-segment recommendations are in the report.*

## 🧰 Tech Stack

`R` · `cluster` · `factoextra` · `NbClust` · `flexclust` · `ggplot2` · `dplyr` · `tidyverse` · `fmsb` (radar charts)

## 📁 Repo Structure

```text
.
├── R/
│   └── chestnut_ridge_analysis.R
├── data/
│   └── retailer.csv
├── docs/
│   └── chestnut_ridge_case_study_report.pdf
├── outputs/            # dendrogram.png, radar.png, segment_profiles.csv
└── README.md
```

## ▶️ Reproduce

```r
# from R / RStudio
install.packages(c("cluster","factoextra","NbClust","flexclust",
                   "ggplot2","dplyr","tidyverse","fmsb"))
source("R/chestnut_ridge_analysis.R")
```

## 📄 Report

Full 15-task report (with GE matrix + recommendations) → [`docs/chestnut_ridge_case_study_report.pdf`](docs/chestnut_ridge_case_study_report.pdf)

---

<sub>MIT-licensed · Author: [Parth Badiger](https://github.com/parthbadiger24-creator)</sub>
