# 📊 Enron Email Network Analysis — Shiny App

This repository contains a Shiny web application and accompanying analysis exploring the famous **Enron email dataset** from 1999–2002. The project was developed by **Marie-Caroline Bertheau** as part of the *R for Big Data* course.

## 🚀 Project Overview

The Enron dataset contains all email communications between 149 employees during a critical period in the company’s history, including the California energy crisis, executive changes, SEC investigation, and the company’s bankruptcy.

This app allows you to:

✅ Explore the activity of the most active Enron employees
✅ Analyze email flows by domain, status, and organizational group
✅ Visualize communication networks and communities
✅ Examine key content and keywords in the email subjects and bodies
✅ Connect temporal dynamics to public events around the Enron scandal

## 📂 Repository Contents

* **app.R** — The Shiny web application source code
* **Mardown.Rmd** — R Markdown file documenting the analysis and statistical commentary
* **Mardown.html** — Compiled HTML version of the R Markdown file

## 🔧 How to Run

1. Clone this repository:

   ```bash
   git clone https://github.com/yourusername/enron-shiny-app.git
   ```

2. Open `app.R` in RStudio.

3. Click the **Run App** button in RStudio.

✅ Requirements:

* R and RStudio installed
* Required R packages: `shiny`, `shinydashboard`, `dplyr`, `ggplot2`, `knitr`, `kableExtra`, `visNetwork`, `igraph`, `ggrepel`

## 📊 Features

* **Dashboard view**: interactive plots showing global mail flow, domain distribution, status analysis, role-based analysis, and group dynamics.
* **Tables view**: top senders/receivers, external/internal summaries, tension-related keywords.
* **Network view**: interactive email network graph with community detection.
* **Keywords view**: explore email content and subject keywords over time.

## 📚 Background

The dataset covers one of the most turbulent periods in corporate history, offering insights into organizational communication, crisis dynamics, and social networks. Public dataset: [Enron Email Dataset](https://www.cs.cmu.edu/~./enron/)

## ✍️ Author

Marie-Caroline Bertheau

