# QUEST

QUEST is a computational method for uncertainty-aware spatial domain detection in spatial transcriptomics. It provides probabilistic domain assignments and location-level uncertainty estimates, enabling the identification of ambiguous regions such as domain boundaries and heterogeneous subdomains.

The method is implemented as an open-source R package with efficient C++ acceleration via Rcpp.

<p align="center">
  <img src="figures/quest_scheme.png" width="800">
</p>

---

## 📦 Installation

You can install the latest development version of QUEST from GitHub:

```r
# install.packages("devtools")
devtools::install_github("YanlinTong/QUEST")
