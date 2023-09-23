import tabula

import pandas as pd

import camelot

tables = tabula.read_pdf("https://declaration.gov.ge/Home/DownloadPdf/132781", pages="all")

tables = camelot.read_pdf("https://declaration.gov.ge/Home/DownloadPdf/132781")
