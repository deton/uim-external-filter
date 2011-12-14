#!/bin/sh
scmdir=$(pkg-config --variable=uim_scmdir uim)
pixmapsdir=$(pkg-config --variable=uim_datadir uim)/pixmaps
cp selection-filter.scm selection-filter-custom.scm "$scmdir"
cp pixmaps/selection-filter.png pixmaps/selection-filter_dark_background.png "$pixmapsdir"
cp pixmaps/selection-filter.svg pixmaps/selection-filter_dark_background.svg "$pixmapsdir"
uim-module-manager --register selection-filter
