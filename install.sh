#!/bin/sh
scmdir=$(pkg-config --variable=uim_scmdir uim)
pixmapsdir=$(pkg-config --variable=uim_datadir uim)/pixmaps
cp external-filter.scm external-filter-custom.scm "$scmdir"
cp pixmaps/external-filter.png pixmaps/external-filter_dark_background.png "$pixmapsdir"
cp pixmaps/external-filter.svg pixmaps/external-filter_dark_background.svg "$pixmapsdir"
uim-module-manager --register external-filter
