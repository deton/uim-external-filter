#!/bin/sh
srcdir=$(dirname $0)
scmdir=$(pkg-config --variable=uim_scmdir uim)
pixmapsdir=$(pkg-config --variable=uim_datadir uim)/pixmaps
cp "$srcdir/external-filter.scm" "$srcdir/external-filter-custom.scm" "$scmdir"
cp "$srcdir/pixmaps/external-filter.png" "$srcdir/pixmaps/external-filter_dark_background.png" "$pixmapsdir"
cp "$srcdir/pixmaps/external-filter.svg" "$srcdir/pixmaps/external-filter_dark_background.svg" "$pixmapsdir"
uim-module-manager --register external-filter
