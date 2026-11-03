#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# PURPOSE  Restrict cattle density to land that can be grazed, following the land-use classes listed in the paper
# INPUT    ../data/10_grazing_mask_30m.tif, ../data/09_gpw_cattle_30m.tif
# OUTPUT   ../results/cattle_density_grazing_30m.tif
# RUN      python3 11_grazing_mask.py   (working directory: code/)
# PACKAGES rasterio, numpy

"""放牧区掩膜 × GPW家牛密度 → 放牧家牛密度栅格"""
import rasterio
import numpy as np

MASK = '../data/10_grazing_mask_30m.tif'
CATTLE = '../data/09_gpw_cattle_30m.tif'
OUT = '../results/cattle_density_grazing_30m.tif'

with rasterio.open(MASK) as dm:
    mask = dm.read(1)
    m_profile = dm.profile.copy()

with rasterio.open(CATTLE) as dc:
    cattle = dc.read(1).astype(np.float32)
    cattle[cattle == dc.nodata] = 0

# 应用掩膜：放牧区保留密度(×0.1)，非放牧区=0
cattle_density = np.zeros(mask.shape, dtype=np.float32)
valid_grazing = (mask == 1) & (cattle > 0)
cattle_density[valid_grazing] = cattle[valid_grazing] * 0.1  # GPW scale factor

# 统计
g_px = (mask == 1).sum()
print(f"放牧区像元: {g_px:,}")
print(f"放牧区有密度值像元: {valid_grazing.sum():,} ({valid_grazing.sum()/g_px*100:.1f}%)")
vals = cattle_density[cattle_density > 0]
print(f"放牧密度 (heads/km²): min={vals.min():.1f} max={vals.max():.1f} mean={vals.mean():.1f} median={np.median(vals):.1f}")

m_profile.update(dtype='float32', count=1, compress='LZW', nodata=0.0)
with rasterio.open(OUT, 'w', **m_profile) as dst:
    dst.write(cattle_density, 1)
print("SAVED:", OUT)
import os
print("size:", os.path.getsize(OUT)//1024, "KB")
