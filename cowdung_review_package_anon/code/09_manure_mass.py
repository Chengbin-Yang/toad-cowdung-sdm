#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# PURPOSE  Seasonal manure mass per 10 m cell, from cattle density and the grazing period
# INPUT    ../data/05_cattle_density_10m.tif
# OUTPUT   ../results/manure_mass_10m.tif
# RUN      python3 09_manure_mass.py   (working directory: code/)
# PACKAGES rasterio, numpy

"""牛粪质量计算 (放牧季 4-11月)
M_i = D_i * A_i * F * T
  D_i: 家牛密度 (heads/km²) — 最终校正版
  A_i: 像元面积 (km²) — 真实地面面积, 经度方向 cos(纬度)修正
  F:   23 kg/头/d (ASAE D384.2)
  T:   244 d (4-11月放牧期)
"""
import numpy as np
import rasterio
import math

DENS = '../data/05_cattle_density_10m.tif'
OUT = '../results/manure_mass_10m.tif'

F = 23.0      # kg/head/day
T = 244.0     # days (Apr-Nov)

with rasterio.open(DENS) as ds:
    d = ds.read(1).astype(np.float32)
    prof = ds.profile.copy()
    res = ds.res[0]          # degrees
    # 行方向(纬度) 1px 米数
    m_per_deg_lat = 111132.0
    # 逐行纬度 → cos 修正 → 经度方向 1px 米数
    rows = np.arange(ds.height)
    lats = ds.transform[5] - (rows + 0.5) * res   # 每行中心纬度 (transform[5] = top)
    cos_lat = np.cos(np.radians(np.clip(lats, -60, 60)))
    # 每行像元面积 km²: (RES*111132) * (RES*111320*cos(lat))
    area_per_row = (res * m_per_deg_lat) * (res * 111320.0 * cos_lat) / 1e6  # km²
    # 广播到全图 (每行相同)
    A = np.broadcast_to(area_per_row[:, None], (ds.height, ds.width)).astype(np.float32)

    # M = D * A * F * T
    M = d * A * F * T
    M = M.astype(np.float32)

    prof.update(compress='lzw', tiled=True, blockxsize=256, blockysize=256, nodata=0.0)
    with rasterio.open(OUT, 'w', **prof) as dst:
        dst.write(M, 1)

# 统计
with rasterio.open(OUT) as ds:
    m = ds.read(1).astype(float)
pos = m[m > 0]
print(f"牛粪质量 (放牧季):")
print(f"  像元数(正值): {len(pos):,}")
print(f"  单像元: min={pos.min():.2f} kg  max={pos.max():.2f} kg  mean={pos.mean():.2f} kg")
print(f"  总产量: {pos.sum()/1e6:.1f} 千吨 ({pos.sum():.2e} kg)")
print(f"  折算: {pos.sum()/1e9:.3f} 百万吨")
print("SAVED:", OUT)
import os
print("size:", os.path.getsize(OUT)//1024//1024, "MB")
