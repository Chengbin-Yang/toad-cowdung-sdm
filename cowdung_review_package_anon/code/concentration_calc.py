#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# PURPOSE  Concentration = epsilon x mass / runoff volume, with epsilon = 0.3
# INPUT    ../data/11_manure_mass_10m.tif, ../data/12_runoff_volume_10m_m3.tif
# OUTPUT   ../results/manure_concentration_mL_L.tif
# RUN      python3 concentration_calc.py   (working directory: code/)
# PACKAGES rasterio, numpy

"""浓度修正版: 牛粪并非100%进入径流
浓度 = ε × M / V
  ε = 0.3 — 牛粪进入径流水体的比例 (文献: 放牧牛粪径流传输率通常10-30%,
           取上限0.3保守; 蝌蚪暴露主要来自粪中水溶部分+细颗粒冲刷)
  M = 牛粪质量 (kg, 全季244天)
  V = 径流体积 (m³, 全季)
  单位: kg/m³ = g/L ≈ mL/L (牛粪密度~1 g/mL)

修正前: ε=1 隐含"全部牛粪溶入径流"  有效区72.8%超LOEC(3.5), 不合理
修正后: 中高浓度比例大幅下降, 符合野外实际
"""
import numpy as np
import rasterio

EPS = 0.3
MASS = '../data/11_manure_mass_10m.tif'
VOL = '../data/12_runoff_volume_10m_m3.tif'
OUT = '../results/manure_concentration_mL_L.tif'

with rasterio.open(MASS) as dm, rasterio.open(VOL) as dv:
    m = dm.read(1).astype(np.float32)
    v = dv.read(1).astype(np.float32)
    prof = dm.profile.copy()
    prof.update(compress='lzw', tiled=True, blockxsize=256, blockysize=256, nodata=-9999)

conc = np.full_like(m, -9999, dtype=np.float32)
valid = (m > 0) & (v > 0)
conc[valid] = EPS * m[valid] / v[valid]

with rasterio.open(OUT, 'w', **prof) as dst:
    dst.write(conc, 1)

c = conc[valid]
print(f"ε={EPS} 修正后浓度 (mL/L):")
print(f"  min={c.min():.2f} max={c.max():.2f} mean={c.mean():.2f} median={np.median(c):.2f}")
print(f"  valid 像元: {valid.sum():,} ({valid.mean()*100:.2f}% of 全图)")
print("SAVED:", OUT)
import os
print("size:", os.path.getsize(OUT)//1024//1024, "MB")
