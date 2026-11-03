#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# PURPOSE  Seasonal runoff volume per 10 m cell from land use, slope and precipitation
# INPUT    ../data/14_landuse_10m.tif (land cover, third party, not included) plus the climate layers used below
# OUTPUT   ../results/runoff_volume_10m_m3.tif
# RUN      python3 10_runoff_volume.py   (working directory: code/)
# PACKAGES rasterio, numpy

"""放牧季径流计算 10m 版 — 用 GLC_FCS10 土地利用 (师兄提供 landuse_0804.tif)
R_i = Σ_{m=4}^{11} (P_m × C_i)    # 累计径流深度 mm
V_i = R_i / 1000 × S_i            # 径流体积 m³, S_i 真实面积

GLC_FCS10 编码 → 径流系数映射 (基于放牧掩膜编码表 + ESSD 2025 分类):
  10,11  针叶林 (Evergreen/Deciduous needleleaf)  → 森林 0.15
  20     阔叶林 (Broadleaf)                        → 森林 0.15
  61,62  灌丛 (Shrubland)                          → 灌丛 0.15
  71,72  草地 (Grassland)                          → 草地 0.30
  81,82  农田 (Cropland)                           → 农田 0.30
  91,92  湿地/苔原 (Wetland/Tundra)                → 湿地 0.50
  122    不透水面 (Impervious)                     → 建成区 0.80
  130    裸地 (Bareland)                           → 裸地 0.45
  140,150 水体 (Water)                             → 水体 1.00
  180    冰雪 (Snow/Ice)                           → 冰雪 0.10
  190,210 其他/无植被                               → 裸地 0.45
  0,255   NoData                                   → 不参与计算
"""
import numpy as np
import rasterio

LULC = '../data/14_landuse_10m.tif'
OUT_R = '../results/seasonal_runoff_10m.tif'
OUT_V = '../results/runoff_volume_10m_m3.tif'

# ERA5 2018-2022 平均月降水 (mm) — 4-11月
monthly_precip = {4: 24.7, 5: 87.2, 6: 128.9, 7: 127.2, 8: 228.4, 9: 129.3, 10: 44.1, 11: 52.2}
SEASONAL = sum(monthly_precip.values())
print(f"ERA5 4-11月降水合计: {SEASONAL:.1f} mm")

# GLC_FCS10 编码 → 径流系数
lulc_C = {
    10: 0.15, 11: 0.15, 20: 0.15,     # 森林
    61: 0.15, 62: 0.15,               # 灌丛
    71: 0.30, 72: 0.30,               # 草地
    81: 0.30, 82: 0.30,               # 农田
    91: 0.50, 92: 0.50,               # 湿地
    122: 0.80,                        # 不透水面
    130: 0.45,                        # 裸地
    140: 1.00, 150: 1.00,             # 水体
    180: 0.10,                        # 冰雪
    190: 0.45, 210: 0.45,             # 其他/无植被
}

with rasterio.open(LULC) as ds:
    lulc = ds.read(1)
    prof = ds.profile.copy()
    prof.update(dtype=rasterio.float32, count=1, compress='lzw', nodata=-9999)
    res = ds.res[0]
    h = ds.height

# 真实面积 (cos 纬度修正)
rows = np.arange(h)
lats = ds.transform[5] - (rows + 0.5) * res
cos_lat = np.cos(np.radians(np.clip(lats, -60, 60)))
area_m2_per_row = (res * 111132.0) * (res * 111320.0 * cos_lat)
S = np.broadcast_to(area_m2_per_row[:, None], (ds.height, ds.width)).astype(np.float32)

nodata = (lulc == 0) | (lulc == 255)
C = np.zeros_like(lulc, dtype=np.float32)
for code, c in lulc_C.items():
    C[lulc == code] = c

# 未映射的有效编码 → 提示
mapped = np.isin(lulc, list(lulc_C.keys())) | nodata
unmapped = (~mapped)
if unmapped.any():
    uv = np.unique(lulc[unmapped])
    print(f"⚠️ 未映射编码: {uv.tolist()}")

R = C * SEASONAL
R[nodata] = np.nan
V = R / 1000.0 * S
V[nodata] = np.nan

print(f"\n有效像元: {(~nodata).mean()*100:.1f}%")
print(f"径流深度 R: {np.nanmin(R):.1f} - {np.nanmax(R):.1f} mm, mean={np.nanmean(R):.1f} mm")
print(f"径流体积 V: {np.nanmin(V):.2f} - {np.nanmax(V):.2f} m³, mean={np.nanmean(V):.3f} m³")

with rasterio.open(OUT_R, 'w', **prof) as dst:
    dst.write(np.where(np.isnan(R), -9999, R).astype(np.float32), 1)
with rasterio.open(OUT_V, 'w', **prof) as dst:
    dst.write(np.where(np.isnan(V), -9999, V).astype(np.float32), 1)
print("SAVED:", OUT_R, OUT_V)

# 按类统计
print(f"\n按土地利用统计:")
print(f"{'编码':>6} {'像元数':>12} {'R(mm)':>8} {'V(总10⁶m³)':>12}")
for code in sorted(lulc_C):
    mask = (lulc == code)
    n = mask.sum()
    if n > 0:
        print(f"{code:>6} {n:>12,} {R[mask].mean():>8.1f} {V[mask].sum()/1e6:>12.2f}")
