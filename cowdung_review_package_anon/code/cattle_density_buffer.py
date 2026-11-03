#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# PURPOSE  Equation (1): cattle density declines linearly from the village value to 8 heads/km2 at a 3.5 km radius
# INPUT    ../data/07_village_points_grazing.csv, ../data/09_gpw_cattle_30m.tif, ../data/10_grazing_mask_30m.tif
# OUTPUT   ../results/cattle_density_10m.tif
# RUN      python3 cattle_density_buffer.py   (working directory: code/)
# PACKAGES rasterio, numpy

"""v5: 村庄圆形缓冲区 + GPW原值 + 边缘8 (半径3500m, 文献值, GPW×0.1修正)
逻辑: 以村庄为圆心, 半径 R0=3500m 的圆盘
  - 圆心处 = GPW 原值 (不衰减)
  - 边缘 (r=R0) = 8 (下限)
  - 中间线性过渡: value(r) = 8 + (amp - 8) * (1 - r/R0)
  - 圆外 = 0
  - 用真实米制距离 (经度方向乘 cos(lat))  地面真圆
  - 最后乘放牧区掩膜
文献: Grzedzicka et al. 2021 (Restoration Ecology) - 半散养奶牛活动半径通常 3.5km
"""
import csv
import numpy as np
import rasterio
from rasterio.transform import from_origin
import math

LON_MIN, LON_MAX = 130.066456, 131.782867
LAT_MIN, LAT_MAX = 42.290701, 43.509086
RES = 0.000089831528412
R0 = 3500.0      # m, 缓冲区半径 (文献值 3.5 km)
EDGE = 8.0       # 边缘值
# 窗口半径按经度方向(更短)算, 确保圆盘完整覆盖: 3500m / (7.3m/px) ≈ 480px
RADIUS_PX = int(R0 / (RES * 111320 * math.cos(math.radians(42.5)))) + 2

# 10m 网格
nx, ny = 19107, 13563

# 读取村庄
villages = []
with open('../data/07_village_points_grazing.csv', encoding='utf-8-sig') as f:
    for row in csv.DictReader(f):
        villages.append((float(row['lon']), float(row['lat'])))

# 读取 GPW 原值  每个村庄幅度
#  GPW 存储值需 ×0.1 才是真实密度 (heads/km²) — GDAL scale factor
with rasterio.open('../data/09_gpw_cattle_30m.tif') as dc:
    cattle30 = dc.read(1).astype(np.float32)
    nodata = dc.nodata
    cattle30[cattle30 == nodata] = 0
    amps = []
    for lon, lat in villages:
        r, c = dc.index(lon, lat)
        amp = float(cattle30[r, c]) * 0.1   # scale factor 0.1
        amps.append(max(amp, EDGE))  # 最低取8, 保证边缘<=内部
print(f"村庄数量: {len(villages)}")
print(f"村庄GPW原值: min={min(amps):.1f} max={max(amps):.1f} mean={np.mean(amps):.1f}")

def lonlat_to_px(lon, lat):
    c = int(round((lon - LON_MIN) / RES))
    r = int(round((LAT_MAX - lat) / RES))
    return r, c

# 预计算每个村庄的圆盘 (真实米制距离)
# 行方向: 1px = RES*111132 m; 列方向: 1px = RES*111320*cos(lat) m
field = np.zeros((ny, nx), dtype=np.float32)
for i, ((lon, lat), amp) in enumerate(zip(villages, amps)):
    r0, c0 = lonlat_to_px(lon, lat)
    r_lo, r_hi = max(0, r0-RADIUS_PX), min(ny, r0+RADIUS_PX+1)
    c_lo, c_hi = max(0, c0-RADIUS_PX), min(nx, c0+RADIUS_PX+1)
    # 窗口内像素相对村庄的米制距离
    rows = np.arange(r_lo, r_hi)
    cols = np.arange(c_lo, c_hi)
    dlat = (rows - r0) * RES * 111132.0            # 纬度方向米
    dlon = (cols - c0) * RES * 111320.0 * math.cos(math.radians(lat))  # 经度方向米 (cos修正)
    dist = np.sqrt(dlat[:, None]**2 + dlon[None, :]**2)
    win = np.where(dist <= R0, 8.0 + (amp - 8.0) * (1.0 - dist / R0), 0.0).astype(np.float32)
    np.maximum(field[r_lo:r_hi, c_lo:c_hi], win, out=field[r_lo:r_hi, c_lo:c_hi])
    if (i+1) % 10 == 0:
        print(f"  {i+1}/{len(villages)} 完成")

print("叠加完成")
print(f"正值像元: {(field>0).sum():,} ({(field>0).mean()*100:.2f}%)")
vals = field[field > 0]
print(f"密度: min={vals.min():.2f} max={vals.max():.2f} mean={vals.mean():.2f}")

# 应用放牧区掩膜
with rasterio.open('../data/10_grazing_mask_30m.tif') as dm:
    mask = dm.read(1)
mask10 = np.repeat(np.repeat(mask, 3, axis=0), 3, axis=1)[:ny, :nx]
final = field * (mask10 == 1).astype(np.float32)
print(f"掩膜后正值像元: {(final>0).sum():,} ({(final>0).mean()*100:.2f}%)")
v2 = final[final > 0]
print(f"掩膜后密度: min={v2.min():.2f} max={v2.max():.2f} mean={v2.mean():.2f}")

# 保存
transform = from_origin(LON_MIN, LAT_MAX, RES, RES)
profile = {
    'driver': 'GTiff', 'width': nx, 'height': ny, 'count': 1,
    'dtype': 'float32', 'crs': 'EPSG:4326', 'transform': transform,
    'compress': 'lzw', 'tiled': True, 'nodata': 0.0,
}
OUT = '../results/cattle_density_10m.tif'
with rasterio.open(OUT, 'w', **profile) as dst:
    dst.write(final.astype(np.float32), 1)
print("SAVED:", OUT)
import os
print("size:", os.path.getsize(OUT)//1024, "KB")
