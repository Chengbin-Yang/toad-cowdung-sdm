#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# PURPOSE  Classified preview of concentration (0.8, 1.2, 2.5, 3.5 mL/L) with two denominators for area shares
# INPUT    ../results/manure_concentration_mL_L.tif
# OUTPUT   ../results/concentration_classes_preview.png
# RUN      python3 concentration_classified_plot.py   (working directory: code/)
# PACKAGES rasterio, numpy, matplotlib

"""牛粪径流浓度分级图 (修正版 ε=0.3) — 双面板: 分级图 + 分级统计条形图"""
import rasterio
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
from matplotlib.colors import ListedColormap, BoundaryNorm
import math

font_path = "/usr/share/fonts/opentype/noto/NotoSerifCJK-Bold.ttc"
zh = fm.FontProperties(fname=font_path)

SRC = '../results/manure_concentration_mL_L.tif'
with rasterio.open(SRC) as ds:
    c = ds.read(1, out_shape=(ds.height//20, ds.width//20)).astype(float)
    extent = [ds.bounds.left, ds.bounds.right, ds.bounds.bottom, ds.bounds.top]
    lat_mid = (ds.bounds.top + ds.bounds.bottom) / 2

# 分级: 0=无数据, 1=<0.8, 2=0.8-1.2, 3=1.2-2.5, 4=2.5-3.5, 5=>=3.5
cls = np.zeros_like(c, dtype=np.uint8)
cls[(c > 0) & (c < 0.8)] = 1
cls[(c >= 0.8) & (c < 1.2)] = 2
cls[(c >= 1.2) & (c < 2.5)] = 3
cls[(c >= 2.5) & (c < 3.5)] = 4
cls[c >= 3.5] = 5

colors = ['#f0f0f0', '#a6d96a', '#fee08b', '#fdae61', '#d7191c']
cmap = ListedColormap(colors)
norm = BoundaryNorm([-0.5, 0.5, 1.5, 2.5, 3.5, 4.5], cmap.N)

fig, axes = plt.subplots(1, 2, figsize=(16, 7.5), dpi=140,
                         gridspec_kw={'width_ratios': [2.2, 1]})

# 左: 分级地图
ax = axes[0]
im = ax.imshow(cls, extent=extent, origin='upper', cmap=cmap, norm=norm, interpolation='nearest')
ax.set_aspect(1.0 / math.cos(math.radians(lat_mid)))
labels = ['无数据', '<0.8 (无/极低)', '0.8–1.2 (低)', '1.2–2.5 (中)', '2.5–3.5 (高)', '≥3.5 (超高)']
patches = [plt.Rectangle((0,0),1,1, fc=colors[0], ec='none')] + \
          [plt.Rectangle((0,0),1,1, fc=color, ec='none') for color in colors[1:]]
leg = ax.legend(patches, labels, loc='lower right', prop=zh, fontsize=9, framealpha=0.9,
                title='浓度 (mL/L)', title_fontproperties=zh)
ax.set_title('牛粪径流浓度分级图 (ε=0.3 修正)', fontproperties=zh, fontsize=13)
ax.set_xlabel('Longitude (°E)', fontproperties=zh, fontsize=10)
ax.set_ylabel('Latitude (°N)', fontproperties=zh, fontsize=10)
ax.tick_params(labelsize=9)

# 右: 分级统计 (双口径)
ax = axes[1]
n_valid = (c > 0).sum()
n_full = c.size
valid_pct = [((cls == i) & (c > 0)).sum() / n_valid * 100 for i in range(1, 6)]
full_pct = [(cls == i).sum() / n_full * 100 for i in range(1, 6)]
x = np.arange(5)
w = 0.38
b1 = ax.bar(x - w/2, valid_pct, w, color=colors[1:], edgecolor='white', label='有效区占比')
b2 = ax.bar(x + w/2, full_pct, w, color=colors[1:], edgecolor='white', alpha=0.55, label='全图占比')
ax.set_xticks(x)
ax.set_xticklabels(['<0.8', '0.8–1.2', '1.2–2.5', '2.5–3.5', '≥3.5'], fontproperties=zh, fontsize=9)
ax.set_ylabel('占比 (%)', fontproperties=zh, fontsize=10)
ax.set_title('分级面积占比 (双口径)', fontproperties=zh, fontsize=11)
for rect, vp, fp in zip(b1, valid_pct, full_pct):
    ax.text(rect.get_x() + rect.get_width()/2, rect.get_height() + 0.5, f'{vp:.1f}%',
            ha='center', fontsize=8, color='#333333')
for rect, fp in zip(b2, full_pct):
    ax.text(rect.get_x() + rect.get_width()/2, rect.get_height() + 0.5, f'{fp:.2f}%',
            ha='center', fontsize=8, color='#888888')
ax.legend(prop=zh, fontsize=9)
ax.set_ylim(0, max(valid_pct) * 1.18)
ax.tick_params(labelsize=9)

plt.tight_layout()
OUT = '../results/concentration_classes_preview.png'
plt.savefig(OUT, dpi=140, bbox_inches='tight')
print("SAVED:", OUT)
import os
print("size:", os.path.getsize(OUT)//1024, "KB")

# 打印完整统计
print("\n=== 分级统计 ===")
print(f"有效像元(有粪): {n_valid:,} ({n_valid/n_full*100:.2f}% of 全图)")
print(f"全图像元: {n_full:,}")
for i, nm in enumerate(['<0.8 (无/极低)', '0.8–1.2 (低)', '1.2–2.5 (中)', '2.5–3.5 (高)', '≥3.5 (超高)'], 1):
    print(f"  {nm:>14}: 有效区 {valid_pct[i-1]:6.2f}% / 全图 {full_pct[i-1]:6.3f}%")
