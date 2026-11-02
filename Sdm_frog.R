library(raster) 
library(rgdal)
library(dismo)
library(ENMeval)
library(megaSDM)
library(SDMtune)
library(ecospat)
library(MASS)
library(dplyr)
library(ggplot2)
library(extrafont)
library(rasterVis)
library(zeallot) 
library(spThin)
library(terra)
library(graphics)

#stepA: Load species data
occs <- read.csv('D:/frog/env2/dataset/salamenter.csv')
colnames(occs) = c('FID','species', 'Latitude','Longitude')
head(occs)

#stepB: Load environmental data traditional raster
env <- raster::stack(list.files(path = 'D:/frog/env2', pattern = '.tif$', full.names = T))


env2 <- rast(list.files(path = "D:/frog/env2", 
                        pattern = ".tif$", full.names = TRUE))


#plot(env2[[1]])
plot(env2)
names(env2)
#step1: Refine and load species points
thinned_dataset_full <-
  thin(loc.data = occs, 
       lat.col = "Latitude", long.col = "Longitude", 
       spec.col = "species", 
       thin.par = 1, reps = 15,     ###Thinning parameter - the distance (in kilometers) that you want records to beseparated by.###
       locs.thinned.list.return = TRUE, 
       write.files = TRUE, 
       max.files = 1, 
       out.dir = "D:/frog/env2/dataset", out.base = "S_thindata", 
       write.log.file = TRUE,
       log.file = "S_thindata.txt")

occlist <- list.files(path = 'D:/frog/env2/dataset/thining', pattern = '.csv', full.names = T)
occ2<- read.csv('D:/frog/env2/dataset/thining/S_thindata_thin1.csv')
head(occ2)

#BackgroundBuffers(occlist = occlist, envdata = env2, output = 'D:/frog/env2/dataset/thining', buff_distance = 50000)

buff <- readOGR('D:/frog/env2/buffer/buffer_50km_buffer.shp')
buff2 <- vect('D:/frog/env2/buffer/buffer_50km_buffer.shp')


env_calib <- raster::mask(env, buff)
#plot(env_calib[[1]])


# Use terra package for mask operation   
env_calib2 <- terra::mask(env2, buff2)

targ <- read.csv('D:/frog/env2/dataset/salamenter02.csv') %>% select(3,4)
head(targ)
plot(targ)
targ.ras <- rasterize(targ, env, 1) #Rasterize points to 1
targ.pres <- which(values(targ.ras) == 1)
targ.pres.locs <- coordinates(targ.ras)[targ.pres, ]    ##Retrieve and pass spatial coordinates

## Use kde2d for two-dimensional kernel density estimation (kde2d: Two-Dimensional Kernel Density Estimation) ##

targ.dens <- MASS::kde2d(targ.pres.locs[,1], targ.pres.locs[,2],      
                         n = c(nrow(targ.ras), ncol(targ.ras)),                  
                         lims = c(extent(env_calib)[1], extent(env_calib)[2], 
                                  extent(env_calib)[3], extent(env_calib)[4]))
targ.dens.ras <- raster(targ.dens, env_calib)                 ## Rasterize
targ.dens.ras2 <- resample(targ.dens.ras, env_calib)     ## Resample raster size to keep it consistent
bias.layer <- raster::mask(targ.dens.ras2, buff)              ## Mask and clip
crs(bias.layer) = crs(env_calib)                                     ## Unify coordinates
plot(bias.layer)         ## Output kernel density layer



## sample target group bg points (sample target background points)
length(which(!is.na(values(subset(env_calib,1)))))    ##View pixel count

targ.bg <- xyFromCell(bias.layer, 
                      sample(which(!is.na(values(subset(env_calib, 1)))), 10000,prob = values(bias.layer)[!is.na(values(subset(env_calib, 1)))])) %>% as.data.frame()
colnames(targ.bg) = colnames(occs[, c(2,3)])
head(targ.bg)
write.csv(targ.bg, 'D:/frog/env2/buffer/bc/bg_sal.csv')
points(targ.bg)


targ.bg<- read.csv('D:/frog/env2/buffer/bc/bg_sal.csv')
head(targ.bg)


names(env2)
#Spearman test
cor.bg <- prepareSWD(species = 'pika', a = targ.bg[, c(2,3)], env = env2)
#cor.bg <- prepareSWD(species = 'pika', a = targ.bg[, c(2,3)], env = env2)

plotCor(cor.bg, method = 'spearman', cor_th = 0)
n=corVar(cor.bg, method = 'spearman', cor_th = 0)
#write.csv(n, 'D:/frog/frog_spearman.csv')
plotCor(cor.bg, method = 'spearman', cor_th = 0.7)
corVar(cor.bg, method = 'spearman', cor_th = 0.7)

## STEP 1 ::: remove variables with low importance (remove variables with low importance)
## first generate a default MaxEnt model (first generate a default MaxEnt model)
data_def <- prepareSWD(species = 'frog', p = occ2[,c(2,3)], 
                       a = targ.bg[, c(2,3)], env = env2)
c(train, test) %<-% trainValTest(data_def, test = 0.3)
(default_model <- train(method = 'Maxent', data = train))

reduced_var_mod <- reduceVar(default_model, th = 0, metric = 'tss', 
                             test = test, permut = 10, use_jk = T, use_pc = T)
## STEP 2 ::: remove highly correlated variables (remove highly correlated variables based on Spearman)
selected_var_mod <- varSel(reduced_var_mod, metric = 'tss', test = test,
                           bg4cor = cor.bg, method = 'spearman', cor_th = 0.7,
                           permut = 10, use_pc = T)


names(env)
env_select <- stack(subset(env, c(6, 7, 13, 17, 21, 22, 23, 24, 26))) #11 environmental factors for overall modeling
#env_select <-env
names(env_select)
#########Parameter tuning
occ2<- occ2[, c(2,3)]
targ.bg<- targ.bg[, c(2,3)]
head(occ2)
head(targ.bg)
colnames(targ.bg) <- colnames(occ2)
combined_data <- rbind(occ2, targ.bg)
head(combined_data)

combined_data_xy <- combined_data

###Rasterize background points and species occurrence points (prepare for masking)
targ.ras <- rasterize(combined_data_xy, env_select, 1)
plot(targ.ras)

# Perform mask extraction (based on background points and species occurrence points)
env_calib02 <- mask(env_select, targ.ras)


cvfolds <- ENMeval::get.randomkfold(occs = occ2, bg = targ.bg[, c(1,2)], kfolds = 10)
ENMeval::evalplot.grps(envs = env_calib02[[1]], pts = occ2, pts.grp = cvfolds$occs.grp)
ENMeval::evalplot.grps(envs = env_calib02[[1]], pts = targ.bg[, c(1,2)], pts.grp = cvfolds$bg.grp)


pika <- ENMevaluate(occs = occ2,
                    envs = env_calib02,
                    bg = targ.bg[, c(1,2)],
                    tune.args = list(fc = c('L', 'LQ', 'H', 'LQH', 'LQHP', 'LQHPT'), rm =seq(0.5,5,0.5)),
                    partitions = 'randomkfold',
                    partition.settings = list(kfolds = 10),
                    algorithm = 'maxent.jar',
                    doClamp = T)
eval <- eval.results(pika)


write.csv(eval, 'D:/frog/env2/buffer/bc/sala_Parameter.csv')


###Model optimization based on AICc
## optimal AICc (optimal AICc) #?LQH3
(opt.aicc <- eval %>% filter(delta.AICc == 0))  
mod.aicc <- eval.predictions(pika)[[opt.aicc$tune.args]]
plot(mod.aicc)

## sequential selection (sequential selection 1)   #?H0.5
(opt.seq1 <- eval %>% 
    filter(auc.val.avg == max(auc.val.avg)))

mod.seq1 <- eval.predictions(pika)[[opt.seq1$tune.args]]
plot(mod.seq1)


#####################Prediction
names(env_select)
# 1. 定义三个模型的参数配置列表
model_configs <- list(
  "LQH3" = list(
    linear = "true", quadratic = "true", hinge = "true", product = "false", threshold = "false",
    betamultiplier = "3"
  ),
  
  "H0.5" = list(
    linear = "false", quadratic = "false", hinge = "true", product = "false", threshold = "false",
    betamultiplier = "0.5"
  )
)

# 2. 优化后的分块预测函数
predict_in_blocks <- function(model, raster_data, block_size, output_dir) {
  nrows <- nrow(raster_data)
  ncols <- ncol(raster_data)
  block_files <- list()  
  block_index <- 1
  
  for (start_row in seq(1, nrows, by = block_size)) {
    end_row <- min(start_row + block_size - 1, nrows)
    
    # 提取分块 (显式指定列范围 1 到 cols，确保稳定性)
    block <- crop(raster_data, extent(raster_data, r1 = start_row, r2 = end_row, c1 = 1, c2 = ncols))
    
    # 预测分块
    block_pred <- predict(model, block)
    
    # 优化：使用 stack 组合所有重复(replicates)图层并计算平均值
    block_avg <- mean(stack(block_pred))
    
    # 保存分块结果
    block_file <- file.path(output_dir, paste0("block_", block_index, ".tif"))
    writeRaster(block_avg, block_file, format = "GTiff", overwrite = TRUE)
    
    block_files[[block_index]] <- block_file
    block_index <- block_index + 1
    
    # 释放内存
    block_pred <- NULL
    block_avg <- NULL
    gc()
    rJava::.jcall("java/lang/System", "V", "gc")
  }
  
  return(block_files)   
}

# 3. 循环运行三个模型
for (model_name in names(model_configs)) {
  message(paste("正在运行模型:", model_name, "--------------------"))
  
  cfg <- model_configs[[model_name]]
  
  # 动态构建 Maxent 参数
  maxent_args <- c(
    'responsecurves=true',
    'pictures=true',
    'jackknife=true',
    'outputformat=cloglog',
    'outputfiletype=asc',
    'randomseed=true',
    'replicates=10',
    'randomtestpoints=0',
    'replicatetype=crossvalidate',
    'writebackgroundpredictions=true',
    'writeplotdata=true',
    'visible=true',
    'autofeature=false',  # 关闭自动特征，确保手选特征生效
    'outputgrids=true',
    'plots=true',
    'maximumiterations=5000',
    paste0('linear=', cfg$linear),
    paste0('quadratic=', cfg$quadratic),
    paste0('product=', cfg$product),
    paste0('threshold=', cfg$threshold),
    paste0('hinge=', cfg$hinge),
    paste0('betamultiplier=', cfg$betamultiplier)
  )
  
  # 动态设置 Maxent 结果输出路径
  maxent_path <- file.path('D:/frog/endresult', paste0('PIKA_Test(', model_name, ')'))
  dir.create(maxent_path, showWarnings = FALSE, recursive = TRUE)
  
  # 运行 Maxent 模型
  dismo.mod <- dismo::maxent(x = env_select, 
                             p = occ2[, c(1,2)], 
                             a = targ.bg, 
                             path = maxent_path, 
                             args = maxent_args)
  
  # 设置分块预测输出目录
  output_dir <- file.path("D:/frog/endresult", paste0("frog_Test(", model_name, ")"), "block")   
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 调用分块预测
  block_size <- 50 # 可根据内存大小调整
  block_files <- predict_in_blocks(dismo.mod, env_select, block_size, output_dir)
  
  # 加载并合并所有分块
  all_blocks <- lapply(block_files, raster)   
  dismo.pred.avg <- do.call(merge, all_blocks)
  
  # 绘制当前模型结果
  plot(dismo.pred.avg, main = paste("Model:", model_name))
  
  # 保存最终大图
  output_final_dir <- file.path("D:/frog/endresult", paste0("frog(", model_name, ").tif"))   
  writeRaster(dismo.pred.avg, output_final_dir, format = "GTiff", overwrite = TRUE)
  
  # 清理当前循环的临时变量，防止内存累积
  rm(dismo.mod, dismo.pred.avg, all_blocks)
  gc()
}

message("所有模型运行完毕！")






















