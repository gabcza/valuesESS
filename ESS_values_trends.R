### Final models with charts
# ! once trends are calculated -> writing trends to a table [to be done, see last part of the code

###############################################################################################################################################
# m1_1: Left-right identification

# best fit with LR
m_vl_r1_1 <- readRDS("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh/m_vl_r1_1.rds")

sjt.lmer(m_vl_r1_1)

# m2: best fit with AIC
m_vl_r2 <- readRDS("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh/m_vl_r2.rds")

### Charts ###
### m1_1
# openness 
png("m_vl_r1_1_open.png")
plot(effect(term = "openess_s_gc:west_east:essround", 
            mod = m_vl_r1_1, 
            x.var = "openess_s_gc"),
     multiline = TRUE,
     main = " Openness x West vs. East ",
     xlab = "Openness",
     ylab = "Left-right self-placement",
     ylim = c(0, 0.8),
     lines = list(col = c("black", "black")),
     lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()


# self-enh 
png("m_vl_r1_1_enh.png")
plot(effect(term = "west_east:essround:self_enh_s_gc", 
            mod = m_vl_r1_1, 
            x.var = "self_enh_s_gc"),
     multiline = TRUE,
     main = "Self-enhancement x West vs. East ",
     xlab = "Self-enhancement",
     ylab = "Left-right self-placement",
     ylim = c(0, 0.8),
     lines = list(col = c("black", "black")),
     lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

### m2
# openness
png("m_vl_r2_open.png")
plot(effect(term = "openess_s_gc:welstate", 
            mod = m_vl_r2, 
            x.var = "openess_s_gc"),
     multiline = TRUE,
     main = " Openness x Welfare state ",
     xlab = "Openness",
     ylab = "Left-right self-placement",
     ylim = c(0, 0.8),
     #lines = list(col = c("black", "black")),
     #lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

# self-enh 
png("m_vl_r2_s_enh.png")
plot(effect(term = "welstate:self_enh_s_gc", 
            mod = m_vl_r2, 
            x.var = "self_enh_s_gc"),
     multiline = TRUE,
     main = "Self-enhancement x Welfare state ",
     xlab = "Self-enhancement",
     ylab = "Left-right self-placement",
     ylim = c(0, 0.8),
     #lines = list(col = c("black", "black")),
     #lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

### Trends ###
### m1_1
# Trends comparisons
# by ESS round
trend_vl_r1_1_o = cld(lstrends (m_vl_r1_1, ~ west_east|essround, var = "openess_s_gc"),details=TRUE)
saveRDS(trend_vl_r1_1_o, "trend_vl_r1_1_o.rds") # save trend
readRDS("trend_vl_r1_1_o.rds") # read trend
# by west-east
trend_vl_r1_1_1_o = cld(lstrends (m_vl_r1_1, ~ essround|west_east, var = "openess_s_gc"), details = TRUE)
saveRDS(trend_vl_r1_1_1_o, "trend_vl_r1_1_1_o.rds") # save trend
readRDS("trend_vl_r1_1_1_o.rds") # read trend

# self-enhancement
trend_vl_r1_1_s = cld(lstrends (m_vl_r1_1, ~ west_east|essround, var = "self_enh_s_gc"),details=TRUE)
saveRDS(trend_vl_r1_1_s, "trend_vl_r1_1_s.rds") # save trend
readRDS("trend_vl_r1_1_s.rds") # read trend
# by west-east
trend_vl_r1_1_1_s = cld(lstrends (m_vl_r1_1, ~ essround|west_east, var = "self_enh_s_gc"), details = TRUE)
saveRDS(trend_vl_r1_1_1_s, "trend_vl_r1_1_1_s.rds") # save trend
readRDS("trend_vl_r1_1_1_s.rds") # read trend

### m2
# openess
trend_vl_r2_o = cld(lstrends (m_vl_r2, ~ welstate, var = "openess_s_gc"),details=TRUE)
saveRDS(trend_vl_r2_o, "trend_vl_r2_o.rds") # save trend
readRDS("trend_vl_r2_o.rds") # read trend

# self-enhancement
trend_vl_r2_s = cld(lstrends (m_vl_r2, ~ welstate, var = "self_enh_s_gc"),details=TRUE)
saveRDS(trend_vl_r2_s, "trend_vl_r2_s.rds") # save trend
readRDS("trend_vl_r2_s.rds") # read trend

################################################################################################################################
### Cultural beliefs
# m1_1: best fit with AIC and LR
m_vl_cult1_1 <- readRDS("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh/m_vl_cult1_1.rds")
sjt.lmer(m_vl_cult1_1)

### Charts 
### m1_1
# openness
png("m_vl_cult1_1_open.png")
plot(effect(term = "openess_s_gc:west_east:essround", 
            mod = m_vl_cult1_1, 
            x.var = "openess_s_gc"),
     multiline = TRUE,
     main = " Openness x West vs. East ",
     xlab = "Openness",
     ylab = "Cultural beliefs",
     ylim = c(0, 0.8),
     lines = list(col = c("black", "black")),
     lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()


# self-enh 
png("m_vl_cult1_1_enh.png")
plot(effect(term = "west_east:essround:self_enh_s_gc", 
            mod = m_vl_cult1_1, 
            x.var = "self_enh_s_gc"),
     multiline = TRUE,
     main = "Self-enhancement x West vs. East ",
     xlab = "Self-enhancement",
     ylab = "Cultural beliefs",
     ylim = c(0, 0.8),
     lines = list(col = c("black", "black")),
     lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

### Trends ###
### m1_1
# Trends comparisons
# by ESS round
trend_vl_cult1_1_o = cld(lstrends (m_vl_cult1_1, ~ west_east|essround, var = "openess_s_gc"),details=TRUE)
saveRDS(trend_vl_cult1_1_o, "trend_vl_cult1_1_o.rds") # save trend
readRDS("trend_vl_cult1_1_o.rds") # read trend
# by west-east
trend_vl_cult1_1_1_o = cld(lstrends (m_vl_cult1_1, ~ essround|west_east, var = "openess_s_gc"), details = TRUE)
saveRDS(trend_vl_cult1_1_1_o, "trend_vl_cult1_1_1_o.rds") # save trend
readRDS("trend_vl_cult1_1_1_o.rds") # read trend

# self-enhancement
trend_vl_cult1_1_s = cld(lstrends (m_vl_cult1_1, ~ west_east|essround, var = "self_enh_s_gc"),details=TRUE)
saveRDS(trend_vl_cult1_1_s, "trend_vl_cult1_1_s.rds") # save trend
readRDS("trend_vl_cult1_1_s.rds") # read trend
# by west-east
trend_vl_cult1_1_1_s = cld(lstrends (m_vl_cult1_1, ~ essround|west_east, var = "self_enh_s_gc"), details = TRUE)
saveRDS(trend_vl_cult1_1_1_s, "trend_vl_cult1_1_1_s.rds") # save trend
readRDS("trend_vl_cult1_1_1_s.rds") # read trend

################################################################################################################################
### Economic beliefs
# m2_1: best fit with AIC and LR
m_vl_econ2_1 <- readRDS("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh/m_vl_econ2_1.rds")
sjt.lmer(m_vl_econ2_1)

### Charts
### m2
# openness 
png("m_vl_econ2_1_open.png")
plot(effect(term = "openess_s_gc:welstate:essround", 
            mod = m_vl_econ2_1, 
            x.var = "openess_s_gc"),
     multiline = TRUE,
     main = " Openness x Welfare State ",
     xlab = "Openness",
     ylab = "Economic beliefs",
     ylim = c(0, 0.8),
     #lines = list(col = c("black", "black")),
     #lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()


# self-enh 
png("m_vl_econ2_1_enh.png")
plot(effect(term = "welstate:essround:self_enh_s_gc", 
            mod = m_vl_econ2_1, 
            x.var = "self_enh_s_gc"),
     multiline = TRUE,
     main = "Self-enhancement x Welfare State ",
     xlab = "Self-enhancement",
     ylab = "Economic beliefs",
     ylim = c(0, 0.8),
     #lines = list(col = c("black", "black")),
     #lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

### Trends ###
### m2_1
# Trends comparisons
# by ESS round
trend_vl_econ2_1_o = cld(lstrends (m_vl_econ2_1, ~ welstate|essround, var = "openess_s_gc"),details=TRUE)
saveRDS(trend_vl_econ2_1_o, "trend_vl_econ2_1_o.rds") # save trend
readRDS("trend_vl_econ2_1_o.rds") # read trend
# by welstate
trend_vl_econ2_1_1_o = cld(lstrends (m_vl_econ2_1, ~ essround|welstate, var = "openess_s_gc"), details = TRUE)
saveRDS(trend_vl_econ2_1_1_o, "trend_vl_econ2_1_1_o.rds") # save trend
readRDS("trend_vl_econ2_1_1_o.rds") # read trend

# self-enhancement
trend_vl_econ2_1_s = cld(lstrends (m_vl_econ2_1, ~ welstate|essround, var = "self_enh_s_gc"),details=TRUE)
saveRDS(trend_vl_econ2_1_s, "trend_vl_econ2_1_s.rds") # save trend
readRDS("trend_vl_econ2_1_s.rds") # read trend
# by welstate
trend_vl_econ2_1_1_s = cld(lstrends (m_vl_econ2_1, ~ essround|welstate, var = "self_enh_s_gc"), details = TRUE)
saveRDS(trend_vl_econ2_1_1_s, "trend_vl_econ2_1_1_s.rds") # save trend
readRDS("trend_vl_econ2_1_1_s.rds") # read trend

################################################################################################################################
### Economic beliefs: 1 item

# best fit with LR
m_vl_incdif1_1 <- readRDS("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh/m_vl_incdif1_1.rds")
sjt.lmer(m_vl_incdif1_1)

# m2: best fit with AIC
m_vl_incdif2 <- readRDS("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh/m_vl_incdif2.rds")

### Charts ###
### m1_1
# openness
png("m_vl_incdif1_1_open.png")
plot(effect(term = "openess_s_gc:west_east:essround", 
            mod = m_vl_incdif1_1, 
            x.var = "openess_s_gc"),
     multiline = TRUE,
     main = " Openness x West vs. East ",
     xlab = "Openness",
     ylab = "Economic beliefs [1 item]",
     ylim = c(0, 0.8),
     lines = list(col = c("black", "black")),
     lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

# self-enh
png("m_vl_incdif1_1_enh.png")
plot(effect(term = "west_east:essround:self_enh_s_gc", 
            mod = m_vl_incdif1_1, 
            x.var = "self_enh_s_gc"),
     multiline = TRUE,
     main = "Self-enhancement x West vs. East ",
     xlab = "Self-enhancement",
     ylab = "Economic beliefs [1 item]",
     ylim = c(0, 0.8),
     lines = list(col = c("black", "black")),
     lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

### m2
# openness
png("m_vl_incdif2_open.png")
plot(effect(term = "openess_s_gc:welstate", 
            mod = m_vl_incdif2, 
            x.var = "openess_s_gc"),
     multiline = TRUE,
     main = " Openness x Welfare state ",
     xlab = "Openness",
     ylab = "Economic beliefs [1 item]",
     ylim = c(0, 0.8),
     #lines = list(col = c("black", "black")),
     #lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

# self-enh 
png("m_vl_incdif2_s_enh.png")
plot(effect(term = "welstate:self_enh_s_gc", 
            mod = m_vl_incdif2, 
            x.var = "self_enh_s_gc"),
     multiline = TRUE,
     main = "Self-enhancement x Welfare state ",
     xlab = "Self-enhancement",
     ylab = "Economic beliefs [1 item]",
     ylim = c(0, 0.8),
     #lines = list(col = c("black", "black")),
     #lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

########################################################
# m2_1: after changing optimizer, shows best fit (LRT)
# so far the model did not converge but shows little problem
m_vl_incdif2_1 <- readRDS("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh/m_vl_incdif2_1.rds")
ss <- getME(m_vl_incdif2_1 ,c("theta","fixef"))
m_vl_incdif2_1_update <- readRDS("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh/m_vl_incdif2_1_update.rds")

sjt.lmer(m_vl_incdif2_1_update)

### Charts
### m2
# openness 
png("m_vl_incdif2_1_open.png")
plot(effect(term = "openess_s_gc:welstate:essround", 
            mod = m_vl_incdif2_1, 
            x.var = "openess_s_gc"),
     multiline = TRUE,
     main = " Openness x Welfare State ",
     xlab = "Openness",
     ylab = "Opposition toward redistribution",
     ylim = c(0, 0.8),
     #lines = list(col = c("black", "black")),
     #lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()


# self-enh 
png("m_vl_incdif2_1_enh.png")
plot(effect(term = "welstate:essround:self_enh_s_gc", 
            mod = m_vl_incdif2_1, 
            x.var = "self_enh_s_gc"),
     multiline = TRUE,
     main = "Self-enhancement x Welfare State ",
     xlab = "Self-enhancement",
     ylab = "Opposition toward for redistribution",
     ylim = c(0, 0.8),
     #lines = list(col = c("black", "black")),
     #lty = c("solid", "dashed"),
     key.args = list(space = "right", columns = 1, border = FALSE, cex = 1.2))
dev.off()

### Trends ###
### m2_1
# Trends comparisons
# by ESS round
trend_vl_incdif2_1_o = cld(lstrends (m_vl_incdif2_1_update2, ~ welstate|essround, var = "openess_s_gc"),details=TRUE)
saveRDS(trend_vl_incdif2_1_o, "trend_vl_incdif2_1_o.rds") # save trend
readRDS("trend_vl_incdif2_1_o.rds") # read trend
# by welstate
trend_vl_incdif2_1_1_o = cld(lstrends (m_vl_incdif2_1_update2, ~ essround|welstate, var = "openess_s_gc"), details = TRUE)
saveRDS(trend_vl_incdif2_1_1_o, "trend_vl_incdif2_1_1_o.rds") # save trend
readRDS("trend_vl_incdif2_1_1_o.rds") # read trend

# self-enhancement
trend_vl_incdif2_1_s = cld(lstrends (m_vl_incdif2_1_update2, ~ welstate|essround, var = "self_enh_s_gc"),details=TRUE)
saveRDS(trend_vl_incdif2_1_s, "trend_vl_incdif2_1_s.rds") # save trend
readRDS("trend_vl_incdif2_1_s.rds") # read trend
# by welstate
trend_vl_incdif2_1_1_s = cld(lstrends (m_vl_incdif2_1_update2, ~ essround|welstate, var = "self_enh_s_gc"), details = TRUE)
saveRDS(trend_vl_incdif2_1_1_s, "trend_vl_incdif2_1_1_s.rds") # save trend
readRDS("trend_vl_incdif2_1_1_s.rds") # read trend

################################################################################################################################
################################################################################################################################
### Writing trends to a table 
setwd("C:/Users/gczar_000/Documents/_ESS/Open_and_Self-enh")

################################################################################################################################
### Left-right self-placement ###

### m1_1
### OPENNESS
### Comparison between welstate
x = trend_vl_r1_1_o$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_r1_1_o$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
### Comparison between ESS rounds
x= trend_vl_r1_1_1_o$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_r1_1_1_o$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)

### SELF-ENHANCEMENT
### Comparison between welstate
x = trend_vl_r1_1_s$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_r1_1_s$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
### Comparison between ESS rounds
x= trend_vl_r1_1_1_s$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_r1_1_1_s$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)

### m2
# OPENNESS
trend_vl_r2_o = cld(lstrends (m_vl_r2, ~ welstate, var = "openess_s_gc"),details=TRUE)
saveRDS(trend_vl_r2_o, "trend_vl_r2_o.rds") # save trend
readRDS("trend_vl_r2_o.rds") # read trend

# SELF-ENHANCEMENT
trend_vl_r2_s = cld(lstrends (m_vl_r2, ~ welstate, var = "self_enh_s_gc"),details=TRUE)
saveRDS(trend_vl_r2_s, "trend_vl_r2_s.rds") # save trend
readRDS("trend_vl_r2_s.rds") # read trend


################################################################################################################################
### Cultural beliefs ###

### m1_1
### OPENNESS
### Comparison between welstate
x = trend_vl_cult1_1_o$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_cult1_1_o$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
### Comparison between ESS rounds
x= trend_vl_cult1_1_1_o$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_cult1_1_1_o$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)

### SELF-ENHANCEMENT
### Comparison between welstate
x = trend_vl_cult1_1_s$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_cult1_1_s$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
### Comparison between ESS rounds
x= trend_vl_cult1_1_1_s$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_cult1_1_1_s$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)



################################################################################################################################
## Economic beliefs ###
### OPENNESS
### Comparison between welstate
x = trend_vl_econ2_1_o$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_econ2_1_o$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
### Comparison between ESS rounds
x= trend_vl_econ2_1_1_o$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_econ2_1_1_o$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)

### SELF-ENHANCEMENT
### Comparison between welstate
x = trend_vl_econ2_1_s$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_econ2_1_s$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
### Comparison between ESS rounds
x= trend_vl_econ2_1_1_s$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_econ2_1_1_s$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)

################################################################################################################################
## Economic beliefs: 1-item (preference for redistribution) ###
### OPENNESS
### Comparison between welstate
x = trend_vl_incdif1_1_o$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_incdif1_1_o$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
### Comparison between ESS rounds
x= trend_vl_incdif1_1_1_o$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_incdif1_1_1_o$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)

### SELF-ENHANCEMENT
### Comparison between welstate
x = trend_vl_incdif1_1_s$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_incdif1_1_s$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
### Comparison between ESS rounds
x= trend_vl_incdif1_1_1_s$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_incdif1_1_1_s$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)

### m2
# OPENNES
trend_vl_incdif2_o = cld(lstrends (m_vl_incdif2, ~ welstate, var = "openess_s_gc"),details=TRUE)
saveRDS(trend_vl_incdif2_o, "trend_vl_incdif2_o.rds") # save trend
readRDS("trend_vl_incdif2_o.rds") # read trend

# SELF-ENHANCEMENT
trend_vl_incdif2_s = cld(lstrends (m_vl_incdif2, ~ welstate, var = "self_enh_s_gc"),details=TRUE)
saveRDS(trend_vl_incdif2_s, "trend_vl_incdif2_s.rds") # save trend
readRDS("trend_vl_incdif2_s.rds") # read trend

### m2_1 [updated with different optimizer, the default one initially gave a warning]
### OPENNESS
### Comparison between welstate
x = trend_vl_incdif2_1_o$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_incdif2_1_o$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
### Comparison between ESS rounds
x= trend_vl_incdif2_1_1_o$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_incdif2_1_1_o$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)

### SELF-ENHANCEMENT
### Comparison between welstate
x = trend_vl_incdif2_1_s$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_incdif2_1_s$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
### Comparison between ESS rounds
x= trend_vl_incdif2_1_1_s$lsmeans
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)
###
x=trend_vl_incdif2_1_1_s$comparisons
x$contrast = paste0("'", x$contrast)
write.table(x,"x.csv",sep=";",dec=",", quote = TRUE, row.names = FALSE)

