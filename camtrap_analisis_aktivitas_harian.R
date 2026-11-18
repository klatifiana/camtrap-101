## Analisis data kamera jebak (camera trap)

## Tujuan: Pola aktivitas harian
## Dipersiapkan oleh: Kurnia Latifiana (k.latifiana@gmail.com)

## ---- 0. Packages dan pengaturan ----------------------------------------------
# Jalankan sekali saja:
# install.packages(c("camtrapR", "activity", "overlap"))

library(camtrapR) 
library(activity)
library(overlap)

getwd()

dir.create("output", showWarnings = FALSE)

# Tabel kejadian independen dari camtrapR (hasil script data management, langkah 6).
rec <- read.csv("output/record_table_independent.csv")

## ---- 1. Upaya survei dan ringkasan ----------------------------------------
camop <- cameraOperation(CTtable            = stations,
                         stationCol         = "StationID",
                         cameraCol          = "CamID",
                         setupCol           = "Setup_date",
                         retrievalCol       = "Retrieval_date",
                         hasProblems        = has_problems,
                         byCamera           = FALSE,
                         allCamsOn          = FALSE,
                         camerasIndependent = !cams_same_spot,
                         dateFormat         = "ymd")
sum(camop, na.rm = TRUE)                                     # total hari-kamera

n_by_sp <- sort(table(rec$Species), decreasing = TRUE)
n_by_sp
# Kurva aktivitas (Lashley dkk. 2018: disarankan ±100 rekaman)
data.frame(Species = names(n_by_sp), n = as.integer(n_by_sp),
           reliable = cut(as.integer(n_by_sp), c(-Inf, 29, 99, Inf),
                          labels = c("too few (<30)", "use with caution", "good (100+)")))


## ---- 1b. Indeks kelimpahan relatif (RAI) -----------------------------------
# RAI = kejadian independen / hari-kamera x 100  (kejadian per 100 hari-kamera)
# Hari-kamera diambil dari camop: hari pemasangan dan pengambilan dihitung 0,5,
# hari kamera rusak (Problem1_from/to, misalnya ST003) tidak dihitung.
# RAI BUKAN estimasi populasi: bandingkan spesies yang SAMA antarstasiun atau
# antarwaktu, jangan antarspesies (Sollmann dkk. 2013). Human ikut dihitung.
effort <- rowSums(camop, na.rm = TRUE)                       # hari-kamera per stasiun
effort

# Per stasiun x spesies (stasiun tanpa deteksi tetap muncul dengan n_event = 0)
rai_station <- as.data.frame(table(Station = factor(rec$Station, levels = names(effort)),
                                   Species = rec$Species),
                             responseName = "n_event", stringsAsFactors = FALSE)
rai_station$effort <- unname(effort[rai_station$Station])
rai_station$RAI    <- round(rai_station$n_event / rai_station$effort * 100, 2)
xtabs(RAI ~ Species + Station, rai_station)                  # tabel lebar: baris = spesies

# Per spesies, semua stasiun digabung
rai_species <- aggregate(n_event ~ Species, data = rai_station, FUN = sum)
rai_species$effort <- sum(effort)
rai_species$RAI    <- round(rai_species$n_event / rai_species$effort * 100, 2)
rai_species <- rai_species[order(-rai_species$RAI), ]
rai_species

write.csv(rai_station, "output/rai_per_stasiun.csv", row.names = FALSE)
write.csv(rai_species, "output/rai_per_spesies.csv", row.names = FALSE)


## ---- 2. Pola aktivitas harian ---------------------------------------------
dir.create("output/activity_plots", showWarnings = FALSE)

# Kurva aktivitas untuk semua spesies dengan minimal 30 rekaman, disimpan sebagai PNG
sp_ok <- names(n_by_sp)[n_by_sp >= 30]
for (sp in sp_ok) {
  png(file.path("output/activity_plots", paste0("activity_", sp, ".png")),
      width = 1600, height = 1200, res = 200)
  activityDensity(recordTable = rec, species = sp, plotR = TRUE)
  dev.off()
}


# Grafik tumpang tindih dua spesies
spA <- "Leopard_cat"          # UBAH
spB <- "Common_palm_civet"    # UBAH
activityOverlap(recordTable = rec, speciesA = spA, speciesB = spB,
                plotR = TRUE, addLegend = TRUE, legendPosition = "topleft")
# Simpan sebagai PNG di folder activity_plots
png(file.path("output/activity_plots", paste0("overlap_", spA, "_vs_", spB, ".png")),
    width = 1600, height = 1200, res = 200)
activityOverlap(recordTable = rec, speciesA = spA, speciesB = spB,
                plotR = TRUE, addLegend = TRUE, legendPosition = "topleft",
                main = paste("Activity overlap:", spA, "vs", spB))
dev.off()


## ---- 2b. Aktivitas manusia -------------------------------------------------
# "Human" diperlakukan seperti spesies: satu kejadian independen = rekaman manusia
# yang berjarak > 30 menit dari rekaman manusia sebelumnya di grid yang sama.
hum   <- "Human"
n_hum <- sum(rec$Species == hum)
n_hum

# Jumlah kejadian manusia per jam (lebih mudah dibaca daripada kurva jika rekaman sedikit)
hum_hour <- as.integer(substr(rec$Time[rec$Species == hum], 1, 2))
table(factor(hum_hour, levels = 0:23))
# Di grid mana manusia paling sering terekam?
sort(table(rec$Station[rec$Species == hum]), decreasing = TRUE)

# Kurva aktivitas manusia, dan tumpang tindih manusia vs setiap spesies satwa
# (dibuat juga jika < 30 rekaman, tetapi tafsirkan dengan hati-hati)
if (n_hum >= 10) {
  png(file.path("output/activity_plots", "activity_Human.png"),
      width = 1600, height = 1200, res = 200)
  activityDensity(recordTable = rec, species = hum, plotR = TRUE)
  dev.off()
  
  for (sp in setdiff(sp_ok, hum)) {
    png(file.path("output/activity_plots", paste0("overlap_Human_vs_", sp, ".png")),
        width = 1600, height = 1200, res = 200)
    activityOverlap(recordTable = rec, speciesA = hum, speciesB = sp,
                    plotR = TRUE, addLegend = TRUE, legendPosition = "topleft",
                    main = paste("Activity overlap:", hum, "vs", sp))
    dev.off()
  }
}