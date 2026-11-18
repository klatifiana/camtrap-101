###############################################################################
## Data management kamera jebak: struktur folder GRID / KAMERA, Bushnell JPG + MOV
## Dipersiapkan oleh: Kurnia Latifiana (k.latifiana@gmail.com)
##
## Struktur data mentah:
##   1_raw/
##     Q7/                    <- lokasi grid = kolom "StationID" (stasiun di camtrapR)
##       207/                 <- ID kamera   = "CamID"  di camtrapR
##         100EK113/          <- folder buatan kamera (nama dan kedalaman bebas)
##           IMG_0001.JPG, IMG_0002.MOV ...
##       258/
##         100EK113/ ...
##
## Jika satu kamera punya beberapa kali pengambilan kartu SD, simpan setiap
## pengecekan di subfolder sendiri, misalnya: 207/check01/100EK113/
##
## Skrip ini membuat dua folder baru di sebelahnya:
##   2_renamed/Q7/207/Q7_207_100EK113_IMG_0001.JPG    (nama unik)
##   3_sorted/ Q7/207/Leopard_cat/02/...              (spesies / jumlah individu, dipilah manual)
###############################################################################

## ---- 0. Packages dan pengaturan ----------------------------------------------
# Jalankan sekali saja:
# install.packages(c("camtrapR", "activity", "overlap"))

library(camtrapR) 
library(activity)
library(overlap)

getwd()

# setwd("./1_raw")   # UBAH: folder yang berisi data original

raw_dir     <- "1_raw"
renamed_dir <- "2_renamed"
sorted_dir  <- "3_sorted"

dir.create("output")

tz <- "Asia/Jakarta"             # WIB
media_ext <- c("jpg", "jpeg", "mov", "mp4")

# Apakah kamera-kamera dalam satu grid mengarah ke TITIK YANG SAMA (kamera berpasangan)?
#   TRUE  -> satwa yang terekam kedua kamera dalam 30 menit dihitung sekali per grid
#   FALSE -> setiap kamera dianggap titik sampel yang independen
cams_same_spot <- TRUE           # UBAH sesuai desain lapangan Anda

# Nama folder yang BUKAN spesies (tidak dipakai dalam analisis aktivitas)
# "Human" sengaja TIDAK dikecualikan: aktivitas manusia ikut dianalisis (langkah 8b)
exclude_sp <- c("Empty", "Vehicle", "Unknown", "Unidentified")

# Folder TANPA subfolder jumlah individu (file langsung di folder ini).
# Empty TETAP punya subfolder: 00 (0 individu).
no_count_sp <- c("Unknown", "Unidentified")

# ExifTool (membaca tanggal/jam di dalam foto dan video)
#  - Windows: exiftoolPath(exiftoolDir = "C:/exiftool")
#  - Mac: biasanya sudah terdeteksi
Sys.which("exiftool")            # hasilnya tidak boleh ""


## ---- 1. Periksa struktur folder mentah -----------------------------------
grids <- list.dirs(raw_dir, recursive = FALSE, full.names = FALSE)
length(grids)                    # jumlah lokasi grid

cams <- do.call(rbind, lapply(grids, function(g) {
  cc <- list.dirs(file.path(raw_dir, g), recursive = FALSE, full.names = FALSE)
  if (length(cc) == 0) return(NULL)
  data.frame(StationID = g, CamID = cc)
}))
nrow(cams)                       # jumlah kombinasi grid-kamera
table(cams$StationID)              # jumlah kamera per grid

# Grid yang tidak punya folder kamera (periksa!)
setdiff(grids, cams$StationID)


## ---- 2. Tabel stasiun (satu baris per KAMERA) -----------------------------
# Kolom: StationID, CamID, Longitude, Latitude, Setup_date, Retrieval_date,
#        Problem1_from, Problem1_to, Habitat
# "StationID" dipakai sebagai stasiun di camtrapR (stationCol = "StationID").
# Catatan: di hasil camtrapR (tabel rekaman) kolomnya bernama "Station" dan "Camera".
# Hanya pertama kali: membuat template dengan kolom StationID + CamID sudah terisi.
# Buka di Excel lalu isi koordinat dan tanggal (format YYYY-MM-DD).
template_csv <- "station_table.csv"
if (!file.exists(template_csv)) {
  tmpl <- cams
  tmpl[c("Longitude", "Latitude", "Elevation_m", "Habitat", "Setup_date", "Retrieval_date",
         "Problem1_from", "Problem1_to")] <- ""
  write.csv(tmpl, template_csv, row.names = FALSE, na = "")
  message("Template dibuat: isi ", template_csv, " lalu jalankan lagi mulai langkah 2.")
}

# Dibaca sebagai teks agar ID kamera seperti "007" tidak kehilangan angka nol di depan
stations <- read.csv(template_csv, colClasses = "character", na.strings = c("", "NA"))
stations$Longitude <- as.numeric(stations$Longitude)
stations$Latitude  <- as.numeric(stations$Latitude)

# Setiap folder harus ada di tabel, dan setiap baris tabel harus punya folder
key_tab <- paste(stations$StationID, stations$CamID, sep = "/")
key_dir <- paste(cams$StationID, cams$CamID, sep = "/")
setdiff(key_dir, key_tab)        # folder yang belum ada di tabel
setdiff(key_tab, key_dir)        # baris tabel tanpa folder
stopifnot(!any(duplicated(key_tab)))
has_problems <- any(!is.na(stations$Problem1_from))


## ---- 3. Ganti nama dan salin (foto DAN video) -----------------------------
# Nama baru = StationID_CamID_<subfolder kamera>_<nama asli>
#   CT_data_original/Q7/207/100EK113/IMG_0001.JPG
#   -> CT_data_renamed/Q7/207/Q7_207_100EK113_IMG_0001.JPG
# Bushnell mengulang penomoran setiap kartu diformat. Jika ada beberapa kali
# pengecekan, simpan di subfolder terpisah (misalnya 207/check01/100EK113/);
# nama subfolder otomatis menjadi bagian dari nama file baru.
rename_media <- function(raw_dir, out_dir, ext) {
  pattern <- paste0("\\.(", paste(ext, collapse = "|"), ")$")
  files <- list.files(raw_dir, pattern = pattern, ignore.case = TRUE,
                      recursive = TRUE, full.names = FALSE)
  files <- files[!grepl("^\\._", basename(files))]     # abaikan file bantu Mac "._"
  parts <- strsplit(files, "/", fixed = TRUE)
  ok_depth <- lengths(parts) >= 3                      # harus di dalam grid/kamera/file
  if (any(!ok_depth)) {
    warning(sum(!ok_depth), " file tidak berada di folder StationID/CamID dan dilewati:\n",
            paste(head(files[!ok_depth]), collapse = "\n"))
    files <- files[ok_depth]; parts <- parts[ok_depth]
  }
  station  <- vapply(parts, `[`, "", 1)
  camera   <- vapply(parts, `[`, "", 2)
  rest     <- vapply(parts, function(p) paste(p[-(1:2)], collapse = "_"), "")
  new_name <- paste(station, camera, rest, sep = "_")
  dest_dir <- file.path(out_dir, station, camera)
  if (any(duplicated(file.path(dest_dir, new_name)))) stop("Ada nama baru yang ganda - periksa susunan folder")
  for (d in unique(dest_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  copied <- file.copy(file.path(raw_dir, files), file.path(dest_dir, new_name),
                      overwrite = FALSE, copy.date = TRUE)
  data.frame(StationID = station, CamID = camera, original = files,
             renamed = new_name, copied = copied)
}

# Jalankan sekali ke folder CT_data_renamed yang masih KOSONG
rename_log <- rename_media(raw_dir, renamed_dir, media_ext)
write.csv(rename_log, "output/rename_log.csv", row.names = FALSE)

table(rename_log$copied)                                   # semua TRUE?
files_per_cam <- aggregate(original ~ StationID + CamID, rename_log, length)
names(files_per_cam)[3] <- "n_files"
files_per_cam[order(files_per_cam$n_files), ]              # kamera dengan file sangat sedikit?
table(tolower(tools::file_ext(rename_log$renamed)))        # jumlah foto vs video


## ---- 4. Periksa tanggal/jam SEBELUM pemilahan -----------------------------
# Foto: metadata JPG pertama
exifTagNames(inDir = renamed_dir, returnMetadata = TRUE)

# Video: semua tag waktu dari satu file MOV
exiftool_cmd <- Sys.which("exiftool")
one_mov <- rename_log$renamed[grepl("\\.mov$", rename_log$renamed, ignore.case = TRUE)][1]
if (!is.na(one_mov)) {
  r <- rename_log[rename_log$renamed == one_mov, ]
  system2(exiftool_cmd, c("-time:all", "-a", "-G0:1", "-s",
                          shQuote(file.path(renamed_dir, r$StationID, r$CamID, one_mov))))
}
# Bandingkan dengan tanggal/jam yang tertulis di banner video.
# - Biasanya QuickTime:CreateDate sama dengan banner (waktu lokal, WIB).
# - Jika video tepat 7 jam lebih awal, berarti tersimpan dalam UTC. Perbaiki (sekali saja!):
# system2(exiftool_cmd, c("-overwrite_original", "-ext", "mov", "-r",
#                         "-QuickTime:CreateDate+=0:0:0 7:0:0", shQuote(renamed_dir)))

## 4b. Tanggal di luar periode pemasangan? (semua file, SEBELUM pemilahan)
ex <- read.csv(text = system2(exiftool_cmd, c("-csv", "-r", "-ext", "jpg", "-ext", "mov",
                 "-DateTimeOriginal", "-CreateDate", shQuote(renamed_dir)), stdout = TRUE),
               colClasses = "character")
if (is.null(ex$CreateDate)) ex$CreateDate <- ""
dt <- as.POSIXct(ifelse(nzchar(ex$DateTimeOriginal), ex$DateTimeOriginal, ex$CreateDate),
                 format = "%Y:%m:%d %H:%M:%S", tz = tz)
p  <- strsplit(sub(paste0("^", renamed_dir, "/"), "", ex$SourceFile), "/")
i  <- match(paste(sapply(p, `[`, 1), sapply(p, `[`, 2)), paste(stations$StationID, stations$CamID))
luar <- dt < as.POSIXct(stations$Setup_date[i], tz = tz) |
        dt >= as.POSIXct(stations$Retrieval_date[i], tz = tz) + 86400
ex[which(luar), c("SourceFile", "DateTimeOriginal")]   # harus kosong

# Jam kamera salah (misalnya salah AM/PM, atau ter-reset karena baterai habis)?
# Jika ada baris: perbaiki dengan timeShiftImages di bawah, lalu jalankan 4b lagi.

# # Koreksi FOTO dari satu kamera:
# ts <- data.frame(StationID = "ST004", CamID = "438",
#                  timeshift = "0000:00:00 12:00:00", sign = "+")
# timeShiftImages(inDir = renamed_dir, timeShiftTable = ts,
#                 stationCol = "StationID", cameraCol = "CamID", hasCameraFolders = TRUE,
#                 timeShiftColumn = "timeshift", timeShiftSignColumn = "sign")
# # ...lalu VIDEO-nya dengan ExifTool pada folder kamera tersebut:
# system2(exiftool_cmd, c("-overwrite_original", "-ext", "mov",
#                         "-QuickTime:CreateDate+=0:0:0 12:0:0",
#                         shQuote(file.path(renamed_dir, "ST004", "438"))))
# 
# # Hapus file cadangan "*_original" dari ExifTool (aslinya tetap ada di 1_raw):
# file.remove(list.files(renamed_dir, pattern = "_original$", recursive = TRUE, full.names = TRUE))
# # jalankan 4b lagi.


## ---- 5. Identifikasi spesies (manual) -----------------------------------
# 5a. Salin file hasil ganti nama ke folder baru untuk dipilah (2_renamed tetap jadi cadangan)
if (!dir.exists(sorted_dir)) {
  dir.create(sorted_dir)
  file.copy(list.files(renamed_dir, full.names = TRUE), sorted_dir,
            recursive = TRUE, copy.date = TRUE)
}

# 5b. Buat folder spesies yang sama di SETIAP folder kamera
species_list <- c("Leopard_cat", "Common_palm_civet", "Long-tailed_macaque",
                  "Barking_deer", "Wild_boar", "Javan_mongoose",
                  "Human", "Empty", "Unknown")          # UBAH sesuai daftar spesies Anda
createSpeciesFolders(inDir = sorted_dir, species = species_list,
                     hasCameraFolders = TRUE)

# Folder jumlah individu di dalam setiap folder spesies (01 = 1 individu, 02 = 2, ...)
# Lebih dari 5? Buat sendiri folder dengan 2 digit, misalnya "07" atau "12".
# Empty hanya diberi folder 00; Unknown (no_count_sp) tidak diberi folder jumlah.
count_folders <- sprintf("%02d", 1:5)
sp_paths <- list.dirs(sorted_dir, recursive = TRUE)
sp_paths <- sp_paths[lengths(strsplit(sp_paths, "/")) == 4]         # 3_sorted/grid/kamera/spesies
for (d in sp_paths) {
  if (basename(d) == "Empty") {
    dir.create(file.path(d, "00"), showWarnings = FALSE)
  } else if (!basename(d) %in% c(exclude_sp, no_count_sp)) {
    for (n in count_folders) dir.create(file.path(d, n), showWarnings = FALSE)
  }
}

# # Hanya untuk data yang SUDAH dipilah tanpa folder jumlah (file langsung di folder
# # spesies): pindahkan sekali saja: Empty -> 00/, spesies lain -> 01/, lalu drag
# # file berisi lebih dari 1 individu ke 02/03/... secara manual.
# old <- list.files(sorted_dir, pattern = "\\.(jpg|jpeg|mov|mp4)$", ignore.case = TRUE,
#                   recursive = TRUE)
# old_sp <- sapply(strsplit(old, "/"), `[`, 3)
# keep   <- lengths(strsplit(old, "/")) == 4 & !old_sp %in% no_count_sp
# old <- old[keep]; old_sp <- old_sp[keep]
# nn  <- ifelse(old_sp == "Empty", "00", "01")
# for (nd in unique(file.path(sorted_dir, dirname(old), nn))) dir.create(nd, showWarnings = FALSE)
# file.rename(file.path(sorted_dir, old),
#             file.path(sorted_dir, dirname(old), nn, basename(old)))

# 5c. Pilah secara manual (di luar R):
#   - Buka 3_sorted/<StationID>/<CamID>/ di Finder atau File Explorer.
#   - PINDAHKAN (bukan salin) setiap file ke SATU folder spesies.
#     Pindahkan semua foto burst dan video dari pemicu yang sama sekaligus.
#   - Lalu PINDAHKAN ke folder jumlah individu di dalam folder spesies:
#     01 = 1 individu, 02 = 2 individu, dst. Hitung individu yang TERLIHAT di file itu.
#     Foto burst boleh beda folder (misal foto 1 -> 01, foto 2 -> 03);
#     jumlah per kejadian diambil dari nilai terbesar (langkah 6).
#   - Empty -> Empty/00/. Unknown tidak perlu folder jumlah.
#   - Dua spesies dalam satu gambar? Salin file itu ke folder jumlah kedua spesies.
#   - Ragu? Masukkan ke "Unknown" dan tanyakan ke ahli kemudian.
#   - Gunakan nama folder yang persis sama: ejaan peka huruf besar-kecil
#     (Leopard_cat dan leopard_cat dianggap dua spesies). Tanpa spasi.

# 5d. Kemajuan: file yang belum dipilah
pattern <- paste0("\\.(", paste(media_ext, collapse = "|"), ")$")
all_media <- list.files(sorted_dir, pattern = pattern, ignore.case = TRUE, recursive = TRUE)
parts <- strsplit(all_media, "/")
depth <- lengths(parts)              # 3 = belum dipilah, 4 = folder spesies, 5 = folder jumlah
sp_of <- sapply(parts, `[`, 3)       # nama folder spesies
unsorted  <- all_media[depth == 3]                             # belum masuk folder spesies
uncounted <- all_media[depth == 4 & !sp_of %in% no_count_sp]   # sudah spesies, belum folder jumlah
length(unsorted)                                # harus mencapai 0
length(uncounted)                               # harus mencapai 0
if (length(unsorted)  > 0) table(dirname(unsorted))    # kamera mana yang belum selesai
if (length(uncounted) > 0) table(dirname(uncounted))   # folder spesies yang belum dihitung

# 5e. Pemeriksaan mutu: orang kedua mengidentifikasi ulang sampel acak 5%
sorted_files <- all_media[depth == 5 | (depth == 4 & sp_of %in% no_count_sp)]
set.seed(2026)
qc_files <- sample(sorted_files, ceiling(0.05 * length(sorted_files)))
qc_p     <- strsplit(qc_files, "/")
write.csv(data.frame(File = qc_files, Species_observer2 = "", n_indv_observer2 = ""),
          "output/qc_check_sheet.csv", row.names = FALSE)    # berikan ke orang kedua
write.csv(data.frame(File = qc_files,
                     Species_observer1    = sapply(qc_p, `[`, 3),
                     n_indv_observer1 = suppressWarnings(as.integer(sapply(qc_p, `[`, 4)))),
          "output/qc_key.csv", row.names = FALSE)            # simpan, jangan diperlihatkan
# Catatan: nama folder ada di kolom File; untuk QC yang buta, berikan salinan file
# tanpa folder kepada orang kedua.
# Setelah orang kedua mengisi qc_check_sheet.csv:
# chk <- read.csv("output/qc_check_sheet.csv"); key <- read.csv("output/qc_key.csv")
# mean(chk$Species_observer2 == key$Species_observer1)        # kesesuaian spesies (target > 0,95)
# key[chk$Species_observer2 != key$Species_observer1, ]       # diskusikan file-file ini
# ok <- !is.na(key$n_indv_observer1)
# mean(chk$n_indv_observer2[ok] == key$n_indv_observer1[ok])   # kesesuaian jumlah individu
# key[ok & chk$n_indv_observer2 != key$n_indv_observer1, ]

# 5f. Setelah selesai: hapus folder kosong dan periksa nama
sp_dirs <- list.dirs(sorted_dir, recursive = TRUE, full.names = FALSE)
n_dirs  <- sp_dirs[lengths(strsplit(sp_dirs, "/")) == 4]           # grid/kamera/spesies/jumlah
sp_dirs <- sp_dirs[lengths(strsplit(sp_dirs, "/")) == 3]           # grid/kamera/spesies
sort(table(basename(sp_dirs)))                                      # cari salah ketik
n_dirs[!grepl("^[0-9]+$", basename(n_dirs))]                        # folder jumlah harus angka; harus kosong
# # Hapus folder jumlah yang kosong DULU, lalu folder spesies yang kosong:
# for (d in file.path(sorted_dir, n_dirs))
#   if (length(list.files(d, recursive = TRUE)) == 0) unlink(d, recursive = TRUE)
# createSpeciesFolders(inDir = sorted_dir, species = species_list,
#                      hasCameraFolders = TRUE, removeFolders = TRUE)


## ---- 6. Tabel rekaman (kejadian independen) + jumlah individu ------------
# Susunan: 3_sorted/<StationID>/<CamID>/<Species>/<NN>/file  (NN = jumlah individu)
# camtrapR membaca folder TERAKHIR sebagai spesies (di sini "01", "02", ...), jadi:
#   6a. baca tanggal/jam SEMUA file, lalu ambil kamera, spesies, dan n_indv dari path
#   6b. hitung kejadian independen dengan filterRecordTable (aturan sama: 30 menit)

## 6a. Tanggal/jam semua file (belum difilter independensi)
rec_all <- recordTable(inDir                  = sorted_dir,
                       IDfrom                 = "directory",
                       minDeltaTime           = 0,
                       timeZone               = tz,
                       removeDuplicateRecords = FALSE,
                       video = list(file_formats = c("jpg", "mov"),
                                    dateTimeTag  = "QuickTime:CreateDate"))  # sesuaikan dengan langkah 4
p <- strsplit(sub(paste0("^", sorted_dir, "/"), "", rec_all$Directory), "/")
rec_all$Camera     <- sapply(p, `[`, 2)
rec_all$Species    <- sapply(p, `[`, 3)
rec_all$n_indv <- suppressWarnings(as.integer(sapply(p, `[`, 4)))
rec_all$depth      <- lengths(p)
rec_all <- rec_all[!rec_all$Species %in% exclude_sp, ]

# File yang belum masuk folder jumlah, atau nama folder bukan angka (harus kosong)
cek <- rec_all[rec_all$depth != 4 | is.na(rec_all$n_indv) | rec_all$n_indv < 1,
               c("Directory", "FileName")]
cek
stopifnot(nrow(cek) == 0)

## 6b. Kejadian independen: >= 30 menit dari rekaman independen terakhir
# Foto burst dengan detik yang sama dianggap duplikat dan hanya satu yang disimpan:
# urutkan dulu agar yang disimpan adalah foto dengan individu terbanyak.
rec_all <- rec_all[order(-rec_all$n_indv), ]
rec <- filterRecordTable(recordTable          = rec_all[, c("Station", "Camera", "Species",
                                                            "DateTimeOriginal", "Directory",
                                                            "FileName", "n_indv")],
                         minDeltaTime         = 30,                   # menit
                         deltaTimeComparedTo  = "lastIndependentRecord",
                         stationCol           = "Station",
                         cameraCol            = "Camera",
                         camerasIndependent   = !cams_same_spot,
                         timeZone             = tz,
                         eventSummaryColumn   = "n_indv",
                         eventSummaryFunction = "max")    # jumlah individu kejadian = nilai terbesar
rec <- as.data.frame(rec)
rec$n_indv <- rec$n_indv_max                      # ganti nilai foto pertama dengan maksimum
rec <- rec[, c("Station", "Camera", "Species", "DateTimeOriginal", "Date", "Time",
               "delta.time.secs", "delta.time.mins", "delta.time.hours", "delta.time.days",
               "Directory", "FileName", "n_images", "n_indv")]
table(rec$Species, rec$n_indv)                        # sebaran ukuran kelompok per spesies

write.csv(rec, "output/record_table_independent.csv", row.names = FALSE)

# Pemeriksaan: rekaman di luar periode pemasangan tiap kamera = jam kamera salah?
rec_dt <- as.POSIXct(rec$DateTimeOriginal, tz = tz)
idx <- match(paste(rec$Station, rec$Camera), paste(stations$StationID, stations$CamID))
outside <- rec_dt < as.POSIXct(stations$Setup_date[idx], tz = tz) |
           rec_dt > as.POSIXct(stations$Retrieval_date[idx], tz = tz) + 86400
rec[which(outside), c("Station", "Camera", "Species", "DateTimeOriginal", "FileName")]

