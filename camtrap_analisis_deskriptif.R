## Analisis data kamera jebak (camera trap)
## Dipersiapkan oleh: Kurnia Latifiana (k.latifiana@gmail.com)

## Pertanyaan
# 1. Spesies apa saja yang ditemukan?
# 2. Berapa jumlah foto independent event tiap spesies?
# 3. Spesies apa yang paling sering dan paling sedikit terekam?
# 4. Berapa jumlah foto independent event spesies X?
# 5. Di mana saja spesies X ditemukan?
# 6. Berapa jumlah individu (n_indv) tiap spesies? (langkah 2)


## ---- 0. Packages dan pengaturan ----------------------------------------------
# Jalankan sekali saja:
# install.packages(c("tidyverse", "leaflet", "sf", "ggspatial", "prettymapr"))

library(tidyverse)
library(sf)             # data spasial 
library(ggspatial)      # peta dasar, peta statis
library(leaflet)        # peta interaktif (optional)

getwd()

dir.create("output", showWarnings = FALSE)
dir.create("output/descriptive_plots", showWarnings = FALSE)

# Tabel kejadian independen dari camtrapR (hasil script data management, langkah 6).
# Satu baris = satu independent event; kolom n_images = jumlah foto dalam event itu;
# kolom n_indv = jumlah individu (nilai terbesar dari foto-foto dalam event itu).
rec <- read_csv("output/record_table_independent.csv",
                col_types = cols(Camera = col_character()))

# Tabel lama tanpa folder jumlah individu: anggap 1 individu per event
if (!"n_indv" %in% names(rec)) {
  warning("Kolom n_indv tidak ada: dianggap 1 individu per event")
  rec$n_indv <- 1L
}

# Tabel stasiun (koordinat, habitat). Satu baris per grid: jika satu grid punya
# beberapa kamera, koordinat kamera pertama yang dipakai.
stations <- read_csv("station_table.csv",
                     col_types = cols(StationID = col_character(),
                                      CamID     = col_character())) |>
  distinct(StationID, .keep_all = TRUE)

# "Human" ikut dihitung seperti spesies. Jika hanya ingin satwa, aktifkan baris ini:
# rec <- rec |> filter(Species != "Human")


## ---- 1. Spesies apa saja yang ditemukan? ----------------------------------
rec |> distinct(Species) |> arrange(Species)
n_distinct(rec$Species)                       # jumlah spesies


## ---- 2. Jumlah foto, independent event, dan individu tiap spesies ---------
summary <- rec |>
  group_by(Species) |>
  summarise(n_photo    = sum(n_images),          # jumlah foto
            n_event   = n(),                    # jumlah independent event
            n_indv    = sum(n_indv),        # total individu dari semua event
            n_station = n_distinct(Station)) |> # di berapa stasiun terekam
  arrange(desc(n_event))
summary

write_csv(summary, "output/summary_per_spesies.csv")

# Grafik batang (bukan histogram: sumbu-y berisi kategori spesies, bukan rentang nilai).
# Tiga panel berdampingan karena skala event, foto, dan individu berbeda. Urutan spesies
# sama di semua panel (berdasarkan event), jadi lonjakan satwa berkelompok terlihat.
plot_q2 <- summary |>
  mutate(Species = fct_reorder(str_replace_all(Species, "_", " "), n_event)) |>
  pivot_longer(c(n_event, n_photo, n_indv), names_to = "measure", values_to = "count") |>
  mutate(measure = factor(measure, levels = c("n_event", "n_photo", "n_indv"),
                          labels = c("Independent event", "n_photo", "n_indv"))) |>
  ggplot(aes(x = count, y = Species)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = count), hjust = -0.2, size = 3.2, colour = "#52514e") +  # angka di ujung batang
  facet_wrap(~ measure, scales = "free_x") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +                    # ruang untuk angka
  labs(title    = "Jumlah independent event, foto, dan individu per spesies",
       subtitle = "Diurutkan berdasarkan jumlah independent event",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y  = element_blank(),
        panel.grid.minor    = element_blank(),
        panel.grid.major.x  = element_line(colour = "#e1e0d9", linewidth = 0.3),
        strip.text          = element_text(face = "bold", hjust = 0),
        panel.spacing.x     = unit(2, "lines"),
        plot.title.position = "plot")
plot_q2

ggsave("output/descriptive_plots/q2_event_n_photo_per_spesies.png", plot_q2,
       width = 11, height = 4.5, dpi = 200, bg = "white")


## ---- 3. Spesies paling sering dan paling sedikit terekam ------------------
# Pakai jumlah EVENT, bukan jumlah foto: satwa berkelompok (monyet, babi hutan)
# memicu kamera berulang kali sehingga jumlah fotonya membengkak.
summary |> slice_max(n_event, n = 1)          # paling sering
summary |> slice_min(n_event, n = 1)          # paling sedikit

# Bandingkan: peringkat berdasarkan jumlah FOTO
summary |> slice_max(n_photo, n = 1)


## ---- 4. Jumlah foto dan independent event spesies X -----------------------
spX <- "Barking_deer"                         # UBAH: nama folder spesies
summary |> filter(Species == spX)


## ---- 5. Di mana saja spesies X ditemukan? --------------------------------
loc_X <- rec |>
  filter(Species == spX) |>
  group_by(Station) |>
  summarise(n_photo    = sum(n_images),
            n_event    = n(),
            n_indv     = sum(n_indv)) |>
  left_join(stations |> select(StationID, Habitat, Elevation_m, Longitude, Latitude),
            by = join_by(Station == StationID))
loc_X

# Data peta: SEMUA stasiun, termasuk yang tidak merekam spesies X (n_event = 0)
map_X <- stations |>
  select(Station = StationID, Habitat, Elevation_m, Longitude, Latitude) |>
  left_join(loc_X |> select(Station, n_photo, n_event, n_indv), by = join_by(Station)) |>
  mutate(n_photo = replace_na(n_photo, 0),
         n_event = replace_na(n_event, 0),
         n_indv  = replace_na(n_indv, 0)) |>
  arrange(desc(n_event))                      # titik besar digambar dulu

# 5a. Peta statis dengan citra satelit (PNG).
#     Perlu internet saat pertama dijalankan; tile disimpan di output/tile_cache
#     sehingga run berikutnya bisa tanpa internet.
map_X_sf <- map_X |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>   # WGS84
  st_transform(3857)                                              # Web Mercator (sama dengan tile)

# Batas peta = kotak di sekitar semua stasiun + 900 m
bb <- map_X_sf |> st_bbox() |> st_as_sfc() |> st_buffer(900) |> st_bbox()

# Citra satelit Google (lyrs=s; ganti lyrs=y untuk satelit + label jalan/nama tempat).
# URL Google tidak punya ekstensi file (.jpg), jadi didaftarkan dulu sebagai sumber
# tile dengan extension = "jpg" (paket rosm terpasang bersama ggspatial).
rosm::register_tile_source(google_sat = rosm:::source_from_url_format(
  "https://mt1.google.com/vt/lyrs=s&x=${x}&y=${y}&z=${z}",
  extension = "jpg", max_zoom = 20))

map_q5_sat <- ggplot(map_X_sf) +
  annotation_map_tile(type = "google_sat", zoomin = 0, progress = "none",
                      cachedir = "output/tile_cache") +
  geom_sf(data = filter(map_X_sf, n_event == 0),
          shape = 21, size = 3.5, stroke = 1, colour = "white", fill = "#898781") +
  geom_sf(data = filter(map_X_sf, n_event > 0), aes(size = n_event),
          shape = 21, stroke = 1, colour = "white", fill = "#2a78d6") +
  geom_sf_label(aes(label = paste0(Station, " (", n_event, ")")),
                nudge_x = 220, hjust = 0, size = 3, colour = "#0b0b0b",
                fill = "white", linewidth = 0, label.r = unit(2, "pt")) +
  scale_size_area(max_size = 12, name = "Independent event") +
  annotation_scale(location = "br", style = "ticks",
                   line_col = "white", text_col = "white") +
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(0.8, "cm"),
                         style = north_arrow_orienteering(fill = c("white", "grey20"),
                                                          text_col = "white")) +
  coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")],
           crs = 3857, expand = FALSE) +
  labs(title    = paste("Lokasi deteksi", str_replace_all(spX, "_", " ")),
       subtitle = "Angka = jumlah independent event; titik abu-abu = tidak terdeteksi",
       caption  = "Peta dasar: Google Satellite",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        plot.title.position = "plot")
map_q5_sat

ggsave(paste0("output/descriptive_plots/q5_map_satellite_", spX, ".png"), map_q5_sat,
       width = 7, height = 5.2, dpi = 200, bg = "white")

# 5b. Peta interaktif dengan peta dasar (perlu internet saat dibuka).
#     Arahkan kursor/klik titik untuk detail. Disimpan sebagai HTML, buka di browser.
map_q5_web <- leaflet(map_X) |>
  addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={z}",
           attribution = "Imagery &copy; Google", group = "Citra satelit") |>
  addProviderTiles(providers$OpenStreetMap,     group = "OpenStreetMap") |>
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                   radius       = ~if_else(n_event > 0, 5 + 3 * sqrt(n_event), 6),
                   fillColor    = ~if_else(n_event > 0, "#2a78d6", "#898781"),
                   fillOpacity  = 0.9, color = "white", weight = 2,
                   label        = ~paste0(Station, " (", n_event, ")"),
                   labelOptions = labelOptions(noHide = TRUE, direction = "right",
                                               offset = c(20, 0)),
                   popup        = ~paste0("<b>", Station, "</b><br>", Habitat,
                                          "<br>Elevasi: ", Elevation_m, " m",
                                          "<br>Independent event: ", n_event,
                                          "<br>Foto: ", n_photo,
                                          "<br>Individu: ", n_indv)) |>
  addLegend(colors  = c("#2a78d6", "#898781"),
            labels  = c("Terdeteksi (ukuran = jumlah event)", "Tidak terdeteksi"),
            title   = str_replace_all(spX, "_", " "), opacity = 0.9) |>
  addLayersControl(baseGroups = c("Citra satelit", "OpenStreetMap"),
                   options    = layersControlOptions(collapsed = FALSE)) |>
  addScaleBar(position = "bottomleft")
map_q5_web

html_q5 <- paste0("output/descriptive_plots/q5_map_", spX, ".html")
htmlwidgets::saveWidget(map_q5_web, html_q5, selfcontained = TRUE)
unlink(sub("[.]html$", "_files", html_q5), recursive = TRUE)   # hapus folder sisa (HTML sudah mandiri)

# Stasiun yang TIDAK merekam spesies X
stations |>
  anti_join(loc_X, by = join_by(StationID == Station)) |>
  select(StationID, Habitat)

# Semua spesies x stasiun sekaligus (jumlah independent event; 0 = tidak terekam)
rec |>
  count(Species, Station) |>
  pivot_wider(names_from = Station, values_from = n, values_fill = 0, names_sort = TRUE)
# Catatan: jumlah event antar stasiun belum memperhitungkan lama kamera aktif
# (misalnya ST003 mati 10-20 April).

# Grafik batang berkelompok: x = stasiun, y = independent event, warna = spesies.
# Palet 8 warna yang aman untuk buta warna; urutan warna mengikuti urutan spesies
# (event terbanyak dulu). Lebih dari 8 spesies: gabungkan yang jarang menjadi "Other".
sp_colours <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                "#e87ba4", "#008300", "#6250d6", "#e34948")

plot_q5_all <- rec |>
  count(Species, Station) |>
  complete(Species, Station = stations$StationID, fill = list(n = 0)) |>  # stasiun tanpa deteksi = 0
  mutate(Species = fct_reorder(str_replace_all(Species, "_", " "), n,
                               .fun = sum, .desc = TRUE)) |>               # urut: event terbanyak dulu
  ggplot(aes(x = Station, y = n, fill = Species)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.85,
           colour = "white", linewidth = 0.3) +                             # garis putih pemisah batang
  geom_text(aes(label = n), position = position_dodge(width = 0.85),
            vjust = -0.5, size = 2.6, colour = "#52514e") +                 # angka di ujung batang
  scale_fill_manual(values = sp_colours, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title    = "Jumlah independent event per stasiun dan spesies",
       subtitle = "0 = tidak terekam di stasiun tersebut",
       x = NULL, y = "Independent event") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.x  = element_blank(),
        panel.grid.minor    = element_blank(),
        panel.grid.major.y  = element_line(colour = "#e1e0d9", linewidth = 0.3),
        legend.position     = "top",
        legend.justification = "left",
        plot.title.position = "plot") +
  guides(fill = guide_legend(nrow = 1))
plot_q5_all

ggsave("output/descriptive_plots/q5_event_per_station_all_species.png", plot_q5_all,
       width = 11, height = 5.5, dpi = 200, bg = "white")
