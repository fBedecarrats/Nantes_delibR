library(readr)
library(purrr)
library(pdftools)
library(tesseract)
library(jpeg)
library(stringr)
library(dplyr)

clean_logo_VdN <- function(x) {
  x <- str_replace(x, "(^|\n)( \n(\n)?)?v?(V|v|Y)(I|i).*\n(\n)?.*(N|W)( ?)an?.*\n", 
                   "Ville de Nantes")
  x <- str_replace(x, "^VILLE DE(\n\n)?.{1,9}es.?", "Ville de Nantes")
  x <- str_replace(x, "^Nan.{1,9}es.?", "Ville de Nantes")
  x <- str_replace(x, "^nt (D|B)(E|É)(\n\nN?Nant ?es)?", 
                   "Ville de Nantes")
  return(x)
}

clean_logo_NM <- function(x) {
  x <- str_replace(x, "^.?.?( {0,5})?\n? ? ?\n?\n?.? ?Nantes", "Nantes")
  x <- str_replace(x, "Nantes\n\nNES ol", "Nantes\nMétropole")
  return(x)
}

str_sub(consolidated$txt, 1, 20) %>%
  str_replace("^.?.?( {0,5})?\n? ? ?\n?\n?.? ?Nantes", "Nantes") %>%
  str_replace("Nantes\n\nNES ol", "Nantes\nMétropole")

folder <- "delibs/02_with_urls"
files <- list.files(folder, full.names = TRUE)
consolidated <- map_dfr(files, read_csv, 
                        col_types = cols(.default = col_character()))
# write_csv2(consolidated, paste0(folder, "/consolidated.csv"))

engine <- tesseract(language = "fra",
                    options = list(tessedit_pageseg_mode = "1"))
n <- nrow(consolidated)
for (i in 1:n) {
  if (is.na(consolidated$`Numéro de l'acte`[i])) { next }
  pdf <- consolidated$`URL de la délibération`[i]
  np <- pdf_info(pdf)[["pages"]]
  txtout <- vector(length = np)
  vdn <- str_detect(consolidated$`Numéro de l'acte`[i], "CM")
  nm <- str_detect(consolidated$`Numéro de l'acte`[i], "DC")
  for (j in 1:np) {
    print(paste0("Delib ", i, "/", n, "; page ", j, "/", np))
    image <- pdf_render_page(pdf = pdf, page = j, dpi = 300)
    t <- ocr(writeJPEG(image), engine = engine)
    t <- ifelse(vdn, clean_logo_VdN(t), t)
    t <- ifelse(nm, clean_logo_NM(t), t)
    txtout[j] <- t
  }
  consolidated$txt[i] = ifelse(is.na(consolidated$`Numéro de l'acte`[i]), NA,
                               paste(txtout, collapse = "\n"))
}


consolidated$txt <- ifelse()

consolidated <- consolidated %>%
  mutate(txt = str_replace(txt, "(\n|^). Direction", "\nDirection"),
         txt = str_replace(txt, "(\n|^). Secrétariat", "\nSecrétariat"),
         txt = str_replace_all(txt, "\nDélibération.{1,5}\n",
                               "\nDélibération\n"),
         txt = str_remove_all(txt, "Accusé de réception en préfecture(\n){1,2}.*\n"),
         txt = str_remove_all(txt, "Date de télétransmission.*\n"),
         txt = str_remove_all(txt, "Date de réception.*\n"))

save(consolidated, file = "consolidated.Rdata")
# load(paste0(folder, "/consolidated.Rdata"))
write_excel_csv(consolidated, na = "",
                file = "Delibs_consolidees.csv")

