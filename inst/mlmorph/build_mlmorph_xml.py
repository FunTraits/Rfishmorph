#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
build_mlmorph_xml.py

Convertit les fichiers TPS produits par prepare_mlmorph_training.R
(train.tps / val.tps / test.tps) en fichiers XML dlib exploitables par
ml-morph (shape_trainer.py, detector_trainer.py, shape_tester.py).

Point clé — conventions d'axe :
  * TPS (sortie de l'étape R)   : origine BAS-gauche, Y vers le haut.
  * XML dlib / image / ml-morph : origine HAUT-gauche, Y vers le bas.
Le TPS a été obtenu à partir des coordonnées ImageJ (haut-gauche) par
Y_tps = hauteur - Y_imagej. On applique donc l'inverse ici :
Y_img = hauteur - Y_tps, ce qui restitue exactement les coordonnées image
d'origine. La hauteur de chaque image est lue dans split_assignments.csv.

Chaque image reçoit une "box" (rectangle englobant) calculée à partir de
l'étendue des landmarks + une marge relative ; elle sert de cible au
détecteur d'objet et d'ancrage au prédicteur de forme.

Usage :
  python3 build_mlmorph_xml.py \
      --dataset-dir mlmorph_dataset \
      --photo-dir  T_26_LaSaudrune/Photos \
      --out-dir    mlmorph_run \
      --box-pad 0.15
"""
import argparse
import csv
import os
import sys
import xml.etree.ElementTree as ET
from xml.dom import minidom

N_LM = 21


def parse_tps(path):
    """Lit un TPS (bloc LM=/coords/IMAGE=/ID=) -> liste de dicts."""
    specimens = []
    cur = None
    with open(path, "r") as fh:
        for raw in fh:
            line = raw.strip()
            if not line:
                continue
            if line.startswith("LM="):
                if cur is not None:
                    specimens.append(cur)
                cur = {"coords": [], "image": None, "id": None}
            elif line.startswith("IMAGE="):
                cur["image"] = line.split("=", 1)[1]
            elif line.startswith("ID="):
                cur["id"] = line.split("=", 1)[1]
            elif line.startswith("SCALE="):
                continue
            else:
                parts = line.split()
                if len(parts) == 2:
                    cur["coords"].append((float(parts[0]), float(parts[1])))
        if cur is not None:
            specimens.append(cur)
    # Contrôle de cohérence : tous les spécimens ont le même nombre de landmarks
    if specimens:
        n = len(specimens[0]["coords"])
        for s in specimens:
            if len(s["coords"]) != n:
                raise ValueError(
                    f"{os.path.basename(path)} : {s['id']} a "
                    f"{len(s['coords'])} landmarks (attendu {n})")
    return specimens


def load_heights(split_csv):
    """code -> hauteur_px depuis split_assignments.csv."""
    heights = {}
    with open(split_csv, newline="") as fh:
        for row in csv.DictReader(fh):
            h = row.get("height_px", "")
            if h not in ("", "NA"):
                heights[row["code"]] = float(h)
    return heights


def try_image_size(photo_dir, image_name):
    """(w, h) via Pillow si dispo, sinon (None, None)."""
    try:
        from PIL import Image
        Image.MAX_IMAGE_PIXELS = None  # grandes images alignées : pas d'alerte "bomb"
        with Image.open(os.path.join(photo_dir, image_name)) as im:
            return im.size  # (w, h)
    except Exception:
        return (None, None)


def build_box(coords_img, pad, img_w, img_h, fixed_dims=None):
    """
    Box (haut-gauche). Deux modes :
      * fixed_dims=(W,H) : box de taille FIXE (même ratio pour tous), centrée sur
        le barycentre des landmarks — requis pour le détecteur HOG. Le centre est
        décalé au besoin pour rester dans l'image (taille/ratio préservés).
      * sinon : rectangle englobant les landmarks + marge relative (box serrée).
    """
    xs = [c[0] for c in coords_img]
    ys = [c[1] for c in coords_img]
    if fixed_dims is not None:
        width, height = fixed_dims
        cx = sum(xs) / len(xs); cy = sum(ys) / len(ys)
        left = cx - width / 2.0; top = cy - height / 2.0
        left = max(0.0, left); top = max(0.0, top)
        if img_w is not None:
            left = min(left, max(0.0, img_w - width))
        if img_h is not None:
            top = min(top, max(0.0, img_h - height))
        return int(round(left)), int(round(top)), int(round(width)), int(round(height))
    x0, x1 = min(xs), max(xs)
    y0, y1 = min(ys), max(ys)
    w, h = x1 - x0, y1 - y0
    px, py = pad * w, pad * h
    left = max(0.0, x0 - px); top = max(0.0, y0 - py)
    width = w + 2 * px; height = h + 2 * py
    if img_w is not None:
        width = min(width, img_w - left)
    if img_h is not None:
        height = min(height, img_h - top)
    return int(round(left)), int(round(top)), int(round(width)), int(round(height))


def landmark_extent(coords_img):
    xs = [c[0] for c in coords_img]; ys = [c[1] for c in coords_img]
    return max(xs) - min(xs), max(ys) - min(ys)


def compute_fixed_dims(all_specimens, heights, pad):
    """Taille de box commune = médiane des étendues (robuste aux outliers) + marge.
    Retourne (W, H) et la liste des spécimens à étendue verticale anormale (QC)."""
    ws, hs, ext = [], [], {}
    for s in all_specimens:
        code = s["id"]; H = heights.get(code)
        if H is None:
            continue
        coords = [(x, H - y) for (x, y) in s["coords"]]
        w, h = landmark_extent(coords)
        ws.append(w); hs.append(h); ext[code] = (w, h)
    ws.sort(); hs.sort()
    med_w = ws[len(ws) // 2]; med_h = hs[len(hs) // 2]
    W = med_w * (1 + 2 * pad); H = med_h * (1 + 2 * pad)
    outliers = [c for c, (w, h) in ext.items() if h > 2 * med_h or w > 2 * med_w]
    return (W, H), med_h, outliers


def write_xml(specimens, heights, photo_dir, out_path, pad, fixed_dims=None):
    dataset = ET.Element("dataset")
    # Texte volontairement ASCII pur : le parseur XML de dlib ne gere pas les
    # references numeriques (&#8212; etc.) et s'arrete avec "fatal error".
    ET.SubElement(dataset, "name").text = "T-26 La Saudrune ml-morph"
    ET.SubElement(dataset, "comment").text = (
        "Genere par build_mlmorph_xml.py - coords image (haut-gauche).")
    images_el = ET.SubElement(dataset, "images")

    n_ok, n_skip = 0, 0
    abs_photo = os.path.abspath(photo_dir)
    for s in specimens:
        code = s["id"]
        img_name = s["image"]
        if code not in heights:
            print(f"  [skip] {code} : hauteur inconnue", file=sys.stderr)
            n_skip += 1
            continue
        H = heights[code]
        img_path = os.path.join(abs_photo, img_name)
        if not os.path.exists(img_path):
            print(f"  [skip] {code} : image absente ({img_name})", file=sys.stderr)
            n_skip += 1
            continue
        # Un-flip : TPS (bas-gauche) -> image (haut-gauche)
        coords_img = [(x, H - y) for (x, y) in s["coords"]]
        img_w, img_h = try_image_size(photo_dir, img_name)
        if img_h is None:
            img_h = H  # cohérent avec la hauteur du flip
        left, top, width, height = build_box(coords_img, pad, img_w, img_h,
                                             fixed_dims=fixed_dims)

        image_el = ET.SubElement(images_el, "image", file=img_path)
        box_el = ET.SubElement(image_el, "box", top=str(top), left=str(left),
                               width=str(width), height=str(height))
        for i, (x, y) in enumerate(coords_img):
            # noms 0-indexes, largeur fixe pour un tri lexicographique stable
            ET.SubElement(box_el, "part", name=f"{i:02d}",
                          x=str(int(round(x))), y=str(int(round(y))))
        n_ok += 1

    rough = ET.tostring(dataset, encoding="ISO-8859-1")
    pretty = minidom.parseString(rough).toprettyxml(indent="  ",
                                                     encoding="ISO-8859-1")
    with open(out_path, "wb") as fh:
        fh.write(pretty)
    print(f"  -> {os.path.basename(out_path)} : {n_ok} images "
          f"({n_skip} ignorées)")
    return n_ok


def main():
    ap = argparse.ArgumentParser(description="TPS -> XML dlib pour ml-morph")
    ap.add_argument("--dataset-dir", default="mlmorph_dataset",
                    help="dossier contenant train/val/test.tps + split_assignments.csv")
    ap.add_argument("--photo-dir", default="../T_26_LaSaudrune/Photos")
    ap.add_argument("--out-dir", default="mlmorph_run")
    ap.add_argument("--box-pad", type=float, default=0.15,
                    help="marge relative de la box (0.15 = 15%%)")
    ap.add_argument("--fixed-box", action="store_true",
                    help="box de taille fixe (ratio identique) — requis pour le "
                         "détecteur HOG, recommandé sur jeu canonicalisé")
    args = ap.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)
    split_csv = os.path.join(args.dataset_dir, "split_assignments.csv")
    heights = load_heights(split_csv)
    print(f"Hauteurs chargées : {len(heights)} spécimens")

    # Chargement de tous les blocs (pour dims fixes + QC)
    blocks = {}
    for block in ("train", "val", "test"):
        tps = os.path.join(args.dataset_dir, f"{block}.tps")
        if os.path.exists(tps):
            blocks[block] = parse_tps(tps)
        else:
            print(f"  [!] {tps} absent — ignoré")

    fixed_dims = None
    if args.fixed_box:
        allspec = [s for spec in blocks.values() for s in spec]
        fixed_dims, med_h, outliers = compute_fixed_dims(allspec, heights, args.box_pad)
        print(f"Box fixe : {fixed_dims[0]:.0f} x {fixed_dims[1]:.0f} px "
              f"(ratio {fixed_dims[0]/fixed_dims[1]:.2f}, identique pour tous)")
        if outliers:
            print(f"  [QC] {len(outliers)} spécimen(s) à étendue de landmarks "
                  f"anormale (landmark probablement mal placé/imputé) : "
                  f"{', '.join(sorted(outliers))}")

    total = 0
    for block, specimens in blocks.items():
        out = os.path.join(args.out_dir, f"{block}.xml")
        total += write_xml(specimens, heights, args.photo_dir, out,
                           args.box_pad, fixed_dims=fixed_dims)
    print(f"Terminé : {total} images écrites dans {os.path.abspath(args.out_dir)}")


if __name__ == "__main__":
    main()
