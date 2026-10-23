#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
predict_new_image.py

Landmarking automatique d'UNE nouvelle photo à partir de 4 clics utilisateur :
  - museau (LM1) et base de caudale (LM2)  -> orientation + longueur de corps
  - 2 points d'échelle sur la règle        -> calibration px->mm (optionnel)

Pipeline : canonicalise l'image (rotation tête-à-gauche + mise à l'échelle à une
longueur de corps commune + recadrage) exactement comme à l'entraînement, applique
le prédicteur de forme ml-morph (dlib) pour placer les 19 landmarks anatomiques,
puis inverse la transformation pour ramener les points en coordonnées image
d'origine. Les points d'échelle 20-21 sont repris des clics utilisateur.

Un affinage optionnel (--refine) recadre une 2e fois centré sur le barycentre
des 19 points prédits (comme à l'entraînement) pour améliorer le cadrage.

Sortie : CSV (landmark, X, Y) en coordonnées image d'origine (haut-gauche).

Usage :
  python3 predict_new_image.py --image photo.jpeg \
      --snout 2450,1980 --caudal 3480,1960 \
      --scale1 3719,2079 --scale2 3869,2075 --scale-mm 10 \
      --dataset-dir mlmorph_dataset_aligned \
      --predictor mlmorph_run_aligned/predictor.dat \
      --out out.csv
"""
import argparse
import csv
import math
import os
import sys

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
try:
    import cv2
    import build_mlmorph_xml as bx     # compute_fixed_dims, load_heights, parse_tps
except ImportError as e:
    sys.exit(f"Dépendance manquante ({e}). Activez le venv .venv_mlmorph.")
try:
    import dlib                        # modèles .dat (régression par arbres)
except ImportError:
    dlib = None


class CNNModel:
    """Modèle à cartes de chaleur (train_cnn.py) exporté en ONNX : model.onnx +
    cnn_params.json dans le même dossier. Prend l'image CANONIQUE (même repère
    que les modèles dlib) et rend 19 points (pixels canoniques) + confiance."""

    def __init__(self, path):
        import json
        try:
            import onnxruntime as ort
        except ImportError:
            sys.exit("Dépendance manquante (onnxruntime) pour un modèle .onnx : "
                     "~/.venv_mlmorph/bin/pip install onnxruntime")
        self.p = json.load(open(os.path.join(os.path.dirname(path), "cnn_params.json")))
        self.sess = ort.InferenceSession(path, providers=["CPUExecutionProvider"])

    def __call__(self, img_canon):
        p = self.p
        x = cv2.resize(img_canon, (p["in_w"], p["in_h"]), interpolation=cv2.INTER_AREA)
        x = cv2.cvtColor(x, cv2.COLOR_BGR2RGB).astype(np.float32) / 255.0
        x = (x - np.array(p["mean"], np.float32)) / np.array(p["std"], np.float32)
        xy, conf = self.sess.run(None, {"image": x.transpose(2, 0, 1)[None].astype(np.float32)})
        return xy[0] / p["canon_scale"], conf[0]


class GlobalModel:
    """Réseau GLOBAL (train_global.py) : localise les landmarks sur la photographie
    ENTIÈRE, sans clic. model.onnx + global_params.json dans le même dossier.
    Sert à placer LM1 (museau), LM2 (caudale) et LM3 (dorsal) -- les points qui
    fixent le repère du modèle fin -- en mode --auto."""

    def __init__(self, path):
        import json
        try:
            import onnxruntime as ort
        except ImportError:
            sys.exit("Dépendance manquante (onnxruntime) : ~/.venv_mlmorph/bin/pip install onnxruntime")
        self.p = json.load(open(os.path.join(os.path.dirname(path), "global_params.json")))
        self.sess = ort.InferenceSession(path, providers=["CPUExecutionProvider"])

    def run(self, img):
        """img BGR quelconque -> (19 x 2 points en pixels de img, confiances).
        Garde les cartes de probabilité de LM1-LM3 (self.heat, modèles exportés
        avec la sortie `heat`) et le facteur d'échelle (self.f) pour candidates()."""
        S = int(self.p["size"]); h, w = img.shape[:2]; f = S / max(h, w)
        nh, nw = max(1, round(h * f)), max(1, round(w * f))
        box = np.zeros((S, S, 3), np.uint8)
        box[:nh, :nw] = cv2.resize(img, (nw, nh), interpolation=cv2.INTER_AREA if f < 1 else cv2.INTER_LINEAR)
        x = cv2.cvtColor(box, cv2.COLOR_BGR2RGB).astype(np.float32) / 255.0
        x = (x - np.array(self.p["mean"], np.float32)) / np.array(self.p["std"], np.float32)
        out = self.sess.run(None, {"image": x.transpose(2, 0, 1)[None].astype(np.float32)})
        self.f = f
        self.heat = out[2][0] if len(out) > 2 else None
        return out[0][0] / f, out[1][0]

    @property
    def has_heat(self):
        return any(o.name == "heat" for o in self.sess.get_outputs())

    def candidates(self, j, k=4, rel=0.08, r=2):
        """Pics locaux de la carte du point j (0 = LM1, 1 = LM2, 2 = LM3) de la
        dernière passe run() -> liste de (xy en pixels de l'image, masse)."""
        if self.heat is None:
            return []
        p = self.heat[j].astype(np.float32); H, W = p.shape
        stride = float(self.p.get("stride", 4))
        m = cv2.dilate(p, np.ones((5, 5), np.uint8))
        ys, xs = np.where((p >= m) & (p >= rel * p.max()))
        out = []
        for i in np.argsort(-p[ys, xs])[:k]:
            y, x = ys[i], xs[i]
            y0, y1, x0, x1 = max(0, y - r), min(H, y + r + 1), max(0, x - r), min(W, x + r + 1)
            w = p[y0:y1, x0:x1]; mass = float(w.sum())
            cy = float((w.sum(1) * np.arange(y0, y1)).sum() / mass)
            cx = float((w.sum(0) * np.arange(x0, x1)).sum() / mass)
            out.append((np.array([(cx + 0.5) * stride - 0.5, (cy + 0.5) * stride - 0.5]) / self.f, mass))
        return out

    def locate(self, img, zoom=True, pad=0.25):
        """Passe 1 sur la photo entière ; passe 2 (zoom) sur un recadrage autour du
        poisson détecté (boîte des 19 points + pad x longueur), à meilleure résolution."""
        P, c = self.run(img)
        if zoom:
            H, W = img.shape[:2]
            bl = float(np.hypot(*(P[1] - P[0])))
            x0, y0 = np.floor(P.min(0) - pad * bl).astype(int)
            x1, y1 = np.ceil(P.max(0) + pad * bl).astype(int)
            x0, y0, x1, y1 = max(0, x0), max(0, y0), min(W, x1), min(H, y1)
            if x1 - x0 > 16 and y1 - y0 > 16:
                P2, c2 = self.run(img[y0:y1, x0:x1])
                P, c = P2 + [x0, y0], c2
        return P, c

    def locate_verified(self, img, fine, max_fish=1, min_rel_score=0.85, **kw):
        """Plusieurs poissons (planches) : détecte le meilleur, le MASQUE (couleur
        médiane du fond sur l'enveloppe de ses 19 points élargie), et recommence
        sur l'image masquée -- le réseau global, entraîné sur un poisson par photo,
        concentre sa probabilité sur un seul poisson à la fois. Arrêt à max_fish
        poissons, ou quand le score tombe sous min_rel_score x celui du premier."""
        found, search, hulls = [], img, []
        fill = tuple(int(v) for v in np.median(img.reshape(-1, 3), 0))
        for _ in range(max_fish):
            h = self._locate_one(img, fine, search=search, **kw)
            if not h:
                break
            h = h[0]
            if found and h["score"] < min_rel_score * found[0]["score"]:
                break
            if hulls:
                # un « nouveau » poisson qui chevauche un poisson déjà trouvé (même
                # museau, nageoire caudale débordant du masque) n'en est pas un
                Pf = h["P"][np.isfinite(h["P"]).all(1)]
                inside = max(np.mean([cv2.pointPolygonTest(hl, (float(x), float(y)), False) >= 0 for x, y in Pf])
                             for hl in hulls)
                key_in = any(cv2.pointPolygonTest(hl, (float(h[k_][0]), float(h[k_][1])), False) >= 0
                             for hl in hulls for k_ in ("snout", "caudal"))
                if inside > 0.25 or key_in:
                    break
            found.append(h)
            if len(found) >= max_fish:
                break
            bl = float(np.hypot(*(h["caudal"] - h["snout"])))
            P = h["P"][np.isfinite(h["P"]).all(1)].astype(np.float32)
            hull = cv2.convexHull(P).reshape(-1, 2)
            c = hull.mean(0)
            hull = c + (hull - c) * 1.0 + np.sign(hull - c) * 0.12 * bl
            search = search.copy()
            cv2.fillPoly(search, [np.round(hull).astype(np.int32)], fill)
            hulls.append(np.round(hull).astype(np.float32).reshape(-1, 1, 2))
        return found

    def _locate_one(self, img, fine, search=None, k=4, n_hyp=8, pad=0.25):
        max_fish = 1
        """Localisation ROBUSTE : au lieu de la moyenne soft-argmax (qui tombe entre
        deux poissons, ou sur une étiquette), énumère les pics de LM1 et de LM2,
        forme les couples museau-caudale plausibles, fait passer chacun par le
        modèle fin `fine(img, snout, caudal, orient) -> (P, conf)` et garde celui
        que le modèle fin reconnaît le mieux (confiance moyenne des points 3-19) :
        un couple formé sur deux poissons, ou sur un objet, ne ressemble à aucun
        poisson dans le repère canonique. Puis passe zoomée sur le poisson retenu.
        max_fish > 1 : plusieurs poissons disjoints (planches), par score décroissant.
        Rend une liste de dict(snout, caudal, orient, P, conf, score), triée par score."""
        H, W = img.shape[:2]; side = max(H, W)
        P0, c0 = self.run(img if search is None else search)
        C1, C2, C3 = (self.candidates(j, k) for j in range(3))
        if not C1 or not C2:
            C1, C2, C3 = [(P0[0], 1.0)], [(P0[1], 1.0)], [(P0[2], 1.0)]
        hyps = []
        for s, ms in C1:
            for t, mt in C2:
                bl = float(np.hypot(*(t - s)))
                if bl < 0.06 * side:
                    continue
                mid = (s + t) / 2
                near = [(q, mq) for q, mq in C3 if np.hypot(*(q - mid)) < 0.6 * bl]
                if near:
                    orients = [max(near, key=lambda z: z[1])[0]]
                else:          # pas de dos plausible : les deux orientations
                    nrm = np.array([-(t - s)[1], (t - s)[0]]) / bl
                    orients = [mid + 0.3 * bl * nrm, mid - 0.3 * bl * nrm]
                for o in orients:
                    hyps.append((ms * mt, s, t, o))
        hyps = sorted(hyps, key=lambda h: -h[0])[:n_hyp]
        scored = []
        for prior, s, t, o in hyps:
            P, cf = fine(img, s, t, o)
            scored.append(dict(snout=s, caudal=t, orient=o, P=P, conf=cf,
                               score=float(np.mean(cf[2:19]))))
        scored.sort(key=lambda d: -d["score"])
        chosen = []
        for h in scored:
            bl = float(np.hypot(*(h["caudal"] - h["snout"])))
            mid = (h["snout"] + h["caudal"]) / 2
            clash = any(min(np.hypot(*(h[k1] - g[k2])) for k1 in ("snout", "caudal") for k2 in ("snout", "caudal"))
                        < 0.15 * bl or np.hypot(*(mid - (g["snout"] + g["caudal"]) / 2)) < 0.3 * bl
                        for g in chosen)
            if not clash:
                chosen.append(h)
            if len(chosen) >= max_fish:
                break
        # passe zoomée : recadrage sur le poisson tel que le modèle fin l'a vu
        for h in chosen:
            P, bl = h["P"], float(np.hypot(*(h["caudal"] - h["snout"])))
            x0, y0 = np.floor(np.nanmin(P, 0) - pad * bl).astype(int)
            x1, y1 = np.ceil(np.nanmax(P, 0) + pad * bl).astype(int)
            x0, y0, x1, y1 = max(0, x0), max(0, y0), min(W, x1), min(H, y1)
            if x1 - x0 <= 16 or y1 - y0 <= 16:
                continue
            Pz, cz = self.run((img if search is None else search)[y0:y1, x0:x1])
            new = {}
            for j, key in ((0, "snout"), (1, "caudal")):
                cands = [(q + [x0, y0], m) for q, m in self.candidates(j, k)] or [(Pz[j] + [x0, y0], 1.0)]
                q = min(cands, key=lambda z: np.hypot(*(z[0] - h[key])))[0]
                if np.hypot(*(q - h[key])) < 0.15 * bl:
                    new[key] = q
            if new:
                s, t = new.get("snout", h["snout"]), new.get("caudal", h["caudal"])
                Pn, cn = fine(img, s, t, h["orient"])
                h.update(snout=s, caudal=t, P=Pn, conf=cn, score=float(np.mean(cn[2:19])))
        return chosen


class RefineModel:
    """2e étage à haute résolution (train_refine.py) : réaffine les points d'une
    RÉGION (tête, pectorale, caudale) sur un recadrage du repère canonique pris
    dans la photo d'origine. model.onnx + refine_params.json."""

    def __init__(self, path):
        import json
        import onnxruntime as ort
        self.p = json.load(open(os.path.join(os.path.dirname(path), "refine_params.json")))
        self.sess = ort.InferenceSession(path, providers=["CPUExecutionProvider"])
        self.ids = list(self.p["landmark_ids"])
        # points réécrits : tous ceux de la région, sauf 8 et 9 (points de la ligne
        # ventrale, dérivés par les conventions) pour la tête. Validation des traits
        # (bloc test) : réaffiner 8/9 décorrèle leur erreur de celle de 3/4 (Bd), que
        # le 1er étage partage, et dégrade VEp (Eh/Bd) ; les garder du 1er étage donne
        # le meilleur résultat sur les 9 rapports.
        self.apply_ids = [l for l in self.ids if l not in self.p.get("keep_first_stage", [8, 9])]

    def __call__(self, img, M, Pc, ids, n_iter=2):
        """img : photo d'origine ; M : affine origine -> canonique ; Pc : points
        canoniques (lignes = ids). Rend Pc avec les points de la région réaffinés."""
        S = float(self.p["crop_frac"]) * float(self.p["target_len"]); size = int(self.p["size"]); k = size / S
        rows = [ids.index(l) for l in self.ids if l in ids]
        if not rows:
            return Pc, None
        Pc = Pc.copy(); conf = None
        M3 = np.vstack([M, [0, 0, 1]])
        for _ in range(n_iter):                       # 2e passe : recentrée sur les points réaffinés
            sub = Pc[rows]; c = np.nanmean(sub[np.isfinite(sub).all(1)], 0)
            A = np.array([[k, 0, size / 2 - k * c[0]], [0, k, size / 2 - k * c[1]], [0, 0, 1]])
            F = (A @ M3)[:2]
            zoom = abs(np.linalg.det(F[:, :2])) ** 0.5
            x = cv2.warpAffine(img, F, (size, size), flags=cv2.INTER_AREA if zoom < 1 else cv2.INTER_LINEAR,
                               borderValue=(0, 0, 0))
            x = cv2.cvtColor(x, cv2.COLOR_BGR2RGB).astype(np.float32) / 255.0
            x = (x - np.array(self.p["mean"], np.float32)) / np.array(self.p["std"], np.float32)
            xy, cf = self.sess.run(None, {"image": x.transpose(2, 0, 1)[None].astype(np.float32)})
            q = (xy[0] - [size / 2 - k * c[0], size / 2 - k * c[1]]) / k
            for j, l in enumerate(self.ids):
                if l in ids and l in self.apply_ids:
                    Pc[ids.index(l)] = q[j]
            conf = {l: v for l, v in zip(self.ids, cf[0]) if l in self.apply_ids}
        return Pc, conf


def find_refiners(predictor):
    """mlmorph_run_refine_<région>/model.onnx à côté du dossier du modèle fin."""
    root = os.path.dirname(os.path.dirname(os.path.abspath(predictor)))
    paths = [os.path.join(root, f"mlmorph_run_refine_{r}", "model.onnx") for r in ("head", "pectoral", "caudal")]
    return [p for p in paths if os.path.exists(p)]


def apply_refiners(img, M, Pc, ids, refiners, conf=None):
    """Applique les modèles de région (2e étage) aux points canoniques Pc."""
    conf = None if conf is None else np.array(conf, float).copy()
    for R in refiners or []:
        Pc, cr = R(img, M, Pc, ids)
        if cr and conf is not None:
            for l, v in cr.items():
                if l in ids:
                    conf[ids.index(l)] = v
    return Pc, conf


def fine_predict(img, snout, caudal, orient, cnn, TL, cw, ch, refiners=None, keep12=True):
    """Modèle fin (.onnx) dans le repère canonique défini par museau/caudale/dos
    -> 19 points en pixels de img + confiances (LM1/LM2 = museau/caudale).
    refiners : modèles de région (2e étage) ; keep12=False laisse le 2e étage
    réaffiner aussi LM1/LM2 (détections automatiques, pas des clics)."""
    dx, dy = caudal - snout
    angle = math.degrees(math.atan2(dy, dx)); scale = TL / max(math.hypot(dx, dy), 1e-6)
    M = orient_dorsal_up(affine((snout + caudal) / 2, angle, scale, cw, ch), orient, ch)
    P, c = cnn(cv2.warpAffine(img, M, (cw, ch), flags=cv2.INTER_LINEAR if scale < 1 else cv2.INTER_CUBIC))
    P = np.asarray(P, float).copy()
    Mi = cv2.invertAffineTransform(M)
    if refiners:
        ids = landmark_ids(cnn)
        P[0], P[1] = (M[:, :2] @ snout + M[:, 2]), (M[:, :2] @ caudal + M[:, 2])
        P, c = apply_refiners(img, M, P, ids, refiners, c)
    P = (Mi[:, :2] @ P.T).T + Mi[:, 2]
    if keep12 or not refiners:
        P[0], P[1] = snout, caudal
    return P, np.asarray(c, float)


def landmark_ids(cnn):
    """Identifiants des sorties du modèle fin : 1-19, plus 22 et 24 (charnières)
    pour un modèle entraîné avec --hinges."""
    p = getattr(cnn, "p", {}) or {}
    return list(p.get("landmark_ids", range(1, int(p.get("n_landmarks", 19)) + 1)))


def straighten(img, poly, dorsal, TL, cw, ch):
    """REDRESSE un poisson courbé : l'axe brisé poly = [LM1, LM22, LM24, LM2]
    (pixels de img) est déroulé en une droite horizontale de longueur TL (repère
    canonique cw x ch, museau à gauche, dos en haut). Rend l'image redressée et
    une fonction to_orig(points canoniques) -> pixels de img. Aux charnières, la
    normale est interpolée sur une fenêtre de ±6 % de la longueur pour éviter les
    plis. Longueur de référence = longueur de l'axe brisé (celle que
    fishmorph_segments() mesure), pas la corde 1-2."""
    poly = np.asarray(poly, float)
    seg = np.diff(poly, axis=0); ln = np.hypot(seg[:, 0], seg[:, 1])
    L = float(ln.sum()); cum = np.concatenate([[0], np.cumsum(ln)])
    tan = seg / ln[:, None]
    k = TL / L                                   # px canoniques par px d'origine
    x0 = cw / 2.0 - TL / 2.0

    def frame(u):
        """Point de l'axe et normale (unitaire) à l'abscisse curviligne u."""
        u = np.asarray(u, float)
        i = np.clip(np.searchsorted(cum, u, side="right") - 1, 0, len(ln) - 1)
        p = poly[i] + (u - cum[i])[..., None] * tan[i]
        t = tan[i].copy()
        w = 0.06 * L
        for j in range(1, len(ln)):              # lissage aux charnières
            a = np.clip((u - (cum[j] - w)) / (2 * w), 0, 1)
            near = (u > cum[j] - w) & (u < cum[j] + w)
            t[near] = ((1 - a[near])[..., None] * tan[j - 1] + a[near][..., None] * tan[j])
        t /= np.hypot(t[..., 0], t[..., 1])[..., None]
        return p, np.stack([-t[..., 1], t[..., 0]], -1)

    sgn = 1.0
    if dorsal is not None and np.isfinite(dorsal).all():
        d = np.asarray(dorsal, float)
        j = int(np.argmin([np.hypot(*(d - (poly[i] + tan[i] * np.clip(np.dot(d - poly[i], tan[i]), 0, ln[i]))))
                           for i in range(len(ln))]))
        nrm = np.array([-tan[j][1], tan[j][0]])
        if np.dot(d - poly[j], nrm) > 0:         # dos du côté +n -> y canonique < ch/2
            sgn = -1.0

    def to_orig(Pc):
        Pc = np.asarray(Pc, float)
        u = (Pc[:, 0] - x0) / k; v = sgn * (Pc[:, 1] - ch / 2.0) / k
        p, n = frame(u)
        return p + v[:, None] * n

    u = (np.arange(cw) - x0) / k
    p, n = frame(u)                               # par colonne
    v = sgn * (np.arange(ch) - ch / 2.0) / k      # par ligne
    mx = (p[None, :, 0] + v[:, None] * n[None, :, 0]).astype(np.float32)
    my = (p[None, :, 1] + v[:, None] * n[None, :, 1]).astype(np.float32)
    img_s = cv2.remap(img, mx, my, interpolation=cv2.INTER_LINEAR, borderMode=cv2.BORDER_CONSTANT)
    return img_s, to_orig


def refine_curved(img, P, ids, orient, cnn, TL, cw, ch, thr=1.5, n_iter=2):
    """Si le modèle fin place les charnières 22/24 nettement hors de l'axe 1-2
    (écart > thr % de la longueur), le poisson est courbé : on le redresse le
    long de 1-22-24-2 et on refait la prédiction, deux fois au plus.
    Rend (P, conf, courbure en % de Bl ou 0, redressé ?)."""
    if 22 not in ids or 24 not in ids:
        return P, None, 0.0, False
    i22, i24 = ids.index(22), ids.index(24)
    conf = None; curved = False
    snout, caudal = P[0].copy(), P[1].copy()
    for _ in range(n_iter):
        ax = caudal - snout; bl = float(np.hypot(*ax)); nrm = np.array([-ax[1], ax[0]]) / max(bl, 1e-6)
        dev = max(abs(np.dot(P[i22] - snout, nrm)), abs(np.dot(P[i24] - snout, nrm))) / max(bl, 1e-6) * 100
        if not curved and dev < thr:
            return P, conf, dev, False
        poly = [snout, P[i22], P[i24], caudal]
        img_s, to_orig = straighten(img, poly, orient, TL, cw, ch)
        Pc, conf = cnn(img_s)
        P = to_orig(Pc); P[0], P[1] = snout, caudal
        curved = True
    return P, np.asarray(conf, float), dev, True


def find_global_model(predictor):
    """mlmorph_run_global/model.onnx à côté du dossier du modèle fin."""
    root = os.path.dirname(os.path.dirname(os.path.abspath(predictor)))
    p = os.path.join(root, "mlmorph_run_global", "model.onnx")
    return p if os.path.exists(p) else ""


def parse_xy(s):
    a, b = s.split(",")
    return np.array([float(a), float(b)])


def affine(center, angle, scale, cw, ch):
    """Matrice affine original->canonicalisé, centrée sur `center`."""
    M = cv2.getRotationMatrix2D((float(center[0]), float(center[1])), angle, scale)
    M[0, 2] += cw / 2.0 - center[0]
    M[1, 2] += ch / 2.0 - center[1]
    return M


def orient_dorsal_up(M, dorsal, ch):
    """Si le point dorsal se retrouve SOUS l'axe (poisson ventre en l'air), ajoute
    un retournement vertical à M pour garantir dos-en-haut dans le repère canonique."""
    if dorsal is None:
        return M
    dy = M[1, 0] * dorsal[0] + M[1, 1] * dorsal[1] + M[1, 2]
    if dy > ch / 2.0:                      # dorsal en bas -> flip vertical (y -> ch - y)
        M = M.copy()
        M[1, :] = [-M[1, 0], -M[1, 1], ch - M[1, 2]]
    return M


def fixed_box_dims(dataset_dir, pad=0.15):
    """Reproduit la box fixe d'entraînement (compute_fixed_dims sur le jeu aligné)."""
    specs = []
    for b in ("train", "val", "test"):
        p = os.path.join(dataset_dir, f"{b}.tps")
        if os.path.exists(p):
            specs += bx.parse_tps(p)
    heights = bx.load_heights(os.path.join(dataset_dir, "split_assignments.csv"))
    (W, H), _, _ = bx.compute_fixed_dims(specs, heights, pad)
    return W, H


def read_canon_params(dataset_dir):
    """Paramètres du repère canonique écrits à l'entraînement
    (canon_params.txt, produit par build_campaign_dataset.py). Absent pour les
    anciens jeux -> {} et comportement historique inchangé."""
    p = os.path.join(dataset_dir, "canon_params.txt")
    out = {}
    if os.path.exists(p):
        for ln in open(p):
            if "=" in ln:
                k, v = ln.strip().split("=", 1)
                out[k.strip()] = v.strip()
    return out


def predict_config(predictor, img_canon, box_w, box_h, cw, ch):
    """Prédit les N landmarks dans l'image canonicalisée (box fixe centrée)."""
    left = int(round(cw / 2 - box_w / 2)); top = int(round(ch / 2 - box_h / 2))
    rect = dlib.rectangle(left, top, left + int(box_w), top + int(box_h))
    rgb = cv2.cvtColor(img_canon, cv2.COLOR_BGR2RGB)
    shape = predictor(rgb, rect)
    return np.array([[shape.part(i).x, shape.part(i).y]
                     for i in range(shape.num_parts)], dtype=float)


def main():
    ap = argparse.ArgumentParser(description="Landmarking auto d'une nouvelle photo")
    ap.add_argument("--image", required=True)
    ap.add_argument("--snout", default="", help="x,y du museau (LM1)")
    ap.add_argument("--caudal", default="", help="x,y de la base de caudale (LM2)")
    ap.add_argument("--auto", action="store_true",
                    help="aucun clic : LM1, LM2 et LM3 sont localisés par le réseau global "
                         "(mlmorph_run_global/model.onnx), puis le modèle fin place les autres")
    ap.add_argument("--global-model", default="", help="modèle global (.onnx) ; défaut : "
                    "mlmorph_run_global/model.onnx à côté du dossier de --predictor")
    ap.add_argument("--no-zoom", dest="zoom", action="store_false", default=True,
                    help="mode --auto : pas de seconde passe recadrée du réseau global")
    ap.add_argument("--dorsal", default="", help="x,y d'un point DORSAL (haut du corps) "
                    "-> oriente dos-en-haut automatiquement ; SERT AUSSI de LM3 épinglé "
                    "quand --pin-clicks est actif")
    ap.add_argument("--lm4", default="", help="x,y de LM4 (ventral, à l'aplomb de LM3) "
                    "-> épinglé sur le clic quand --pin-clicks est actif")
    ap.add_argument("--lm7", default="", help="x,y de LM7 (référence tête : verticale œil) "
                    "-> épinglé sur le clic quand --pin-clicks est actif")
    for k in ("lm10", "lm12", "lm15", "lm16", "lm18"):
        ap.add_argument(f"--{k}", default="",
                        help=f"x,y de {k.upper()} -> épinglé sur le clic quand --pin-clicks")
    ap.add_argument("--scale1", default="", help="x,y point d'échelle 20")
    ap.add_argument("--scale2", default="", help="x,y point d'échelle 21")
    ap.add_argument("--scale-mm", type=float, default=0)
    ap.add_argument("--dataset-dir", default="mlmorph_dataset_aligned")
    ap.add_argument("--predictor", default="mlmorph_run_aligned/predictor.dat")
    # None = valeur lue dans <dataset-dir>/canon_params.txt, sinon défaut historique
    ap.add_argument("--target-len", type=float, default=None)
    ap.add_argument("--crop-w-factor", type=float, default=None)
    ap.add_argument("--crop-h-factor", type=float, default=None)
    ap.add_argument("--refine", action="store_true", default=None)
    ap.add_argument("--no-refine", dest="refine", action="store_false")
    ap.add_argument("--pin-clicks", action="store_true", default=True,
                    help="épingle LM1=museau et LM2=caudale sur les clics utilisateur "
                         "(placés précisément) ; le modèle ne prédit que les autres")
    ap.add_argument("--no-pin-clicks", dest="pin_clicks", action="store_false")
    ap.add_argument("--no-refine-regions", dest="refine_regions", action="store_false", default=True,
                    help="sans le 2e étage à haute résolution (mlmorph_run_refine_*)")
    ap.add_argument("--hinges", action="store_true", default=False,
                    help="écrit aussi les charnières 22/24 prédites (modèle à charnières). "
                         "Désactivé par défaut : hors Saudrune (seul site étiqueté), les "
                         "charnières prédites inclinent les repères de segment et dégradent "
                         "les points dérivés par les conventions (VEp, OGp)")
    ap.add_argument("--straighten", action="store_true", default=False,
                    help="EXPÉRIMENTAL : redresse le poisson le long de 1-22-24-2 et refait la "
                         "prédiction quand les charnières s'écartent de l'axe (modèle avec "
                         "charnières). Désactivé par défaut : sur le bloc test il dégrade la "
                         "précision dans l'ensemble (le modèle fin suit déjà la courbure)")
    ap.add_argument("--out", default="prediction.csv")
    args = ap.parse_args()

    # Repère canonique : il doit être IDENTIQUE à celui de l'entraînement.
    # center=midpoint (modèles campagne) : la fenêtre est centrée sur le milieu
    # LM1-LM2, exactement comme à l'entraînement -> pas de passe "refine", qui
    # recentrerait sur le barycentre prédit et décalerait toute la configuration.
    cp = read_canon_params(args.dataset_dir)
    if args.target_len is None:
        args.target_len = float(cp.get("target_len", 750))
    if args.crop_w_factor is None:
        args.crop_w_factor = float(cp.get("crop_w_factor", 2.6))
    if args.crop_h_factor is None:
        args.crop_h_factor = float(cp.get("crop_h_factor", 1.7))
    if args.refine is None:
        args.refine = cp.get("refine", "1") not in ("0", "false", "False")

    img = cv2.imread(args.image, cv2.IMREAD_COLOR | cv2.IMREAD_IGNORE_ORIENTATION)
    if img is None:
        sys.exit(f"Image illisible : {args.image}")
    auto_conf = None
    if args.auto:
        gpath = args.global_model or find_global_model(args.predictor)
        if not gpath or not os.path.exists(gpath):
            sys.exit("Mode --auto : modèle global introuvable (mlmorph_run_global/model.onnx)")
        GM = GlobalModel(gpath)
        if GM.has_heat and args.predictor.lower().endswith(".onnx"):
            # localisation vérifiée par le modèle fin (robuste aux étiquettes, aux
            # reflets et aux photos à plusieurs poissons) ; avec un museau ou une
            # caudale déjà cliqués, on garde le poisson le plus proche du clic
            cp0 = read_canon_params(args.dataset_dir)
            TL0 = float(cp0.get("target_len", 750))
            cw0 = int(round(float(cp0.get("crop_w_factor", 2.6)) * TL0))
            ch0 = int(round(float(cp0.get("crop_h_factor", 1.7)) * TL0))
            fine_cnn = CNNModel(args.predictor)
            fine = lambda im, s_, t_, o_: fine_predict(im, s_, t_, o_, fine_cnn, TL0, cw0, ch0)
            clicked = [(0, "snout", args.snout), (1, "caudal", args.caudal)]
            clicked = [(j, k_, parse_xy(v)) for j, k_, v in clicked if v]
            V = GM.locate_verified(img, fine, max_fish=4 if clicked else 1)
            if not V:
                sys.exit("Mode --auto : aucun poisson trouvé sur la photo")
            if clicked:
                V.sort(key=lambda h: min(np.hypot(*(h[k_] - p)) for _, k_, p in clicked))
            h = V[0]
            G = np.vstack([h["snout"], h["caudal"], h["orient"]])
            auto_conf = h["conf"]
        else:
            G, auto_conf = GM.locate(img, zoom=args.zoom)
        # un clic fourni prime sur la détection automatique
        user12 = (bool(args.snout), bool(args.caudal))
        args.snout = args.snout or f"{G[0, 0]},{G[0, 1]}"
        args.caudal = args.caudal or f"{G[1, 0]},{G[1, 1]}"
        auto_dorsal = G[2]
    elif not (args.snout and args.caudal):
        sys.exit("--snout et --caudal sont requis (ou --auto)")
    snout = parse_xy(args.snout); caudal = parse_xy(args.caudal)
    dorsal = parse_xy(args.dorsal) if args.dorsal else None
    if args.auto and dorsal is None:
        orient = auto_dorsal            # oriente dos-en-haut, mais LM3 reste prédit par le modèle fin
    else:
        orient = dorsal
    lm4 = parse_xy(args.lm4) if args.lm4 else None
    lm7 = parse_xy(args.lm7) if args.lm7 else None
    # LM10, LM12, LM15, LM16, LM18 : indices 0-based 9, 11, 14, 15, 17
    extra_pins = {9: args.lm10, 11: args.lm12, 14: args.lm15, 15: args.lm16, 17: args.lm18}
    extra_pins = {i: parse_xy(s) for i, s in extra_pins.items() if s}
    # (image lue plus haut avec IMREAD_IGNORE_ORIENTATION : les applications R
    #  (jpeg::readJPEG, png::readPNG) lisent la grille brute sans appliquer la
    #  balise EXIF d'orientation ; les clics vivent dans cette grille.)

    dx, dy = caudal - snout
    angle = math.degrees(math.atan2(dy, dx))          # signe validé
    body = math.hypot(dx, dy)
    scale = args.target_len / body if body > 0 else 1.0
    cw = int(round(args.crop_w_factor * args.target_len))
    ch = int(round(args.crop_h_factor * args.target_len))
    # box of the training set: frozen in canon_params.txt when the dataset
    # declares it (build_combined_dataset.py), otherwise rebuilt from the TPS
    if "box_w" in cp and "box_h" in cp:
        box_w, box_h = float(cp["box_w"]), float(cp["box_h"])
    else:
        box_w, box_h = fixed_box_dims(args.dataset_dir)
    conf = None
    if args.predictor.lower().endswith(".onnx"):
        cnn = CNNModel(args.predictor)
        if (cnn.p.get("canon_w"), cnn.p.get("canon_h")) != (cw, ch):
            sys.exit(f"Repère canonique incohérent : modèle {cnn.p.get('canon_w')}x{cnn.p.get('canon_h')}, "
                     f"jeu {cw}x{ch} (canon_params.txt de --dataset-dir)")
        # même rééchantillonnage qu'à la construction du jeu d'entraînement
        interp = cv2.INTER_LINEAR if scale < 1 else cv2.INTER_CUBIC
        def run_model(img_c):
            P, c = cnn(img_c)
            run_model.conf = c
            return P
    else:
        if dlib is None:
            sys.exit("Dépendance manquante (dlib) pour un modèle .dat. Activez le venv .venv_mlmorph.")
        predictor = dlib.shape_predictor(args.predictor)
        interp = cv2.INTER_CUBIC
        def run_model(img_c):
            return predict_config(predictor, img_c, box_w, box_h, cw, ch)
    run_model.conf = None

    # Passe 1 : centre = milieu museau-caudale (seule info disponible a priori)
    center = (snout + caudal) / 2.0
    M = orient_dorsal_up(affine(center, angle, scale, cw, ch), orient, ch)
    img_c = cv2.warpAffine(img, M, (cw, ch), flags=interp)
    P_canon = run_model(img_c)

    # Passe 2 (refine) : recentrer sur le barycentre des 19 points prédits
    if args.refine:
        Minv = cv2.invertAffineTransform(M)
        centroid_canon = P_canon.mean(axis=0)
        center = Minv[:, :2] @ centroid_canon + Minv[:, 2]   # -> coords d'origine
        M = orient_dorsal_up(affine(center, angle, scale, cw, ch), orient, ch)
        img_c = cv2.warpAffine(img, M, (cw, ch), flags=interp)
        P_canon = run_model(img_c)

    # 2e étage à haute résolution (mlmorph_run_refine_* à côté du modèle fin) :
    # tête, pectorale, caudale réaffinées sur la photo d'origine
    refiners = []
    if args.refine_regions and args.predictor.lower().endswith(".onnx"):
        refiners = [RefineModel(p) for p in find_refiners(args.predictor)]
    if refiners:
        ids0 = landmark_ids(cnn)
        P_canon = np.asarray(P_canon, float).copy()
        P_canon[0] = M[:, :2] @ snout + M[:, 2]; P_canon[1] = M[:, :2] @ caudal + M[:, 2]
        P_canon, c_ref = apply_refiners(img, M, P_canon, ids0, refiners, run_model.conf)
        if c_ref is not None:
            run_model.conf = c_ref

    # Inverse -> coordonnées image d'origine
    Minv = cv2.invertAffineTransform(M)
    P_orig = (Minv[:, :2] @ P_canon.T).T + Minv[:, 2]

    # Poisson courbé : le modèle fin a aussi placé les charnières 22/24 (modèles
    # entraînés avec --hinges) ; si elles s'écartent de l'axe 1-2, on redresse
    # le poisson le long de 1-22-24-2 et on refait la prédiction.
    ids = landmark_ids(cnn) if args.predictor.lower().endswith(".onnx") else list(range(1, P_orig.shape[0] + 1))
    curv, redressed = 0.0, False
    if 22 in ids:
        P_orig[0], P_orig[1] = snout, caudal
        P2, c2, curv, redressed = refine_curved(img, P_orig, ids, orient, cnn, args.target_len, cw, ch,
                                                thr=1.5 if args.straighten else float("inf"))
        if redressed:
            P_orig, run_model.conf = P2, c2

    # Épinglage : les points placés à la main écrasent la prédiction du modèle.
    # LM1 = museau, LM2 = caudale (toujours) ; LM3 = dorsal, LM4, LM7 si fournis.
    # (La propagation FISHMORPH vers les points dépendants 8/9/11, 5/6/13/14 est
    #  faite côté R par apply_fishmorph() après lecture de ce CSV.)
    if args.pin_clicks and P_orig.shape[0] >= 2:
        # en mode --auto, LM1/LM2 sont des détections : le 2e étage peut les réaffiner
        if not (args.auto and refiners and not user12[0]):
            P_orig[0] = snout
        if not (args.auto and refiners and not user12[1]):
            P_orig[1] = caudal
        if dorsal is not None and P_orig.shape[0] >= 3:
            P_orig[2] = dorsal          # LM3 (= clic dorsal, aussi utilisé pour l'orientation)
        if lm4 is not None and P_orig.shape[0] >= 4:
            P_orig[3] = lm4             # LM4 (ventral, à l'aplomb de LM3)
        if lm7 is not None and P_orig.shape[0] >= 7:
            P_orig[6] = lm7             # LM7 (référence tête)
        for idx, xy in extra_pins.items():   # LM10, LM12, LM16, LM18
            if P_orig.shape[0] > idx:
                P_orig[idx] = xy

    conf = run_model.conf
    if auto_conf is not None and conf is not None:
        conf = np.array(conf, float).copy()
        conf[:2] = auto_conf[:2]        # LM1-LM2 : confiance du réseau global
    rows = [{"landmark": ids[i], "X": round(P_orig[i, 0], 3), "Y": round(P_orig[i, 1], 3),
             "conf": round(float(conf[i]), 4) if conf is not None else ""}
            for i in range(P_orig.shape[0]) if ids[i] <= 19 or args.hinges]
    # Points d'échelle 20-21 depuis les clics
    n = 19
    for k, s in enumerate([args.scale1, args.scale2]):
        if s:
            p = parse_xy(s)
            rows.append({"landmark": n + 1 + k, "X": round(p[0], 3), "Y": round(p[1], 3), "conf": ""})

    with open(args.out, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=["landmark", "X", "Y", "conf"])
        w.writeheader(); w.writerows(rows)

    scale_mm_per_px = ""
    if args.scale1 and args.scale2 and args.scale_mm > 0:
        d = np.hypot(*(parse_xy(args.scale2) - parse_xy(args.scale1)))
        scale_mm_per_px = args.scale_mm / d if d > 0 else ""
    if args.auto:
        print(f"AUTO: LM1={snout[0]:.1f},{snout[1]:.1f} LM2={caudal[0]:.1f},{caudal[1]:.1f}"
              + (f" conf={auto_conf[0]:.3f},{auto_conf[1]:.3f}" if auto_conf is not None else ""))
    if 22 in ids:
        print(f"CURVE: {curv:.2f} % Bl" + (" (redressé)" if redressed else ""))
    print(f"OK: {len(rows)} landmarks -> {os.path.abspath(args.out)}"
          + (f" | mm/px={scale_mm_per_px:.5f}" if scale_mm_per_px else ""))


if __name__ == "__main__":
    main()
