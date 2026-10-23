#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ruler_scale.py -- échelle (mm/px) d'une photographie à partir de la RÈGLE graduée.

Deux temps :
  1. localisation approximative de la règle : deux points 20-21 (réseau
     `mlmorph_run_ruler`, ou deux points fournis) qui donnent la position, la
     direction et un ordre de grandeur de 10 mm ;
  2. mesure fine par analyse spectrale : une bande de l'image est échantillonnée
     le long de la règle, son profil d'intensité moyen contient la périodicité des
     graduations millimétriques, dont la fréquence est mesurée par transformée de
     Fourier (avec affinage sub-fréquentiel). L'angle de la bande est ajusté pour
     maximiser la netteté du pic. Précision attendue : bien meilleure que deux
     clics, la période étant moyennée sur plusieurs dizaines de graduations.

Rend mm/px, la qualité du pic (rapport pic / bruit) et deux points 20-21 posés à
`ruler_mm` l'un de l'autre sur la règle.
"""
import math
import warnings
import numpy as np
import cv2


def _nanmean0(S):
    """Moyenne par colonne en ignorant les pixels hors image (bandes au bord)."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", RuntimeWarning)
        return np.nanmean(S, 0)


def _strip(gray, c, u, length, offs, width):
    """Bande d'image le long de u (centrée en c, décalée de offs perpendiculairement),
    de longueur `length` px et largeur `width` px -> image (width x length)."""
    n = np.array([-u[1], u[0]])
    t = np.arange(length) - length / 2.0
    s = np.arange(width) - width / 2.0 + offs
    X = c[0] + t[None, :] * u[0] + s[:, None] * n[0]
    Y = c[1] + t[None, :] * u[1] + s[:, None] * n[1]
    return cv2.remap(gray, X.astype(np.float32), Y.astype(np.float32), cv2.INTER_LINEAR,
                     borderMode=cv2.BORDER_CONSTANT, borderValue=np.nan)


def _spectrum_peak(profile, pmin, pmax):
    """Période dominante (px) dans [pmin, pmax] d'un profil 1-D ; rend (période,
    rapport pic/bruit)."""
    if not (np.isfinite(pmin) and np.isfinite(pmax) and 0 < pmin < pmax):
        return np.nan, 0.0
    if not (np.isfinite(pmin) and np.isfinite(pmax) and 0 < pmin < pmax):
        return np.nan, 0.0
    p = profile[np.isfinite(profile)]
    if len(p) < 4 * pmax:
        return np.nan, 0.0
    p = p - np.convolve(p, np.ones(int(2 * pmax) | 1) / (int(2 * pmax) | 1), mode="same")   # tendance
    k = max(1, int(pmax))
    p = p[k:-k] if len(p) > 6 * pmax else p
    if len(p) < 16:
        return np.nan, 0.0
    p = (p - p.mean()) * np.hanning(len(p))
    nfft = 1 << int(math.ceil(math.log2(len(p) * 8)))
    F = np.abs(np.fft.rfft(p, nfft))
    f = np.fft.rfftfreq(nfft)
    band = (f >= 1 / pmax) & (f <= 1 / pmin)
    if band.sum() < 3:
        return np.nan, 0.0
    i = np.flatnonzero(band)[np.argmax(F[band])]
    # affinage parabolique (log)
    if 0 < i < len(F) - 1:
        a, b, c = np.log(F[i - 1:i + 2] + 1e-12)
        d = 0.5 * (a - c) / (a - 2 * b + c) if (a - 2 * b + c) != 0 else 0.0
        fi = f[i] + float(np.clip(d, -0.5, 0.5)) * (f[1] - f[0])
    else:
        fi = f[i]
    if not fi > 0:
        return np.nan, 0.0
    if not fi > 0:
        return np.nan, 0.0
    noise = np.median(F[band]) + 1e-9
    return 1.0 / fi, float(F[i] / noise)


def ruler_scale(img, p20, p21, ruler_mm=10.0, n_angles=13, max_angle=6.0, n_offsets=11,
                span=3.0, offset_frac=1.0, period_hint=None, hint_span=1.4):
    """img : BGR ; p20, p21 : position approximative de deux graduations distantes
    d'environ ruler_mm. Rend dict(mm_per_px, period_px, snr, angle, p20, p21) ou None."""
    gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY).astype(np.float32) if img.ndim == 3 else img.astype(np.float32)
    p20, p21 = np.asarray(p20, float), np.asarray(p21, float)
    d = float(np.hypot(*(p21 - p20)))
    if not np.isfinite(d) or d < 5:
        return None
    u0 = (p21 - p20) / d
    c = (p20 + p21) / 2
    per_guess = d / ruler_mm                         # période millimétrique attendue (px)
    if period_hint is not None and np.isfinite(period_hint) and period_hint > 0:
        # ordre de grandeur donné par le réseau de période (train_ruler_period.py) :
        # fenêtre étroite, plus besoin des heuristiques d'harmoniques
        per_guess, span = float(period_hint), hint_span
        d = max(d, 10 * per_guess)
    if per_guess < 1.5:                              # graduations non résolues
        return None
    # la longueur 20-21 du réseau n'est qu'un ordre de grandeur (les deux graduations
    # cliquées n'ont pas d'identité) : recherche de la période dans [guess/span, guess*span]
    pmin, pmax = max(2.5, per_guess / span), per_guess * span
    H, W = gray.shape
    length = int(min(max(5 * d, 40 * per_guess), 1.2 * math.hypot(H, W)))
    width = max(3, int(round(2.0 * per_guess)))
    best = None
    base = math.atan2(u0[1], u0[0])
    for da in np.linspace(-max_angle, max_angle, n_angles):
        a = base + math.radians(da)
        u = np.array([math.cos(a), math.sin(a)])
        for off in np.linspace(-offset_frac * d, offset_frac * d, n_offsets):   # bande des graduations
            S = _strip(gray, c, u, length, off, width)
            prof = _nanmean0(S)
            per, snr = _spectrum_peak(prof, pmin, pmax)
            if np.isfinite(per) and (best is None or snr > best[1]):
                best = (per, snr, da, off, u)
    if best is None:
        return None
    per, snr, da, off, u = best
    # passe fine sur l'angle autour du meilleur
    for dd in np.linspace(-0.5, 0.5, 5) * (2 * max_angle / max(n_angles - 1, 1)):
        a = math.atan2(u[1], u[0]) + math.radians(dd)
        uu = np.array([math.cos(a), math.sin(a)])
        S = _strip(gray, c, uu, length, off, width)
        p2, s2 = _spectrum_peak(_nanmean0(S), pmin, pmax)
        if np.isfinite(p2) and s2 > snr:
            per, snr, u = p2, s2, uu
    # graduations au demi-millimètre : le pic le plus fort peut être celui de 0,5 mm ;
    # s'il existe aussi un pic net à la période double, c'est elle, le millimètre
    # (testé sur toutes les bandes : les longues graduations du millimètre et les
    # courtes du demi-millimètre ne sont pas à la même distance du bord)
    # pic le plus fort au centimètre ou au demi-centimètre (traits longs, chiffres,
    # graduations millimétriques floues) : s'il existe un pic net à la période /10
    # ou /5, c'est elle, le millimètre
    if period_hint is not None and np.isfinite(period_hint) and period_hint > 0:
        mmpx = 1.0 / per
        q21 = p20 + u * (ruler_mm / mmpx) * (1 if np.dot(u, u0) >= 0 else -1)
        return dict(mm_per_px=mmpx, period_px=per, snr=snr, angle=math.degrees(math.atan2(u[1], u[0])),
                    p20=p20, p21=q21)
    prof = _nanmean0(_strip(gray, c, u, length, off, width))
    for div in (10, 5):
        q, sq = _spectrum_peak(prof, max(2.0, 0.92 * per / div), 1.08 * per / div)
        if np.isfinite(q) and sq > 8 and sq > 0.25 * snr and abs(per / q - div) < 0.4:
            per, snr = q, max(snr, sq)
            break
    # recherche dense (pas d'une demi-période double), bande de 4 périodes : la zone
    # des longs traits du millimètre est étroite
    w2 = max(3, int(round(4 * per)))
    span_off = max(offset_frac * d, 12 * per)
    for off2 in np.arange(-span_off, span_off + 1e-9, max(1.0, per)):
        prof = _nanmean0(_strip(gray, c, u, length, off2, w2))
        p1, s1 = _spectrum_peak(prof, 0.9 * per, 1.1 * per)
        p2, s2 = _spectrum_peak(prof, 1.8 * per, 2.2 * per)
        if (np.isfinite(p2) and s2 > 10 and abs(p2 / per - 2) < 0.1
                and (s2 > 0.3 * max(s1, 1e-9) or s2 > 0.2 * snr)):
            per = p2
            break
    mmpx = 1.0 / per
    q20 = p20
    q21 = p20 + u * (ruler_mm / mmpx) * (1 if np.dot(u, u0) >= 0 else -1)
    return dict(mm_per_px=mmpx, period_px=per, snr=snr, angle=math.degrees(math.atan2(u[1], u[0])),
                p20=q20, p21=q21)
