/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using GKCore.Design.Graphics;
using GKUI.Platform;
using GKUI.Platform.Handlers;
using SixLabors.ImageSharp;
using SixLabors.ImageSharp.PixelFormats;
using SixLabors.ImageSharp.Processing;
using Terminal.Gui;

namespace GKUI.Components
{
    using sysRune = System.Rune;
    using tgAttribute = Terminal.Gui.Attribute;
    using tgColor = Terminal.Gui.Color;

    public class ImageView : View
    {
        private Dictionary<TrueColor, BrSym> fAttributeCache;
        private BrSym[,] fSymMatrix;
        private Image<Rgba32> fImage;
        private Rect fOldSize = Rect.Empty;

        public ImageView()
        {
            fAttributeCache = new Dictionary<TrueColor, BrSym>();
        }

        public void OpenImage(object controller, IImage image)
        {
            fImage = ((ImageHandler)image).Handle;
            fAttributeCache.Clear();
            SetNeedsDisplay();
        }

        public override void Redraw(Rect bounds)
        {
            base.Redraw(bounds);

            if (fOldSize != bounds) {
                fOldSize = bounds;

                var termSize = UIHelper.CalculateTerminalSize(fImage.Width, fImage.Height, bounds.Width, bounds.Height);
                var imgScaled = fImage.Clone(x => x.Resize(termSize.Width, termSize.Height, KnownResamplers.Robidoux));
                fSymMatrix = GenerateImageMatrix(imgScaled.Width, imgScaled.Height, imgScaled);
            }

            if (fSymMatrix != null) {
                for (int y = 0; y < fSymMatrix.GetLength(0); y++) {
                    for (int x = 0; x < fSymMatrix.GetLength(1); x++) {
                        var cell = fSymMatrix[y, x];
                        Driver.SetAttribute(cell.Attribute);
                        AddRune(x, y, cell.Symbol);
                    }
                }
            }
        }

        private record BrSym(sysRune Symbol, tgAttribute Attribute);

        private BrSym[,] GenerateImageMatrix(int widthPixels, int heightPixels, Image<Rgba32> sourceImage)
        {
            var matrix = new BrSym[heightPixels, widthPixels];

            for (int y = 0; y < heightPixels; y++) {
                for (int x = 0; x < widthPixels; x++) {
                    Rgba32 pixel = sourceImage[x, y];

                    var trueColor = new TrueColor(pixel.R, pixel.G, pixel.B);
                    if (!fAttributeCache.TryGetValue(trueColor, out var brSym)) {
                        var fbPair = FindBestMatch(pixel.R, pixel.G, pixel.B);
                        brSym = new BrSym(fbPair.symbol, new tgAttribute((tgColor)fbPair.fg, (tgColor)fbPair.bg));
                        fAttributeCache[trueColor] = brSym;
                    }

                    matrix[y, x] = new BrSym(brSym.Symbol, brSym.Attribute);

                    //var color = ColorMapper.GetClosestConsoleColor(new TrueColor(pixel.R, pixel.G, pixel.B));
                    //matrix[y, x] = new BrSym('x', GetAttribute(color)); // '\x2591'
                }
            }

            return matrix;
        }

        private static readonly (char chr, double weight)[] Shades = {
            (' ', 0.0), ('░', 0.25), ('▒', 0.5), ('▓', 0.75), ('█', 1.0)
        };

        private record FBPair(int fg, int bg, char symbol);

        private static FBPair FindBestMatch(int targetR, int targetG, int targetB)
        {
            double minDistance = double.MaxValue;
            var bestMatch = new FBPair(fg: 7 /*Gray*/, bg: 0 /*Black*/, symbol: ' ');

            for (int bg = 0; bg < ColorMapper.Win16Palette.Length; bg++) {
                TrueColor bgEntry = ColorMapper.Win16Palette[bg];

                for (int fg = 0; fg < ColorMapper.Win16Palette.Length; fg++) {
                    TrueColor fgEntry = ColorMapper.Win16Palette[fg];

                    foreach (var shade in Shades) {
                        // Calculating the mixed color
                        double r = bgEntry.Red * (1 - shade.weight) + fgEntry.Red * shade.weight;
                        double g = bgEntry.Green * (1 - shade.weight) + fgEntry.Green * shade.weight;
                        double b = bgEntry.Blue * (1 - shade.weight) + fgEntry.Blue * shade.weight;

                        // Euclidean distance
                        double distance = Math.Sqrt(Math.Pow(targetR - r, 2) + Math.Pow(targetG - g, 2) + Math.Pow(targetB - b, 2));

                        if (distance < minDistance) {
                            minDistance = distance;
                            bestMatch = new FBPair(fg, bg, shade.chr);
                        }
                    }
                }
            }

            return bestMatch;
        }
    }
}
