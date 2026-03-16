/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using Terminal.Gui;

namespace GKUI.Platform
{
    using tgColor = Terminal.Gui.Color;

    public static class ColorMapper
    {
        // Standard Windows Console Palette (CGA/EGA)
        public static readonly TrueColor[] Win16Palette =
        {
            new TrueColor(0, 0, 0),       // Black
            new TrueColor(0, 0, 128),     // DarkBlue
            new TrueColor(0, 128, 0),     // DarkGreen
            new TrueColor(0, 128, 128),   // DarkCyan
            new TrueColor(128, 0, 0),     // DarkRed
            new TrueColor(128, 0, 128),   // DarkMagenta
            new TrueColor(128, 128, 0),   // DarkYellow/Brown (0xC1, 0x9C, 0x00)
            new TrueColor(192, 192, 192), // Gray
            new TrueColor(128, 128, 128), // DarkGray
            new TrueColor(0, 0, 255),     // Blue
            new TrueColor(0, 255, 0),     // Green
            new TrueColor(0, 255, 255),   // Cyan
            new TrueColor(255, 0, 0),     // Red
            new TrueColor(255, 0, 255),   // Magenta
            new TrueColor(255, 255, 0),   // Yellow
            new TrueColor(255, 255, 255)  // White
        };

        public static tgColor GetClosestConsoleColor(TrueColor target)
        {
            double minDiff = double.MaxValue;
            int closestIndex = 0;

            for (int i = 0; i < Win16Palette.Length; i++) {
                // Use the Euclidean distance with weights
                // This is a fast and high-quality way to approximate perception without complex Lab formulas
                double diff = CalculateColorDiff(target, Win16Palette[i]);

                if (diff < minDiff) {
                    minDiff = diff;
                    closestIndex = i;
                }
            }

            return (tgColor)closestIndex;
        }

        private static double CalculateColorDiff(TrueColor c1, TrueColor c2)
        {
            // The Redmean formula is an excellent balance between speed and accuracy of perception.
            long rmean = ((long)c1.Red + c2.Red) / 2;
            long r = (long)c1.Red - c2.Red;
            long g = (long)c1.Green - c2.Green;
            long b = (long)c1.Blue - c2.Blue;
            return Math.Sqrt((((512 + rmean) * r * r) >> 8) + 4 * g * g + (((767 - rmean) * b * b) >> 8));
        }


        private static readonly (char chr, double weight)[] Shades = {
            (' ', 0.0), ('░', 0.25), ('▒', 0.5), ('▓', 0.75), ('█', 1.0)
        };

        public record PixColor(double r, double g, double b, tgColor fg, tgColor bg, char chr);

        public static List<PixColor> InitPixelColors()
        {
            var result = new List<PixColor>();

            for (int bg = 0; bg < Win16Palette.Length; bg++) {
                TrueColor bgEntry = Win16Palette[bg];

                for (int fg = 0; fg < Win16Palette.Length; fg++) {
                    TrueColor fgEntry = Win16Palette[fg];

                    if (fg == bg) continue;

                    foreach (var shade in Shades) {
                        // Calculating the mixed color
                        double r = bgEntry.Red * (1 - shade.weight) + fgEntry.Red * shade.weight;
                        double g = bgEntry.Green * (1 - shade.weight) + fgEntry.Green * shade.weight;
                        double b = bgEntry.Blue * (1 - shade.weight) + fgEntry.Blue * shade.weight;

                        result.Add(new PixColor(r, g, b, (tgColor)fg, (tgColor)bg, shade.chr));
                    }
                }
            }

            return result;
        }

        private static readonly PixColor Default = new PixColor(0, 0, 0, tgColor.Gray, tgColor.Black, ' ');

        public static PixColor FindBestMatch(List<PixColor> pixColors, int targetR, int targetG, int targetB)
        {
            var bestMatch = Default;

            double minDistance = double.MaxValue;
            foreach (var pixCol in pixColors) {
                // Euclidean distance
                double distance = Math.Sqrt(Math.Pow(targetR - pixCol.r, 2) + Math.Pow(targetG - pixCol.g, 2) + Math.Pow(targetB - pixCol.b, 2));

                if (distance < minDistance) {
                    minDistance = distance;
                    bestMatch = pixCol;
                }
            }

            return bestMatch;
        }
    }
}
