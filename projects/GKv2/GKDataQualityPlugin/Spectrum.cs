/*
 *  "ChemLab", Desktop helper application for chemists.
 *  Copyright (C) 1996-1998, 2015 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Drawing;

namespace GKDataQualityPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public static class Spectrum
    {
        public const double WavelengthMinimum = 380.0;
        public const double WavelengthMaximum = 780.0;
        public const double BlueWavelength = WavelengthMinimum;
        public const double RedWavelength = WavelengthMaximum;

        public const double ColdWavelength = 520.0;

        private const double Gamma = 0.8;
        private const int IntensityMax = 255;

        private static int adjust(double color, double factor)
        {
            int result = (color == 0.0f) ? 0 : (int)(Math.Round(IntensityMax * Math.Pow((color * factor), Gamma)));

            if (result < 0) {
                result = 0;
            }
            if (result > 255) {
                result = 255;
            }

            return result;
        }

        public static Color WavelengthToRGB(double wavelength)
        {
            try {
                int wl = (int)(wavelength);
                double red, green, blue;

                if (wl >= 380 && wl <= 439) {
                    red = -(wavelength - 440) / (440 - 380);
                    green = 0.0;
                    blue = 1.0;
                } else if (wl >= 440 && wl <= 489) {
                    red = 0.0;
                    green = (wavelength - 440) / (490 - 440);
                    blue = 1.0;
                } else if (wl >= 490 && wl <= 509) {
                    red = 0.0;
                    green = 1.0;
                    blue = -(wavelength - 510) / (510 - 490);
                } else if (wl >= 510 && wl <= 579) {
                    red = (wavelength - 510) / (580 - 510);
                    green = 1.0;
                    blue = 0.0;
                } else if (wl >= 580 && wl <= 644) {
                    red = 1.0;
                    green = -(wavelength - 645) / (645 - 580);
                    blue = 0.0;
                } else if (wl >= 645 && wl <= 780) {
                    red = 1.0;
                    green = 0.0;
                    blue = 0.0;
                } else {
                    red = 0.0;
                    green = 0.0;
                    blue = 0.0;
                }

                double factor;
                // интенсивность должна ослабевать около пределов зрения
                if (wl >= 380 && wl <= 419) {
                    factor = 0.3 + 0.7 * (wavelength - 380) / (420 - 380);
                } else if (wl >= 420 && wl <= 700) {
                    factor = 1.0;
                } else if (wl >= 701 && wl <= 780) {
                    factor = 0.3 + 0.7 * (780 - wavelength) / (780 - 700);
                } else {
                    factor = 0.0;
                }

                int R = adjust(red, factor);
                int G = adjust(green, factor);
                int B = adjust(blue, factor);

                return Color.FromArgb(R, G, B);
            } catch (Exception) {
                return Color.Black;
            }
        }
    }
}
