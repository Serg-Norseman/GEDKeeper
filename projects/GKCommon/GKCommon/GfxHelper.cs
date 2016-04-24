/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "GEDKeeper".
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

using System.Drawing;
using System.Drawing.Drawing2D;

namespace GKCommon
{
    /// <summary>
    /// Description of GfxHelper.
    /// </summary>
    public static class GfxHelper
    {
        public static Color Darker(Color color, float fraction)
        {
            float factor = (1.0f - fraction);

            int rgb = color.ToArgb();
            int red = (rgb >> 16) & 0xFF;
            int green = (rgb >> 8) & 0xFF;
            int blue = (rgb >> 0) & 0xFF;
            //int alpha = (rgb >> 24) & 0xFF;

            red = (int) (red * factor);
            green = (int) (green * factor);
            blue = (int) (blue * factor);

            red = (red < 0) ? 0 : red;
            green = (green < 0) ? 0 : green;
            blue = (blue < 0) ? 0 : blue;

            return Color.FromArgb(red, green, blue);
        }

        public static Color Lighter(Color color, float fraction)
        {
            float factor = (1.0f + fraction);
            
            int rgb = color.ToArgb();
            int red = (rgb >> 16) & 0xFF;
            int green = (rgb >> 8) & 0xFF;
            int blue = (rgb >> 0) & 0xFF;
            //int alpha = (rgb >> 24) & 0xFF;

            red = (int) (red * factor);
            green = (int) (green * factor);
            blue = (int) (blue * factor);

            if (red < 0) {
                red = 0;
            } else if (red > 255) {
                red = 255;
            }
            if (green < 0) {
                green = 0;
            } else if (green > 255) {
                green = 255;
            }
            if (blue < 0) {
                blue = 0;
            } else if (blue > 255) {
                blue = 255;
            }

            //int alpha = color.getAlpha();

            return Color.FromArgb(red, green, blue);
        }

        public static float ZoomToFit(int imgWidth, int imgHeight, int requireWidth, int requireHeight)
        {
            float aspectRatio;

            if (imgWidth > imgHeight) {
                aspectRatio = (float)requireWidth / imgWidth;

                if (requireHeight < imgHeight * aspectRatio) {
                    aspectRatio = (float)requireHeight / imgHeight;
                }
            } else {
                aspectRatio = (float)requireHeight / imgHeight;

                if (requireWidth < imgWidth * aspectRatio) {
                    aspectRatio = (float)requireWidth / imgWidth;
                }
            }

            return aspectRatio;
        }

        public static GraphicsPath CreateRoundedRectangle(int x, int y, int width, int height, int radius)
        {
            int xw = x + width;
            int yh = y + height;
            int xwr = xw - radius;
            int yhr = yh - radius;
            int xr = x + radius;
            int yr = y + radius;
            int r2 = radius * 2;
            int xwr2 = xw - r2;
            int yhr2 = yh - r2;

            GraphicsPath p = new GraphicsPath();
            p.StartFigure();

            p.AddArc(x, y, r2, r2, 180, 90); // Top Left Corner
            p.AddLine(xr, y, xwr, y); // Top Edge
            p.AddArc(xwr2, y, r2, r2, 270, 90); // Top Right Corner
            p.AddLine(xw, yr, xw, yhr); // Right Edge
            p.AddArc(xwr2, yhr2, r2, r2, 0, 90); // Bottom Right Corner
            p.AddLine(xwr, yh, xr, yh); // Bottom Edge
            p.AddArc(x, yhr2, r2, r2, 90, 90); // Bottom Left Corner
            p.AddLine(x, yhr, x, yr); // Left Edge

            p.CloseFigure();
            return p;
        }

        public static void DrawPathWithFuzzyLine(Graphics gfx, GraphicsPath path, Color baseColor, int maxOpacity, int width, int opaqueWidth)
        {
            if (gfx == null || path == null) return;

            int numSteps = width - opaqueWidth + 1; // Number of pens we will use
            float delta = (float)maxOpacity / numSteps / numSteps; // Change in alpha between pens
            float alpha = delta; // Initial alpha

            for (int thickness = width; thickness >= opaqueWidth; thickness--)
            {
                Color color = Color.FromArgb((int)alpha, baseColor.R, baseColor.G, baseColor.B);

                using (Pen pen = new Pen(color, thickness))
                {
                    pen.EndCap = LineCap.Round;
                    pen.StartCap = LineCap.Round;
                    gfx.DrawPath(pen, path);
                }

                alpha += delta;
            }
        }
    }
}
