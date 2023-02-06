/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using BSLib;
using BSDColors = GKCore.Design.BSDConsts.Colors;

namespace GKCore.Charts
{
    public enum GfxBorderStyle
    {
        None,
        Single,
        Double,
        Triple,
        Sunken3D,
        Raised3D,
        SingleSquareCuts,
        DoubleSquareCuts,
        SingleRoundCuts,
        DoubleRoundCuts,
        SingleBevels,
        DoubleBevels,
        CrossCorners,
        RivetTape,

        Last = RivetTape
    }

    /// <summary>
    /// 
    /// </summary>
    public static class BorderPainter
    {
        private const int DefaultPenWidth = 1;
        private const int DefaultAngleOffset = 12;
        private const int DefaultStreak = 3;

        public static void DrawBorder(ChartRenderer renderer, ExtRect rect, GfxBorderStyle borderStyle)
        {
            int rL = rect.Left;
            int rT = rect.Top;
            int rR = rect.Right;
            int rB = rect.Bottom;

            switch (borderStyle) {
                case GfxBorderStyle.Single:
                    DrawSingleRect(renderer, rL, rT, rect.GetWidth(), rect.GetHeight(), DefaultPenWidth);
                    break;

                case GfxBorderStyle.Double:
                    DrawDoubleRect(renderer, rL, rT, rect.GetWidth(), rect.GetHeight(), DefaultStreak, DefaultPenWidth);
                    break;

                case GfxBorderStyle.Triple:
                    DrawTripleRect(renderer, rL, rT, rect.GetWidth(), rect.GetHeight(), DefaultStreak, DefaultPenWidth);
                    break;

                case GfxBorderStyle.Sunken3D:
                case GfxBorderStyle.Raised3D:
                    Draw3D(renderer, rL, rT, rR, rB, borderStyle, DefaultPenWidth);
                    break;

                case GfxBorderStyle.SingleSquareCuts:
                    DrawSingleSquareCuts(renderer, rL, rT, rR, rB, DefaultAngleOffset, DefaultPenWidth);
                    break;

                case GfxBorderStyle.DoubleSquareCuts:
                    DrawDoubleSquareCuts(renderer, rL, rT, rR, rB, DefaultAngleOffset, DefaultStreak, DefaultPenWidth);
                    break;

                case GfxBorderStyle.SingleRoundCuts:
                    DrawSingleRoundCuts(renderer, rL, rT, rR, rB, DefaultAngleOffset, DefaultPenWidth);
                    break;

                case GfxBorderStyle.DoubleRoundCuts:
                    DrawDoubleRoundCuts(renderer, rL, rT, rR, rB, DefaultAngleOffset, DefaultStreak, DefaultPenWidth);
                    break;

                case GfxBorderStyle.SingleBevels:
                    DrawSingleBevels(renderer, rL, rT, rR, rB, DefaultAngleOffset, DefaultPenWidth);
                    break;

                case GfxBorderStyle.DoubleBevels:
                    DrawDoubleBevels(renderer, rL, rT, rR, rB, DefaultAngleOffset, DefaultStreak, DefaultPenWidth);
                    break;

                case GfxBorderStyle.CrossCorners:
                    DrawCrossCorners(renderer, rL, rT, rR, rB, DefaultAngleOffset, DefaultPenWidth);
                    break;

                case GfxBorderStyle.RivetTape:
                    DrawRivetTape(renderer, rL, rT, rR, rB, DefaultAngleOffset, DefaultPenWidth);
                    break;
            }
        }

        private static void Draw3D(ChartRenderer renderer, int rL, int rT, int rR, int rB, GfxBorderStyle borderStyle, float pw)
        {
            int[] colors;
            bool sunken = (borderStyle == GfxBorderStyle.Sunken3D);
            if (sunken) {
                colors = new int[] {
                    BSDColors.ControlDark, BSDColors.ControlDarkDark, BSDColors.ControlLightLight, BSDColors.ControlLight
                };
            } else {
                colors = new int[] {
                    BSDColors.ControlLightLight, BSDColors.ControlLight, BSDColors.ControlDark, BSDColors.ControlDarkDark
                };
            }

            using (var p = renderer.CreatePen(colors[0], pw)) {
                renderer.DrawLine(p, rL, rB - 1, rL, rT); // L
                renderer.DrawLine(p, rL, rT, rR - 1, rT); // T
            }
            using (var p = renderer.CreatePen(colors[1], pw)) {
                renderer.DrawLine(p, rL + pw, rB - 2, rL + pw, rT + pw); // L
                renderer.DrawLine(p, rL + pw, rT + pw, rR - 2, rT + pw); // T
            }
            using (var p = renderer.CreatePen(colors[2], pw)) {
                renderer.DrawLine(p, rL, rB, rR, rB); // B
                renderer.DrawLine(p, rR, rB, rR, rT); // R
            }
            using (var p = renderer.CreatePen(colors[3], pw)) {
                renderer.DrawLine(p, rL + pw, rB - pw, rR - pw, rB - pw); // B
                renderer.DrawLine(p, rR - pw, rB - pw, rR - pw, rT + pw); // R
            }
        }

        private static void DrawSingleRect(ChartRenderer renderer, int rL, int rT, int rW, int rH, float pw)
        {
            using (var pen = renderer.CreatePen(BSDColors.Black, pw)) {
                renderer.DrawRectangle(pen, null, rL, rT, rW, rH);
            }
        }

        private static void DrawDoubleRect(ChartRenderer renderer, int rL, int rT, int rW, int rH, int streak, float pw)
        {
            using (var pen = renderer.CreatePen(BSDColors.Black, pw)) {
                renderer.DrawRectangle(pen, null, rL, rT, rW, rH);
                rL += streak;
                rT += streak;
                rW -= streak * 2;
                rH -= streak * 2;
                renderer.DrawRectangle(pen, null, rL, rT, rW, rH);
            }
        }

        private static void DrawTripleRect(ChartRenderer renderer, int rL, int rT, int rW, int rH, int streak, float pw)
        {
            using (var pen = renderer.CreatePen(BSDColors.Black, pw)) {
                renderer.DrawRectangle(pen, null, rL, rT, rW, rH);
                rL += streak;
                rT += streak;
                rW -= streak * 2;
                rH -= streak * 2;
                renderer.DrawRectangle(pen, null, rL, rT, rW, rH);
                rL += streak;
                rT += streak;
                rW -= streak * 2;
                rH -= streak * 2;
                renderer.DrawRectangle(pen, null, rL, rT, rW, rH);
            }
        }

        private static void DrawDoubleSquareCuts(ChartRenderer renderer, int rL, int rT, int rR, int rB, float gap, int streak, float pw)
        {
            DrawSingleSquareCuts(renderer, rL, rT, rR, rB, gap, pw);
            rL += streak;
            rT += streak;
            rR -= streak;
            rB -= streak;
            DrawSingleSquareCuts(renderer, rL, rT, rR, rB, gap, pw);
        }

        private static void DrawSingleSquareCuts(ChartRenderer renderer, int rL, int rT, int rR, int rB, float gap, float pw)
        {
            using (var p = renderer.CreatePen(BSDColors.Black, pw)) {
                renderer.DrawLine(p, rL, rB - gap, rL, rT + gap); // L
                renderer.DrawLine(p, rL + gap, rT, rR - gap, rT); // T
                renderer.DrawLine(p, rL + gap, rB, rR - gap, rB); // B
                renderer.DrawLine(p, rR, rB - gap, rR, rT + gap); // R

                // LT corner
                renderer.DrawLine(p, rL + gap, rT, rL + gap, rT + gap); // V
                renderer.DrawLine(p, rL, rT + gap, rL + gap, rT + gap); // H

                // RT corner
                renderer.DrawLine(p, rR - gap, rT, rR - gap, rT + gap); // V
                renderer.DrawLine(p, rR, rT + gap, rR - gap, rT + gap); // H

                // LB corner
                renderer.DrawLine(p, rL + gap, rB, rL + gap, rB - gap); // V
                renderer.DrawLine(p, rL, rB - gap, rL + gap, rB - gap); // H

                // RB corner
                renderer.DrawLine(p, rR - gap, rB, rR - gap, rB - gap); // V
                renderer.DrawLine(p, rR, rB - gap, rR - gap, rB - gap); // H
            }
        }

        private static void DrawDoubleBevels(ChartRenderer renderer, int rL, int rT, int rR, int rB, float gap, int streak, float pw)
        {
            DrawSingleBevels(renderer, rL, rT, rR, rB, gap, pw);
            rL += streak;
            rT += streak;
            rR -= streak;
            rB -= streak;
            DrawSingleBevels(renderer, rL, rT, rR, rB, gap, pw);
        }

        private static void DrawSingleBevels(ChartRenderer renderer, int rL, int rT, int rR, int rB, float gap, float pw)
        {
            using (var p = renderer.CreatePen(BSDColors.Black, pw)) {
                renderer.DrawLine(p, rL, rB - gap, rL, rT + gap); // L
                renderer.DrawLine(p, rL + gap, rT, rR - gap, rT); // T
                renderer.DrawLine(p, rL + gap, rB, rR - gap, rB); // B
                renderer.DrawLine(p, rR, rB - gap, rR, rT + gap); // R

                // LT corner
                renderer.DrawLine(p, rL, rT + gap, rL + gap, rT);

                // RT corner
                renderer.DrawLine(p, rR - gap, rT, rR, rT + gap);

                // LB corner
                renderer.DrawLine(p, rL + gap, rB, rL, rB - gap);

                // RB corner
                renderer.DrawLine(p, rR, rB - gap, rR - gap, rB);
            }
        }

        private static void DrawDoubleRoundCuts(ChartRenderer renderer, int rL, int rT, int rR, int rB, float gap, int streak, float pw)
        {
            DrawSingleRoundCuts(renderer, rL, rT, rR, rB, gap, pw);
            rL += streak;
            rT += streak;
            rR -= streak;
            rB -= streak;
            DrawSingleRoundCuts(renderer, rL, rT, rR, rB, gap, pw);
        }

        private static void DrawSingleRoundCuts(ChartRenderer renderer, int rL, int rT, int rR, int rB, float gap, float pw)
        {
            using (var p = renderer.CreatePen(BSDColors.Black, pw)) {
                renderer.DrawLine(p, rL, rB - gap, rL, rT + gap); // L
                renderer.DrawLine(p, rL + gap, rT, rR - gap, rT); // T
                renderer.DrawLine(p, rL + gap, rB, rR - gap, rB); // B
                renderer.DrawLine(p, rR, rB - gap, rR, rT + gap); // R

                float gap2 = gap * 2;

                // LT corner
                renderer.DrawArc(p, rL - gap, rT - gap, gap2, gap2, 0, 90);

                // RT corner
                renderer.DrawArc(p, rR - gap, rT - gap, gap2, gap2, 90, 90);

                // LB corner
                renderer.DrawArc(p, rL - gap, rB - gap, gap2, gap2, 270, 90);

                // RB corner
                renderer.DrawArc(p, rR - gap, rB - gap, gap2, gap2, 180, 90);
            }
        }

        private static void DrawCrossCorners(ChartRenderer renderer, int rL, int rT, int rR, int rB, float gap, float pw)
        {
            DrawDoubleSquareCuts(renderer, rL, rT, rR, rB, gap, DefaultStreak, pw);

            int xgap = (int)(gap / 2);
            rL += xgap;
            rT += xgap;
            rR -= xgap;
            rB -= xgap;

            using (var pen = renderer.CreatePen(BSDColors.Silver, DefaultStreak)) {
                renderer.DrawLine(pen, rL, rB, rL, rT); // L
                renderer.DrawLine(pen, rL, rT, rR, rT); // T
                renderer.DrawLine(pen, rL, rB, rR, rB); // B
                renderer.DrawLine(pen, rR, rB, rR, rT); // R
            }

            int halfStreak = DefaultStreak / 2;
            rL -= halfStreak;
            rT -= halfStreak;
            rR += halfStreak;
            rB += halfStreak;
            DrawSingleRect(renderer, rL, rT, (rR - rL), (rB - rT), pw);
            rL += DefaultStreak;
            rT += DefaultStreak;
            rR -= DefaultStreak;
            rB -= DefaultStreak;
            DrawSingleRect(renderer, rL, rT, (rR - rL), (rB - rT), pw);
        }

        public const int DarkGoldenrod = 0xB8860B;

        private static void DrawRivetTape(ChartRenderer renderer, int rL, int rT, int rR, int rB, float gap, float pw)
        {
            int tw = 12;
            int htw = 6;
            rL -= htw;
            rT -= htw;
            rR += htw;
            rB += htw;
            using (var p = renderer.CreatePen(BSDColors.Gray, tw))
                using (var rivet = renderer.CreateSphere(5, ChartRenderer.GetColor(/*DarkGoldenrod*/BSDColors.Silver))) {
                renderer.DrawLine(p, rL, rB + htw, rL, rT - htw); // L
                renderer.DrawLine(p, rL, rT, rR, rT); // T
                renderer.DrawLine(p, rL, rB, rR, rB); // B
                renderer.DrawLine(p, rR, rB + htw, rR, rT - htw); // R

                if (rivet != null) {
                    int hNum = ((rR - rL) + tw) / 20;
                    if (!MathHelper.IsOdd(hNum)) hNum -= 1;
                    int dist = ((rR - rL) + tw) / hNum;

                    for (int i = 0; i < hNum; i++) {
                        renderer.DrawImage(rivet, rL - htw / 2 + (i * dist), rT - 5, string.Empty);
                        renderer.DrawImage(rivet, rL - htw / 2 + (i * dist), rB - 5, string.Empty);
                    }

                    hNum = ((rB - rT) + tw) / 20;
                    if (!MathHelper.IsOdd(hNum)) hNum -= 1;
                    dist = ((rB - rT) + tw) / hNum;

                    for (int i = 0; i < hNum; i++) {
                        renderer.DrawImage(rivet, rL - 5, rT + tw / 2 + (i * dist), string.Empty);
                        renderer.DrawImage(rivet, rR - 5, rT + tw / 2 + (i * dist), string.Empty);
                    }
                }
            }

            using (var p = renderer.CreatePen(BSDColors.Black, 2)) {
                rL -= htw;
                rT -= htw;
                rR += htw;
                rB += htw;
                renderer.DrawLine(p, rL, rB, rL, rT); // L
                renderer.DrawLine(p, rL, rT, rR, rT); // T
                renderer.DrawLine(p, rL, rB, rR, rB); // B
                renderer.DrawLine(p, rR, rB, rR, rT); // R

                rL += tw;
                rT += tw;
                rR -= tw;
                rB -= tw;
                renderer.DrawLine(p, rL, rB, rL, rT); // L
                renderer.DrawLine(p, rL, rT, rR, rT); // T
                renderer.DrawLine(p, rL, rB, rR, rB); // B
                renderer.DrawLine(p, rR, rB, rR, rT); // R

                rL += 3;
                rT += 3;
                rR -= 3;
                rB -= 3;
                renderer.DrawLine(p, rL, rB, rL, rT); // L
                renderer.DrawLine(p, rL, rT, rR, rT); // T
                renderer.DrawLine(p, rL, rB, rR, rB); // B
                renderer.DrawLine(p, rR, rB, rR, rT); // R
            }
        }
    }
}
