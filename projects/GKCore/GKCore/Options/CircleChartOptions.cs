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

using System;
using BSLib;
using GKCore.Charts;
using GKCore.Design.Graphics;
using GKCore.Interfaces;
using BSDColors = GKCore.Design.BSDConsts.Colors;

namespace GKCore.Options
{
    /// <summary>
    /// Class of options for circle charts. Common for the circles of ancestors and the circles of descendants.
    /// </summary>
    public class CircleChartOptions : IOptions
    {
        public const int MAX_BRUSHES = 12;

        private static int[] DefBrushColor = new int[] {
            /* 00 */ BSDColors.Coral,
            /* 01 */ BSDColors.CadetBlue,
            /* 02 */ BSDColors.DarkGray,
            /* 03 */ BSDColors.Khaki,
            /* 04 */ BSDColors.LawnGreen,
            /* 05 */ BSDColors.Khaki,
            /* 06 */ BSDColors.HotPink,
            /* 07 */ BSDColors.Ivory,
            /* 08 */ BSDColors.Black, // text
            /* 09 */ BSDColors.Moccasin, // background and central
            /* 10 */ BSDColors.Black, // lines
            /* 11 */ BSDColors.PaleGreen // lines?
        };

        public bool ArcText;
        public IColor[] BrushColor = new IColor[MAX_BRUSHES];
        public bool HideEmptySegments;
        public bool LTRCorrection; // text correction from left to right [experimental]

        public CircleChartOptions()
        {
            ResetDefaults();
        }

        public void ResetDefaults()
        {
            for (int i = 0; i < MAX_BRUSHES; i++) {
                BrushColor[i] = ChartRenderer.GetColor(DefBrushColor[i]);
            }

            ArcText = true;
            HideEmptySegments = false;
            LTRCorrection = false;
        }

        public void Assign(IOptions source)
        {
            CircleChartOptions srcOptions = source as CircleChartOptions;
            if (srcOptions == null) return;

            for (int i = 0; i < MAX_BRUSHES; i++) {
                BrushColor[i] = srcOptions.BrushColor[i];
            }

            ArcText = srcOptions.ArcText;
            HideEmptySegments = srcOptions.HideEmptySegments;
            LTRCorrection = srcOptions.LTRCorrection;
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            try {
                var utils = AppHost.GfxProvider;

                for (int i = 0; i < MAX_BRUSHES; i++) {
                    BrushColor[i] = utils.CreateColor(iniFile.ReadInteger("AncestorsCircle", "Brush_" + Convert.ToString(i), DefBrushColor[i]));
                }

                ArcText = iniFile.ReadBool("AncestorsCircle", "ArcText", true);
                HideEmptySegments = iniFile.ReadBool("AncestorsCircle", "HideEmptySegments", false);
                LTRCorrection = iniFile.ReadBool("AncestorsCircle", "LTRCorrection", false);
            } catch (Exception) {
                throw new PedigreeOptionsException("Error loading AncestorsCircleOptions");
            }
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            for (int i = 0; i < MAX_BRUSHES; i++) {
                iniFile.WriteInteger("AncestorsCircle", "Brush_"+Convert.ToString(i), BrushColor[i].ToArgb());
            }

            iniFile.WriteBool("AncestorsCircle", "ArcText", ArcText);
            iniFile.WriteBool("AncestorsCircle", "HideEmptySegments", HideEmptySegments);
            iniFile.WriteBool("AncestorsCircle", "LTRCorrection", LTRCorrection);
        }
    }
}
