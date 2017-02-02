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

using System;
using System.Drawing;
using GKCommon;
using GKCore.Interfaces;

namespace GKCore.Options
{
    /// <summary>
    /// 
    /// </summary>
    public class AncestorsCircleOptions : IOptions
    {
        public const int MAX_BRUSHES = 12;

        private static Color[] DefBrushColor = new Color[] {
            Color.Coral,
            Color.CadetBlue,
            Color.DarkGray,
            Color.Khaki,
            Color./*CadetBlue,*/LawnGreen,
            Color./*DarkGray,*/Khaki,
            Color./*Khaki,*/HotPink,
            Color./*CadetBlue,*/Ivory,
            Color.Black, // text
            Color.Moccasin, // background
            Color.Black, // lines
            Color.PaleGreen // lines
        };

        public bool ArcText;
        public Color[] BrushColor = new Color[MAX_BRUSHES];
        public bool HideEmptySegments;

        public AncestorsCircleOptions()
        {
            for (int i = 0; i < MAX_BRUSHES; i++) {
                BrushColor[i] = DefBrushColor[i];
            }

            ArcText = false;
            HideEmptySegments = false;
        }

        public void Assign(IOptions source)
        {
            AncestorsCircleOptions srcOptions = source as AncestorsCircleOptions;
            if (srcOptions == null) return;

            for (int i = 0; i < MAX_BRUSHES; i++) {
                BrushColor[i] = srcOptions.BrushColor[i];
            }

            ArcText = srcOptions.ArcText;
            HideEmptySegments = srcOptions.HideEmptySegments;
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            try
            {
                for (int i = 0; i < MAX_BRUSHES; i++) {
                    BrushColor[i] = Color.FromArgb(iniFile.ReadInteger("AncestorsCircle", "Brush_"+Convert.ToString(i), DefBrushColor[i].ToArgb()));
                }

                HideEmptySegments = iniFile.ReadBool("AncestorsCircle", "HideEmptySegments", false);
            }
            catch (Exception)
            {
                throw new EPedigreeOptionsException("Error loading AncestorsCircleOptions");
            }
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            for (int i = 0; i < MAX_BRUSHES; i++) {
                iniFile.WriteInteger("AncestorsCircle", "Brush_"+Convert.ToString(i), BrushColor[i].ToArgb());
            }

            iniFile.WriteBool("AncestorsCircle", "HideEmptySegments", HideEmptySegments);
        }
    }
}
