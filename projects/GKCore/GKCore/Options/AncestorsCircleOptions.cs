/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKCommon;
using GKCore.Charts;
using GKCore.Interfaces;

namespace GKCore.Options
{
    /// <summary>
    /// 
    /// </summary>
    public class AncestorsCircleOptions : IOptions
    {
        public const int MAX_BRUSHES = 12;

        private static int[] DefBrushColor = new int[] {
            /* 00 */ ChartRenderer.Coral,
            /* 01 */ ChartRenderer.CadetBlue,
            /* 02 */ ChartRenderer.DarkGray,
            /* 03 */ ChartRenderer.Khaki,
            /* 04 */ ChartRenderer.LawnGreen,
            /* 05 */ ChartRenderer.Khaki,
            /* 06 */ ChartRenderer.HotPink,
            /* 07 */ ChartRenderer.Ivory,
            /* 08 */ ChartRenderer.Black, // text
            /* 09 */ ChartRenderer.Moccasin, // background and central
            /* 10 */ ChartRenderer.Black, // lines
            /* 11 */ ChartRenderer.PaleGreen // lines?
        };

        public bool ArcText; // TODO: to OptionsDlg
        public IColor[] BrushColor = new IColor[MAX_BRUSHES];
        public bool HideEmptySegments;

        public AncestorsCircleOptions()
        {
            for (int i = 0; i < MAX_BRUSHES; i++) {
                BrushColor[i] = ChartRenderer.GetColor(DefBrushColor[i]);
            }

            ArcText = true;
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
                var utils = AppHost.GfxProvider;

                for (int i = 0; i < MAX_BRUSHES; i++) {
                    BrushColor[i] = utils.CreateColor(iniFile.ReadInteger("AncestorsCircle", "Brush_"+Convert.ToString(i), DefBrushColor[i]));
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
