/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GKCore.Charts;
using GKCore.Design.Graphics;

namespace GKCore.Options
{
    /// <summary>
    /// Class of options for circle charts. Common for the circles of ancestors and the circles of descendants.
    /// </summary>
    public class CircleChartOptions : IOptions
    {
        public const int MAX_BRUSHES = 12;

        private static readonly int[] DefBrushColor = new int[] {
            /* 00 */ GKColors.Coral,
            /* 01 */ GKColors.CadetBlue,
            /* 02 */ GKColors.DarkGray,
            /* 03 */ GKColors.Khaki,
            /* 04 */ GKColors.LawnGreen,
            /* 05 */ GKColors.Khaki,
            /* 06 */ GKColors.HotPink,
            /* 07 */ GKColors.Ivory,
            /* 08 */ GKColors.Black, // text
            /* 09 */ GKColors.Moccasin, // background and central
            /* 10 */ GKColors.Black, // lines
            /* 11 */ GKColors.PaleGreen // lines?
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
                throw new ArgumentNullException(nameof(iniFile));

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
                throw new ArgumentNullException(nameof(iniFile));

            for (int i = 0; i < MAX_BRUSHES; i++) {
                iniFile.WriteInteger("AncestorsCircle", "Brush_"+Convert.ToString(i), BrushColor[i].ToArgb());
            }

            iniFile.WriteBool("AncestorsCircle", "ArcText", ArcText);
            iniFile.WriteBool("AncestorsCircle", "HideEmptySegments", HideEmptySegments);
            iniFile.WriteBool("AncestorsCircle", "LTRCorrection", LTRCorrection);
        }
    }
}
