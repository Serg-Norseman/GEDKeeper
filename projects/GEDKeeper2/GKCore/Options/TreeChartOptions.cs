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
    public sealed class TreeChartOptions : BaseObject, IOptions
    {
        public bool ChildlessExclude;
        public bool Decorative;
        public bool FamilyVisible;
        public bool NameVisible;
        public bool PatronymicVisible;
        public bool NickVisible;
        public bool DiffLines;
        public bool BirthDateVisible;
        public bool DeathDateVisible;
        public bool OnlyYears;
        public bool Kinship;
        public bool PortraitsVisible;
        public bool DefaultPortraits;
        public bool SignsVisible;
        public bool CertaintyIndexVisible;
        public bool TraceSelected;

        public Color MaleColor;
        public Color FemaleColor;
        public Color UnkSexColor;
        public Color UnHusbandColor;
        public Color UnWifeColor;

        public string DefFontName;
        public int DefFontSize;
        public Color DefFontColor;
        public FontStyle DefFontStyle;

        public TreeChartOptions()
        {
            this.FamilyVisible = true;
            this.NameVisible = true;
            this.PatronymicVisible = true;
            this.NickVisible = true;
            this.DiffLines = true;

            this.BirthDateVisible = true;
            this.DeathDateVisible = true;
            this.OnlyYears = true;

            this.Kinship = false;
            this.PortraitsVisible = true;
            this.DefaultPortraits = false;
            this.SignsVisible = false;
            this.CertaintyIndexVisible = false;
            this.TraceSelected = true;
            this.ChildlessExclude = false;
            this.Decorative = true;

            this.MaleColor = Color.FromArgb(-3750145);
            this.FemaleColor = Color.FromArgb(-14650);
            this.UnkSexColor = Color.FromArgb(-14593);
            this.UnHusbandColor = Color.FromArgb(-2631681);
            this.UnWifeColor = Color.FromArgb(-10281);

            this.DefFontName = "Verdana";
            this.DefFontSize = 8;
            this.DefFontColor = Color.Black;
            this.DefFontStyle = FontStyle.Regular;
        }

        public void Assign(IOptions source)
        {
            TreeChartOptions srcOptions = source as TreeChartOptions;
            if (srcOptions == null) return;

            this.FamilyVisible = srcOptions.FamilyVisible;
            this.NameVisible = srcOptions.NameVisible;
            this.PatronymicVisible = srcOptions.PatronymicVisible;
            this.NickVisible = srcOptions.NickVisible;
            this.DiffLines = srcOptions.DiffLines;
            this.BirthDateVisible = srcOptions.BirthDateVisible;
            this.DeathDateVisible = srcOptions.DeathDateVisible;
            this.OnlyYears = srcOptions.OnlyYears;
            this.Kinship = srcOptions.Kinship;
            this.PortraitsVisible = srcOptions.PortraitsVisible;
            this.DefaultPortraits = srcOptions.DefaultPortraits;
            this.SignsVisible = srcOptions.SignsVisible;
            this.CertaintyIndexVisible = srcOptions.CertaintyIndexVisible;
            this.TraceSelected = srcOptions.TraceSelected;
            this.ChildlessExclude = srcOptions.ChildlessExclude;
            this.Decorative = srcOptions.Decorative;
            this.MaleColor = srcOptions.MaleColor;
            this.FemaleColor = srcOptions.FemaleColor;
            this.UnkSexColor = srcOptions.UnkSexColor;
            this.UnHusbandColor = srcOptions.UnHusbandColor;
            this.UnWifeColor = srcOptions.UnWifeColor;
            this.DefFontName = srcOptions.DefFontName;
            this.DefFontSize = srcOptions.DefFontSize;
            this.DefFontColor = srcOptions.DefFontColor;
            this.DefFontStyle = srcOptions.DefFontStyle;
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            this.FamilyVisible = iniFile.ReadBool("Chart", "FamilyVisible", true);
            this.NameVisible = iniFile.ReadBool("Chart", "NameVisible", true);
            this.PatronymicVisible = iniFile.ReadBool("Chart", "PatronymicVisible", true);
            this.NickVisible = iniFile.ReadBool("Chart", "NickVisible", true);
            this.DiffLines = iniFile.ReadBool("Chart", "DiffLines", true);

            this.BirthDateVisible = iniFile.ReadBool("Chart", "BirthDateVisible", true);
            this.DeathDateVisible = iniFile.ReadBool("Chart", "DeathDateVisible", true);
            this.OnlyYears = iniFile.ReadBool("Chart", "OnlyYears", true);

            this.Kinship = iniFile.ReadBool("Chart", "Kinship", false);
            this.SignsVisible = iniFile.ReadBool("Chart", "SignsVisible", false);
            this.PortraitsVisible = iniFile.ReadBool("Chart", "PortraitsVisible", true);
            this.DefaultPortraits = iniFile.ReadBool("Chart", "DefaultPortraits", false);
            this.CertaintyIndexVisible = iniFile.ReadBool("Chart", "CertaintyIndexVisible", false);
            this.TraceSelected = iniFile.ReadBool("Chart", "TraceSelected", true);
            this.ChildlessExclude = iniFile.ReadBool("Chart", "ChildlessExclude", false);
            this.Decorative = iniFile.ReadBool("Chart", "Decorative", true);

            this.MaleColor = Color.FromArgb(iniFile.ReadInteger("Chart", "MaleColor", -3750145));
            this.FemaleColor = Color.FromArgb(iniFile.ReadInteger("Chart", "FemaleColor", -14650));
            this.UnkSexColor = Color.FromArgb(iniFile.ReadInteger("Chart", "UnkSexColor", -14593));
            this.UnHusbandColor = Color.FromArgb(iniFile.ReadInteger("Chart", "UnHusbandColor", -2631681));
            this.UnWifeColor = Color.FromArgb(iniFile.ReadInteger("Chart", "UnWifeColor", -10281));

            this.DefFontName = iniFile.ReadString("Chart", "FontName", "Verdana");
            this.DefFontSize = iniFile.ReadInteger("Chart", "FontSize", 8);
            this.DefFontColor = Color.FromArgb(iniFile.ReadInteger("Chart", "FontColor", Color.Black.ToArgb()));
            this.DefFontStyle = (FontStyle)((uint)iniFile.ReadInteger("Chart", "FontStyle", 0));
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            iniFile.WriteBool("Chart", "FamilyVisible", this.FamilyVisible);
            iniFile.WriteBool("Chart", "NameVisible", this.NameVisible);
            iniFile.WriteBool("Chart", "PatronymicVisible", this.PatronymicVisible);
            iniFile.WriteBool("Chart", "NickVisible", this.NickVisible);
            iniFile.WriteBool("Chart", "DiffLines", this.DiffLines);

            iniFile.WriteBool("Chart", "BirthDateVisible", this.BirthDateVisible);
            iniFile.WriteBool("Chart", "DeathDateVisible", this.DeathDateVisible);
            iniFile.WriteBool("Chart", "OnlyYears", this.OnlyYears);

            iniFile.WriteBool("Chart", "Kinship", this.Kinship);
            iniFile.WriteBool("Chart", "SignsVisible", this.SignsVisible);
            iniFile.WriteBool("Chart", "PortraitsVisible", this.PortraitsVisible);
            iniFile.WriteBool("Chart", "DefaultPortraits", this.DefaultPortraits);
            iniFile.WriteBool("Chart", "CertaintyIndexVisible", this.CertaintyIndexVisible);
            iniFile.WriteBool("Chart", "TraceSelected", this.TraceSelected);
            iniFile.WriteBool("Chart", "ChildlessExclude", this.ChildlessExclude);
            iniFile.WriteBool("Chart", "Decorative", this.Decorative);

            iniFile.WriteInteger("Chart", "MaleColor", this.MaleColor.ToArgb());
            iniFile.WriteInteger("Chart", "FemaleColor", this.FemaleColor.ToArgb());
            iniFile.WriteInteger("Chart", "UnkSexColor", this.UnkSexColor.ToArgb());
            iniFile.WriteInteger("Chart", "UnHusbandColor", this.UnHusbandColor.ToArgb());
            iniFile.WriteInteger("Chart", "UnWifeColor", this.UnWifeColor.ToArgb());

            iniFile.WriteString("Chart", "FontName", this.DefFontName);
            iniFile.WriteInteger("Chart", "FontSize", this.DefFontSize);
            iniFile.WriteInteger("Chart", "FontColor", this.DefFontColor.ToArgb());
            iniFile.WriteInteger("Chart", "FontStyle", (byte)this.DefFontStyle);
        }
    }
}
