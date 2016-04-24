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
using GKCommon;

namespace GKCore.Options
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TreeChartOptions : BaseObject
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
            this.ChildlessExclude = false;
            this.Decorative = true;
            this.FamilyVisible = true;
            this.NameVisible = true;
            this.PatronymicVisible = true;
            this.NickVisible = false;
            this.DiffLines = false;
            this.BirthDateVisible = false;
            this.DeathDateVisible = false;
            this.OnlyYears = false;
            this.Kinship = false;
            this.PortraitsVisible = true;
            this.SignsVisible = false;
            this.CertaintyIndexVisible = false;
            this.TraceSelected = true;
            
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

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null) return;

            this.ChildlessExclude = iniFile.ReadBool("Chart", "ChildlessExclude", false);
            this.Decorative = iniFile.ReadBool("Chart", "Decorative", true);
            this.FamilyVisible = iniFile.ReadBool("Chart", "FamilyVisible", true);
            this.NameVisible = iniFile.ReadBool("Chart", "NameVisible", true);
            this.PatronymicVisible = iniFile.ReadBool("Chart", "PatronymicVisible", true);
            this.NickVisible = iniFile.ReadBool("Chart", "NickVisible", true);
            this.DiffLines = iniFile.ReadBool("Chart", "DiffLines", false);
            this.BirthDateVisible = iniFile.ReadBool("Chart", "BirthDateVisible", false);
            this.DeathDateVisible = iniFile.ReadBool("Chart", "DeathDateVisible", false);
            this.OnlyYears = iniFile.ReadBool("Chart", "OnlyYears", false);
            this.Kinship = iniFile.ReadBool("Chart", "Kinship", false);
            this.SignsVisible = iniFile.ReadBool("Chart", "SignsVisible", false);
            this.PortraitsVisible = iniFile.ReadBool("Chart", "PortraitsVisible", true);
            this.CertaintyIndexVisible = iniFile.ReadBool("Chart", "CertaintyIndexVisible", false);
            this.TraceSelected = iniFile.ReadBool("Chart", "TraceSelected", true);
            
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
            if (iniFile == null) return;

            iniFile.WriteBool("Chart", "ChildlessExclude", this.ChildlessExclude);
            iniFile.WriteBool("Chart", "Decorative", this.Decorative);
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
            iniFile.WriteBool("Chart", "CertaintyIndexVisible", this.CertaintyIndexVisible);
            iniFile.WriteBool("Chart", "TraceSelected", this.TraceSelected);
            
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
