/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib.Design;
using BSLib.Design.Graphics;
using GKCore.Charts;
using GKCore.Interfaces;

using BSDColors = BSLib.Design.BSDConsts.Colors;

namespace GKCore.Options
{
    public enum DeepMode
    {
        None, Background, Foreground
    }

    /// <summary>
    ///
    /// </summary>
    public sealed class TreeChartOptions : IOptions
    {
        public static readonly int MALE_COLOR = -3750145; // FFC6C6FF
        public static readonly int FEMALE_COLOR = -14650; // FFFFC6C6
        public static readonly int UNK_SEX_COLOR = -14593; // FFFFC6FF
        public static readonly int UN_HUSBAND_COLOR = -2631681; // FFD7D7FF
        public static readonly int UN_WIFE_COLOR = -10281; // FFFFD7D7

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
        public bool InvertedTree;
        public bool MarriagesDates;
        public bool ShowPlaces;
        public bool HideUnknownSpouses;
        public bool DottedLinesOfAdoptedChildren;
        public bool SeparateDatesAndPlacesLines;
        public bool BoldNames;
        public bool OnlyLocality;
        public bool MinimizingWidth;
        //public bool RootSpousesAncestors;
        public bool AgeVisible;

        public bool AutoAlign; // debug option, for future purposes
        public GfxBorderStyle BorderStyle;
        public DeepMode DeepMode;

        public IColor MaleColor;
        public IColor FemaleColor;
        public IColor UnkSexColor;
        public IColor UnHusbandColor;
        public IColor UnWifeColor;

        public string DefFontName;
        public int DefFontSize;
        public IColor DefFontColor;
        public BSDTypes.FontStyle DefFontStyle;

        public int BranchDistance;
        public int LevelDistance;
        public int Margins;
        public int SpouseDistance;

        public bool SeparateDepth { get; set; }
        public int DepthLimit { get; set; }
        public int DepthLimitAncestors { get; set; }
        public int DepthLimitDescendants { get; set; }

        public TreeChartOptions()
        {
            FamilyVisible = true;
            NameVisible = true;
            PatronymicVisible = true;
            NickVisible = true;
            DiffLines = true;

            BirthDateVisible = true;
            DeathDateVisible = true;
            OnlyYears = true;

            Kinship = false;
            PortraitsVisible = true;
            DefaultPortraits = false;
            SignsVisible = false;
            CertaintyIndexVisible = false;
            TraceSelected = true;
            ChildlessExclude = false;
            Decorative = true;
            InvertedTree = false;
            MarriagesDates = false;
            ShowPlaces = false;
            HideUnknownSpouses = false;
            DottedLinesOfAdoptedChildren = false;
            SeparateDatesAndPlacesLines = false;
            BoldNames = false;
            SeparateDepth = false;
            OnlyLocality = false;
            MinimizingWidth = true;
            //RootSpousesAncestors = true;
            AgeVisible = false;

            AutoAlign = true;
            BorderStyle = GfxBorderStyle.None;
            DeepMode = DeepMode.None;

            MaleColor = ChartRenderer.GetColor(MALE_COLOR);
            FemaleColor = ChartRenderer.GetColor(FEMALE_COLOR);
            UnkSexColor = ChartRenderer.GetColor(UNK_SEX_COLOR);
            UnHusbandColor = ChartRenderer.GetColor(UN_HUSBAND_COLOR);
            UnWifeColor = ChartRenderer.GetColor(UN_WIFE_COLOR);

            DefFontName = AppHost.GfxProvider.GetDefaultFontName();
            DefFontSize = 8;
            DefFontColor = ChartRenderer.GetColor(BSDColors.Black);
            DefFontStyle = BSDTypes.FontStyle.None;

            BranchDistance = TreeChartModel.DEF_BRANCH_DISTANCE;
            LevelDistance = TreeChartModel.DEF_LEVEL_DISTANCE;
            Margins = TreeChartModel.DEF_MARGINS;
            SpouseDistance = TreeChartModel.DEF_SPOUSE_DISTANCE;
        }

        public void Assign(IOptions source)
        {
            TreeChartOptions srcOptions = source as TreeChartOptions;
            if (srcOptions == null) return;

            FamilyVisible = srcOptions.FamilyVisible;
            NameVisible = srcOptions.NameVisible;
            PatronymicVisible = srcOptions.PatronymicVisible;
            NickVisible = srcOptions.NickVisible;
            DiffLines = srcOptions.DiffLines;
            BirthDateVisible = srcOptions.BirthDateVisible;
            DeathDateVisible = srcOptions.DeathDateVisible;
            OnlyYears = srcOptions.OnlyYears;
            Kinship = srcOptions.Kinship;
            PortraitsVisible = srcOptions.PortraitsVisible;
            DefaultPortraits = srcOptions.DefaultPortraits;
            SignsVisible = srcOptions.SignsVisible;
            CertaintyIndexVisible = srcOptions.CertaintyIndexVisible;
            TraceSelected = srcOptions.TraceSelected;
            ChildlessExclude = srcOptions.ChildlessExclude;
            Decorative = srcOptions.Decorative;
            MaleColor = srcOptions.MaleColor;
            FemaleColor = srcOptions.FemaleColor;
            UnkSexColor = srcOptions.UnkSexColor;
            UnHusbandColor = srcOptions.UnHusbandColor;
            UnWifeColor = srcOptions.UnWifeColor;
            DefFontName = srcOptions.DefFontName;
            DefFontSize = srcOptions.DefFontSize;
            DefFontColor = srcOptions.DefFontColor;
            DefFontStyle = srcOptions.DefFontStyle;
            InvertedTree = srcOptions.InvertedTree;
            MarriagesDates = srcOptions.MarriagesDates;
            ShowPlaces = srcOptions.ShowPlaces;
            HideUnknownSpouses = srcOptions.HideUnknownSpouses;
            DottedLinesOfAdoptedChildren = srcOptions.DottedLinesOfAdoptedChildren;
            SeparateDatesAndPlacesLines = srcOptions.SeparateDatesAndPlacesLines;
            BoldNames = srcOptions.BoldNames;
            SeparateDepth = srcOptions.SeparateDepth;
            BorderStyle = srcOptions.BorderStyle;
            OnlyLocality = srcOptions.OnlyLocality;
            MinimizingWidth = srcOptions.MinimizingWidth;
            AgeVisible = srcOptions.AgeVisible;

            BranchDistance = srcOptions.BranchDistance;
            LevelDistance = srcOptions.LevelDistance;
            Margins = srcOptions.Margins;
            SpouseDistance = srcOptions.SpouseDistance;
        }

        public void LoadFromFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            FamilyVisible = iniFile.ReadBool("Chart", "FamilyVisible", true);
            NameVisible = iniFile.ReadBool("Chart", "NameVisible", true);
            PatronymicVisible = iniFile.ReadBool("Chart", "PatronymicVisible", true);
            NickVisible = iniFile.ReadBool("Chart", "NickVisible", true);
            DiffLines = iniFile.ReadBool("Chart", "DiffLines", true);

            BirthDateVisible = iniFile.ReadBool("Chart", "BirthDateVisible", true);
            DeathDateVisible = iniFile.ReadBool("Chart", "DeathDateVisible", true);
            OnlyYears = iniFile.ReadBool("Chart", "OnlyYears", true);

            Kinship = iniFile.ReadBool("Chart", "Kinship", false);
            SignsVisible = iniFile.ReadBool("Chart", "SignsVisible", false);
            PortraitsVisible = iniFile.ReadBool("Chart", "PortraitsVisible", true);
            DefaultPortraits = iniFile.ReadBool("Chart", "DefaultPortraits", false);
            CertaintyIndexVisible = iniFile.ReadBool("Chart", "CertaintyIndexVisible", false);
            TraceSelected = iniFile.ReadBool("Chart", "TraceSelected", true);
            ChildlessExclude = iniFile.ReadBool("Chart", "ChildlessExclude", false);
            Decorative = iniFile.ReadBool("Chart", "Decorative", true);
            //DeepMode = (DeepMode)iniFile.ReadInteger("Chart", "DeepMode", 0);
            InvertedTree = iniFile.ReadBool("Chart", "InvertedTree", false);
            MarriagesDates = iniFile.ReadBool("Chart", "MarriagesDates", false);
            ShowPlaces = iniFile.ReadBool("Chart", "ShowPlaces", false);
            HideUnknownSpouses = iniFile.ReadBool("Chart", "HideUnknownSpouses", false);
            DottedLinesOfAdoptedChildren = iniFile.ReadBool("Chart", "DottedLinesOfAdoptedChildren", false);
            SeparateDatesAndPlacesLines = iniFile.ReadBool("Chart", "SeparateDatesAndPlacesLines", false);
            BoldNames = iniFile.ReadBool("Chart", "BoldNames", false);
            BorderStyle = (GfxBorderStyle)iniFile.ReadInteger("Chart", "BorderStyle", 0);
            OnlyLocality = iniFile.ReadBool("Chart", "OnlyLocality", false);
            MinimizingWidth = iniFile.ReadBool("Chart", "MinimizingWidth", true);
            AgeVisible = iniFile.ReadBool("Chart", "AgeVisible", false);

            MaleColor = ChartRenderer.GetColor(iniFile.ReadInteger("Chart", "MaleColor", MALE_COLOR));
            FemaleColor = ChartRenderer.GetColor(iniFile.ReadInteger("Chart", "FemaleColor", FEMALE_COLOR));
            UnkSexColor = ChartRenderer.GetColor(iniFile.ReadInteger("Chart", "UnkSexColor", UNK_SEX_COLOR));
            UnHusbandColor = ChartRenderer.GetColor(iniFile.ReadInteger("Chart", "UnHusbandColor", UN_HUSBAND_COLOR));
            UnWifeColor = ChartRenderer.GetColor(iniFile.ReadInteger("Chart", "UnWifeColor", UN_WIFE_COLOR));

            DefFontName = iniFile.ReadString("Chart", "FontName", AppHost.GfxProvider.GetDefaultFontName());
            DefFontSize = iniFile.ReadInteger("Chart", "FontSize", 8);
            DefFontColor = ChartRenderer.GetColor(iniFile.ReadInteger("Chart", "FontColor", BSDColors.Black));
            DefFontStyle = (BSDTypes.FontStyle)iniFile.ReadInteger("Chart", "FontStyle", 0);

            BranchDistance = iniFile.ReadInteger("Chart", "BranchDistance", TreeChartModel.DEF_BRANCH_DISTANCE);
            LevelDistance = iniFile.ReadInteger("Chart", "LevelDistance", TreeChartModel.DEF_LEVEL_DISTANCE);
            Margins = iniFile.ReadInteger("Chart", "Margins", TreeChartModel.DEF_MARGINS);
            SpouseDistance = iniFile.ReadInteger("Chart", "SpouseDistance", TreeChartModel.DEF_SPOUSE_DISTANCE);

            SeparateDepth = iniFile.ReadBool("Chart", "SeparateDepth", false);
            DepthLimit = iniFile.ReadInteger("Chart", "DepthLimit", -1);
            DepthLimitAncestors = iniFile.ReadInteger("Chart", "DepthLimitAncestors", -1);
            DepthLimitDescendants = iniFile.ReadInteger("Chart", "DepthLimitDescendants", -1);
        }

        public void SaveToFile(IniFile iniFile)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            iniFile.WriteBool("Chart", "FamilyVisible", FamilyVisible);
            iniFile.WriteBool("Chart", "NameVisible", NameVisible);
            iniFile.WriteBool("Chart", "PatronymicVisible", PatronymicVisible);
            iniFile.WriteBool("Chart", "NickVisible", NickVisible);
            iniFile.WriteBool("Chart", "DiffLines", DiffLines);

            iniFile.WriteBool("Chart", "BirthDateVisible", BirthDateVisible);
            iniFile.WriteBool("Chart", "DeathDateVisible", DeathDateVisible);
            iniFile.WriteBool("Chart", "OnlyYears", OnlyYears);

            iniFile.WriteBool("Chart", "Kinship", Kinship);
            iniFile.WriteBool("Chart", "SignsVisible", SignsVisible);
            iniFile.WriteBool("Chart", "PortraitsVisible", PortraitsVisible);
            iniFile.WriteBool("Chart", "DefaultPortraits", DefaultPortraits);
            iniFile.WriteBool("Chart", "CertaintyIndexVisible", CertaintyIndexVisible);
            iniFile.WriteBool("Chart", "TraceSelected", TraceSelected);
            iniFile.WriteBool("Chart", "ChildlessExclude", ChildlessExclude);
            iniFile.WriteBool("Chart", "Decorative", Decorative);
            //iniFile.WriteInteger("Chart", "DeepMode", (int)DeepMode);
            iniFile.WriteBool("Chart", "InvertedTree", InvertedTree);
            iniFile.WriteBool("Chart", "MarriagesDates", MarriagesDates);
            iniFile.WriteBool("Chart", "ShowPlaces", ShowPlaces);
            iniFile.WriteBool("Chart", "HideUnknownSpouses", HideUnknownSpouses);
            iniFile.WriteBool("Chart", "DottedLinesOfAdoptedChildren", DottedLinesOfAdoptedChildren);
            iniFile.WriteBool("Chart", "SeparateDatesAndPlacesLines", SeparateDatesAndPlacesLines);
            iniFile.WriteBool("Chart", "BoldNames", BoldNames);
            iniFile.WriteInteger("Chart", "BorderStyle", (int)BorderStyle);
            iniFile.WriteBool("Chart", "OnlyLocality", OnlyLocality);
            iniFile.WriteBool("Chart", "MinimizingWidth", MinimizingWidth);
            iniFile.WriteBool("Chart", "AgeVisible", AgeVisible);

            iniFile.WriteInteger("Chart", "MaleColor", MaleColor.ToArgb());
            iniFile.WriteInteger("Chart", "FemaleColor", FemaleColor.ToArgb());
            iniFile.WriteInteger("Chart", "UnkSexColor", UnkSexColor.ToArgb());
            iniFile.WriteInteger("Chart", "UnHusbandColor", UnHusbandColor.ToArgb());
            iniFile.WriteInteger("Chart", "UnWifeColor", UnWifeColor.ToArgb());

            iniFile.WriteString("Chart", "FontName", DefFontName);
            iniFile.WriteInteger("Chart", "FontSize", DefFontSize);
            iniFile.WriteInteger("Chart", "FontColor", DefFontColor.ToArgb());
            iniFile.WriteInteger("Chart", "FontStyle", (byte)DefFontStyle);

            iniFile.WriteInteger("Chart", "BranchDistance", BranchDistance);
            iniFile.WriteInteger("Chart", "LevelDistance", LevelDistance);
            iniFile.WriteInteger("Chart", "Margins", Margins);
            iniFile.WriteInteger("Chart", "SpouseDistance", SpouseDistance);

            iniFile.WriteBool("Chart", "SeparateDepth", SeparateDepth);
            iniFile.WriteInteger("Chart", "DepthLimit", DepthLimit);
            iniFile.WriteInteger("Chart", "DepthLimitAncestors", DepthLimitAncestors);
            iniFile.WriteInteger("Chart", "DepthLimitDescendants", DepthLimitDescendants);
        }
    }
}
