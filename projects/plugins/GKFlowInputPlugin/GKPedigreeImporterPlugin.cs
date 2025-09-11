/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Plugins;

namespace GKPedigreeImporterPlugin
{
    public enum PLS
    {
        PedigreeImporter = 42,
        File,
        DlgSelect,
        FormatUnsupported,
        DataLoadError,
        ParseError_LineSeq,
        PersonParsed,
        Generation,
        ParseError_AncNotFound,
        ParseError_DateInvalid,
        ParseError_NumDuplicate,
        PersonIdFormat,
        NumsDAboville,
        NumsKonovalov,
        NumsUnknown,
        TextPedigreesParams,
        PersonLineSeparator,
        SurnameFormat,
        GenerationFormat,
        DateSeparator,
        DateFormat,
        ConversionParams,
        SurnamesNormalize,
        Next,
        Back,
        Close,
        PersonLineSpecials,
        Special_1,
        Analyzing,
        Loading,
        NPS,
        SNP,
        NoSpecial,
        AllFiltersW,
        AllFiltersL,
        SpousesInfoConflict
    }

    public class PlugIn : OrdinaryPlugin
    {
        private string fDisplayName = "GKPedigreeImporterPlugin";
        private ILangMan fLangMan;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Tool; } }

        public override void Execute()
        {
            IBaseWindow curBase = Host.GetCurrentFile();
            if (curBase == null) return;

            PedigreeImporterDlg frm = new PedigreeImporterDlg(this, curBase);
#if !NETCOREAPP
            frm.ShowDialog();
#else
            frm.Show();
#endif
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.PedigreeImporter);
            } catch (Exception ex) {
                Logger.WriteError("GKPedigreeImporterPlugin.OnLanguageChange()", ex);
            }
        }
    }
}
