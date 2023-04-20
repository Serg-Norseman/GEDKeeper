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
using System.Reflection;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Interfaces;
using GKCore.Plugins;

[assembly: AssemblyTitle("GKPedigreeImporterPlugin")]
[assembly: AssemblyDescription("GEDKeeper PedigreeImporter plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2014-2023 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.2.0.0")]
[assembly: AssemblyCulture("")]

namespace GKPedigreeImporterPlugin
{
    public enum ILS
    {
        LSID_PluginTitle,
        LSID_File,
        LSID_DlgSelect,
        LSID_FormatUnsupported,
        LSID_DataLoadError,
        LSID_ParseError_LineSeq,
        LSID_PersonParsed,
        LSID_Generation,
        LSID_ParseError_AncNotFound,
        LSID_ParseError_DateInvalid,
        LSID_ParseError_NumDuplicate,
        LSID_PersonIdFormat,
        LSID_NumsDAboville,
        LSID_NumsKonovalov,
        LSID_NumsUnknown,
        LSID_TextPedigreesParams,
        LSID_PersonLineSeparator,
        LSID_SurnameFormat,
        LSID_GenerationFormat,
        LSID_DateSeparator,
        LSID_DateFormat,
        LSID_ConversionParams,
        LSID_SurnamesNormalize,
        LSID_Next,
        LSID_Back,
        LSID_Close,
        LSID_PersonLineSpecials,
        LSID_Special_1,
        LSID_Analyzing,
        LSID_Loading,
        LSID_NPS,
        LSID_SNP,
        LSID_NoSpecial,
        LSID_AllFiltersW,
        LSID_AllFiltersL,
        LSID_SpousesInfoConflict
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
#if !ETO
            frm.ShowDialog();
#else
            frm.Show();
#endif
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(ILS.LSID_PluginTitle);
            } catch (Exception ex) {
                Logger.WriteError("GKPedigreeImporterPlugin.OnLanguageChange()", ex);
            }
        }
    }
}
