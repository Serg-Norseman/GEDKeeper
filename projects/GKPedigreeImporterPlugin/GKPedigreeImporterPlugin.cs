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
using System.Reflection;
using System.Resources;
using System.Runtime.InteropServices;

using GKCore.Interfaces;

[assembly: AssemblyTitle("GKPedigreeImporterPlugin")]
[assembly: AssemblyDescription("GEDKeeper2 PedigreeImporter plugin")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper2")]
[assembly: AssemblyCopyright("Copyright © 2014,2016, Serg V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(false)]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.2.0.0")]
[assembly: AssemblyFileVersion("1.2.0.0")]
[assembly: NeutralResourcesLanguage("en")]

namespace GKPedigreeImporterPlugin
{
    public enum ILS
    {
        LSID_PluginTitle,
        LSID_File, /* 0 */
        LSID_DlgSelect, /* 100 */
        LSID_FormatUnsupported, /* 396 */
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
        LSID_Analysis,
        LSID_Loading,
        LSID_NPS,
        LSID_SNP,
        LSID_NoSpecial,
        LSID_AllFiltersW,
        LSID_AllFiltersL,
        LSID_SpousesInfoConflict
    }
    
    public class PlugIn : IPlugin
    {
        private string fDisplayName = "GKPedigreeImporterPlugin";
        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return this.fDisplayName; } }
        public IHost Host { get { return this.fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        public void Execute()
        {
            IBaseWindow curBase = fHost.GetCurrentFile();
            if (curBase == null) return;

            PedigreeImporterDlg frm = new PedigreeImporterDlg(this, curBase);
            frm.ShowDialog();
        }

        public void OnHostClosing(ref bool cancelClosing) {}
        public void OnHostActivate() {}
        public void OnHostDeactivate() {}

        public void OnLanguageChange()
        {
            try
            {
                this.fLangMan = this.fHost.CreateLangMan(this);
                this.fDisplayName = this.fLangMan.LS(ILS.LSID_PluginTitle);
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKPedigreeImporterPlugin.OnLanguageChange(): " + ex.Message);
            }
        }

        public bool Startup(IHost host)
        {
            bool result = true;
            try
            {
                this.fHost = host;
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKPedigreeImporterPlugin.Startup(): " + ex.Message);
                result = false;
            }
            return result;
        }

        public bool Shutdown()
        {
            bool result = true;
            try
            {
            }
            catch (Exception ex)
            {
                fHost.LogWrite("GKPedigreeImporterPlugin.Shutdown(): " + ex.Message);
                result = false;
            }
            return result;
        }
    }
}
