/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
