/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2022-2025 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.Reflection;
using GDModel;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Plugins;
using GKGenetix.Core;
using GKGenetix.UI.Forms;

[assembly: AssemblyTitle("GKGenetixPlugin")]
[assembly: AssemblyDescription("GEDKeeper Genetix plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright(GeneLab.APP_COPYRIGHT)]
[assembly: AssemblyVersion(GeneLab.APP_VERSION)]
[assembly: AssemblyCulture("")]

namespace GKGenetixPlugin
{
    public enum CLS
    {
        Title = 1,
        DNAAnalysis,
        DNAInheritanceTest,
    }

    public class DNAAnalysisPlugin : OrdinaryPlugin
    {
        private string fDisplayName = "DNAAnalysisPlugin";
        private ILangMan fLangMan;
        private IImage fIcon;

        public override IImage Icon { get { return fIcon; } }
        public override PluginCategory Category { get { return PluginCategory.Common; } }
        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }

        private GKMainFrm fForm;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                CloseForm();
            }
            base.Dispose(disposing);
        }

        internal void CloseForm()
        {
            if (fForm != null) {
                fForm.Close();
                fForm = null;
            }
        }

        public override void Execute()
        {
            if (fForm == null) {
                fForm = new GKMainFrm();
                fForm.SetAppDataPath(Host.GetAppDataPath());
                fForm.SetTestProvider(new PTestProvider(Host));
                fForm.Show();
            } else {
                CloseForm();
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(CLS.DNAAnalysis);

                //if (fForm != null) fForm.SetLocale();
            } catch (Exception ex) {
                Logger.WriteError("GKGenetixPlugin.OnLanguageChange()", ex);
            }
        }

        public override bool Startup(IHost host)
        {
            bool result = base.Startup(host);
            try {
                fIcon = AppHost.GfxProvider.LoadResourceImage(this.GetType(), "GKGenetixPlugin.Resources.GKGenetix.png", ImageTarget.UI);
            } catch (Exception ex) {
                Logger.WriteError("GKGenetixPlugin.Startup()", ex);
                result = false;
            }
            return result;
        }

        public override bool Shutdown()
        {
            bool result = true;
            try {
                CloseForm();
            } catch (Exception ex) {
                Logger.WriteError("GKGenetixPlugin.Shutdown()", ex);
                result = false;
            }
            return result;
        }


        private class PTestProvider : ITestProvider
        {
            private readonly IHost fHost;

            public PTestProvider(IHost host)
            {
                fHost = host;
            }

            public IList<DNATestInfo> RequestTests()
            {
                var result = new List<DNATestInfo>();

                var baseWindow = fHost.GetCurrentFile();
                if (baseWindow != null) {
                    var tree = baseWindow.Context.Tree;

                    for (int i = 0; i < tree.RecordsCount; i++) {
                        if (tree[i] is GDMIndividualRecord indiRec && indiRec.HasDNATests) {
                            for (int j = 0; j < indiRec.DNATests.Count; j++) {
                                var dnaTest = indiRec.DNATests[j];

                                string strDate = dnaTest.Date.GetDisplayString(DateFormat.dfDD_MM_YYYY);
                                string strSex = indiRec.Sex.ToString();
                                string fileRefPath = baseWindow.Context.MediaLoad(dnaTest.FileReference);

                                result.Add(new DNATestInfo() {
                                    Name = dnaTest.TestName,
                                    Date = strDate,
                                    Sex = strSex[2],
                                    FileReference = fileRefPath
                                });
                            }
                        }
                    }
                }

                return result;
            }
        }
    }
}
