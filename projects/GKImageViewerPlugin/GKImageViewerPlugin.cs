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
using System.Reflection;
using System.Resources;
using System.Runtime.InteropServices;

using GKCommon;
using GKCore.Interfaces;

[assembly: AssemblyTitle("GKImageViewerPlugin")]
[assembly: AssemblyDescription("GEDKeeper ImageViewer plugin")]
[assembly: AssemblyConfiguration("")]
[assembly: AssemblyCompany("")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2015 by Sergey V. Zhdanovskih")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]
[assembly: CLSCompliant(true)]
[assembly: ComVisible(false)]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyFileVersion("1.0.0.0")]
[assembly: NeutralResourcesLanguage("en")]

namespace GKImageViewerPlugin
{
    public enum IVLS
    {
        LSID_ImgViewer,
        LSID_FilesFilter,
        LSID_FileLoad
    }

    public class Plugin : BaseObject, IPlugin
    {
        private string fDisplayName = "ImageViewer";
        private IHost fHost;
        private ILangMan fLangMan;

        public string DisplayName { get { return fDisplayName; } }
        public IHost Host { get { return fHost; } }
        public ILangMan LangMan { get { return fLangMan; } }

        internal ImageViewerWin fForm;

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fForm != null) fForm.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Execute()
        {
            try
            {
                fForm = new ImageViewerWin(this);
                fHost.ShowWindow(fForm);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKImageViewerPlugin.Execute(): " + ex.Message);
            }
        }

        public void OnHostClosing(ref bool cancelClosing) {}
        public void OnHostActivate() {}
        public void OnHostDeactivate() {}

        public void OnLanguageChange()
        {
            try
            {
                fLangMan = fHost.CreateLangMan(this);
                fDisplayName = fLangMan.LS(IVLS.LSID_ImgViewer);

                if (fForm != null) fForm.SetLang();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKImageViewerPlugin.OnLanguageChange(): " + ex.Message);
            }
        }

        public bool Startup(IHost host)
        {
            bool result = true;
            try
            {
                fHost = host;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKImageViewerPlugin.Startup(): " + ex.Message);
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
                Logger.LogWrite("GKImageViewerPlugin.Shutdown(): " + ex.Message);
                result = false;
            }
            return result;
        }
    }
}
