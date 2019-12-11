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
using GKCore;
using GKCore.Interfaces;
using GKCore.Plugins;

[assembly: AssemblyTitle("GKImageViewerPlugin")]
[assembly: AssemblyDescription("GEDKeeper ImageViewer plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2015 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

namespace GKImageViewerPlugin
{
    public enum PLS
    {
        LSID_ImgViewer,
        LSID_FilesFilter,
        LSID_FileLoad
    }

    public class Plugin : OrdinaryPlugin
    {
        private string fDisplayName = "ImageViewer";
        private ILangMan fLangMan;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Tool; } }

        internal ImageViewerWin fForm;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fForm != null)
                    fForm.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void Execute()
        {
            try {
                fForm = new ImageViewerWin(this);
                Host.ShowWindow(fForm);
            } catch (Exception ex) {
                Logger.LogException(ex);
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.LSID_ImgViewer);

                if (fForm != null)
                    fForm.SetLang();
            } catch (Exception ex) {
                Logger.LogException(ex);
            }
        }
    }
}
