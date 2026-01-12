/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using BSLib;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Media;
using GKCore.Plugins;
using GKCore.Utilities;

[assembly: AssemblyTitle("GKVisionPlugin")]
[assembly: AssemblyDescription("GEDKeeper ComputerVision plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2025 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("0.1.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKVisionPlugin
{
    public enum PLS
    {
        Title = 1,
    }

    public sealed class Plugin : OrdinaryPlugin
    {
        private string fDisplayName = "GKVisionPlugin";
        private ILangMan fLangMan;


        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Background; } }


        public Plugin()
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
            }
            base.Dispose(disposing);
        }

        public override void Execute()
        {
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.Title);
            } catch (Exception ex) {
                Logger.WriteError("GKVisionPlugin.OnLanguageChange()", ex);
            }
        }

        public override bool Startup(IHost host)
        {
            bool result = base.Startup(host);

            try {
                AppHost.Container.Register<IComputerVision, GKComputerVision>(LifeCycle.Singleton);
                var cvImpl = AppHost.Container.TryResolve<IComputerVision>();

                cvImpl.Restore();

                return result;
            } catch (Exception ex) {
                Logger.WriteError("GKVisionPlugin.Startup()", ex);
                return false;
            }
        }

        public override bool Shutdown()
        {
            bool result = true;
            try {
                var cvImpl = AppHost.Container.TryResolve<IComputerVision>();

                cvImpl.Save();
            } catch (Exception ex) {
                Logger.WriteError("GKVisionPlugin.Shutdown()", ex);
                result = false;
            }
            return result;
        }

        public override void LoadOptions(IniFile ini)
        {
            //fFolder = ini.ReadString("GKVisionPlugin", "Folder", string.Empty);
        }

        public override void SaveOptions(IniFile ini)
        {
            //ini.WriteString("GKVisionPlugin", "Folder", fFolder);
        }
    }
}
