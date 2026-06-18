/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Plugins;
using GKCortex.Features;
using GKCortex.MCP;

[assembly: AssemblyTitle("GKLMChatPlugin")]
[assembly: AssemblyDescription("GEDKeeper LMChat plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2026 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("1.0.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKLMChatPlugin
{
    public enum PLS
    {
        Title = 1,
    }

    public class Plugin : OrdinaryPlugin
    {
        private string fDisplayName = "LMChat";
        private ILangMan fLangMan;
        private MCPServer fMCPServer;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Common; } }

        public override bool Startup(IHost host)
        {
            var result = base.Startup(host);
            MCPController.InitFeatures(embedded: true, pureMode: false, tdeMode: true, ragMode: true);
            fMCPServer = new MCPServer();
            return result;
        }

        public override void Execute()
        {
            var baseWin = Host.GetCurrentFile();
            MCPController.SetContext(baseWin.Context);
            var frm = new LMChatForm(fLangMan, fMCPServer);
            frm.Show();
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.Title);
            } catch (Exception ex) {
                Logger.WriteError("GKLMChatPlugin.OnLanguageChange()", ex);
            }
        }
    }
}
