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
using GKCore.Plugins;
using GKCortex.Features;
using GKCortex.LMChat;

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
        Model = 2,
        Settings = 3,
        Send = 4,
        Temperature = 5,
        TopP = 6,
        PresencePenalty = 7,
        FrequencyPenalty = 8,
        MaxTokens = 9,
        StreamMode = 10,
        SystemPrompt = 11,
        NewSession = 12,
        Stop = 13,
        APIAddress = 14,
        APIKey = 15,
        Session = 16,
        RenameSession = 17,
    }

    public class Plugin : OrdinaryPlugin
    {
        private string fDisplayName = "LMChat";
        private ILangMan fLangMan;
        private LMSettings fLMSettings;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return null; } }
        public override PluginCategory Category { get { return PluginCategory.Common; } }

        public LMSettings LMSettings
        {
            get {
                if (fLMSettings == null) {
                    fLMSettings = new LMSettings();
                }
                return fLMSettings;
            }
        }

        public override bool Startup(IHost host)
        {
            var result = base.Startup(host);
            MCPController.InitFeatures(embedded: true, pureMode: false, tdeMode: true, ragMode: true);
            return result;
        }

        public override void Execute()
        {
            var baseWin = Host.GetCurrentFile();
            MCPController.SetContext(baseWin.Context);
            var frm = new LMChatForm(this);
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

        public override void LoadOptions(IniFile ini)
        {
            LMSettings.LoadOptions(ini, "GKLMChatPlugin");
        }

        public override void SaveOptions(IniFile ini)
        {
            LMSettings.SaveOptions(ini, "GKLMChatPlugin");
        }
    }
}
