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
using GKCortex.MCP;
using GKCortex.Utilities;

[assembly: AssemblyTitle("GKMCPPlugin")]
[assembly: AssemblyDescription("GEDKeeper MCP Server plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2026 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion("0.1.0.0")]
[assembly: AssemblyCulture("")]

#if DEBUG
[assembly: AssemblyConfiguration("Debug")]
#elif RELEASE
[assembly: AssemblyConfiguration("Release")]
#endif

namespace GKMCPPlugin;

public enum PLS
{
    Title = 1,
    Start = 2,
    Stop = 3,
    ServerHost,
    ServerPort,
    TrustedHosts,
    CORS, // Cross-Origin Resource Sharing (CORS)
    VerboseServerLogs,
}

public sealed class Plugin : LMPlugin
{
    private string fDisplayName = "GKMCPPlugin";
    private ILangMan fLangMan;
    private MCPServer fMCPServer;
    private bool fAutoStart;


    public override string DisplayName { get { return fDisplayName; } }
    public override ILangMan LangMan { get { return fLangMan; } }
    public override IImage Icon { get { return null; } }
    public override PluginCategory Category { get { return PluginCategory.Common; } }


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
        var baseWin = Host.GetCurrentFile();
        MCPController.SetContext(baseWin.Context);

        using (var dlg = new MCPServerForm(fLangMan, fMCPServer)) {
            dlg.ShowModal();
        }
    }

    public override void OnLanguageChange()
    {
        try {
            fLangMan = Host.CreateLangMan(this);
            fDisplayName = fLangMan.LS(PLS.Title);
        } catch (Exception ex) {
            Logger.WriteError("GKMCPPlugin.OnLanguageChange()", ex);
        }
    }

    public override bool Startup(IHost host)
    {
        bool result = base.Startup(host);
        try {
            fMCPServer = new MCPServer();
            return result;
        } catch (Exception ex) {
            Logger.WriteError("GKMCPPlugin.Startup()", ex);
            return false;
        }
    }

    public override bool Shutdown()
    {
        bool result = true;
        try {
        } catch (Exception ex) {
            Logger.WriteError("GKMCPPlugin.Shutdown()", ex);
            result = false;
        }
        return result;
    }

    public override void LoadOptions(IniFile ini)
    {
        fAutoStart = ini.ReadBool("GKMCPPlugin", "AutoStart", false);
    }

    public override void SaveOptions(IniFile ini)
    {
        ini.WriteBool("GKMCPPlugin", "AutoStart", fAutoStart);
    }
}
