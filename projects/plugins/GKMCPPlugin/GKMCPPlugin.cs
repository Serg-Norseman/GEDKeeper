/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using System.Threading.Tasks;
using BSLib;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Plugins;
using GKCortex.MCP;
using GKMCPPlugin.Features;
using GKMCPPlugin.Utilities;

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
    ServerHost = 4,
    ServerPort = 5,
    TrustedHosts = 6,
    CORS = 7,
    VerboseServerLogs = 8,
    AutoStart = 9,
    ServerStopped = 10,
    ServerStarted = 11,
    Error = 12,
    MCPSrvConfig = 13,
    ValidPortRequired = 14,
    ValidationError = 15,
    StartingServer = 16,
    ErrorStartingServer = 17,
    StoppingServer = 18,
    MCPServerSettings = 19,
    HostToolTip = 20,
    PortToolTip = 21,
    AllowedHostsTip = 22,
    StartingError = 23,
    StoppingError = 24,
}

public sealed class Plugin : OrdinaryPlugin
{
    private string fDisplayName = "GKMCPPlugin";
    private ILangMan fLangMan;
    private MCPServer fMCPServer;


    public override string DisplayName { get { return fDisplayName; } }
    public override ILangMan LangMan { get { return fLangMan; } }
    public override IImage Icon { get { return null; } }
    public override PluginCategory Category { get { return PluginCategory.Common; } }


    public Plugin()
    {
        InitFeatures(embedded: true, pureMode: false, tdeMode: true, ragMode: true);
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

        using (var dlg = new MCPServerForm(this, fLangMan)) {
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

    internal bool IsRunning()
    {
        return fMCPServer.IsRunning;
    }

    internal async Task StartAsync()
    {
        await fMCPServer.StartAsync(ServerHost, ServerPort, EnableCors, AllowedHosts, VerboseLogging);
    }

    internal async Task StopAsync()
    {
        await fMCPServer.StopAsync();
    }

    #region Options

    internal bool AutoStart = false;
    internal string ServerHost = "localhost";
    internal int ServerPort = 8080;
    internal bool EnableCors = false;
    internal string AllowedHosts = "http://localhost:3000";
    internal bool VerboseLogging = false;

    public override void LoadOptions(IniFile ini)
    {
        AutoStart = ini.ReadBool("GKMCPPlugin", "AutoStart", false);
        if (AutoStart) {
            StartAsync();
        }
    }

    public override void SaveOptions(IniFile ini)
    {
        ini.WriteBool("GKMCPPlugin", "AutoStart", AutoStart);
    }

    #endregion

    #region MCP Features

    public static void InitFeatures(bool embedded, bool pureMode, bool tdeMode, bool ragMode)
    {
        foreach (var keyTok in RuntimeData.RecordTypeMap.Keys)
            MCPToolDiscovery.EnhancementTokens.Add(keyTok);

        MCPController.InitFeatures(tdeMode, ragMode);

        // Files operations
        if (!embedded) {
            MCPController.RegisterTool(new FileNewTool());
            MCPController.RegisterTool(new FileLoadTool());
            MCPController.RegisterTool(new FileSaveTool());
            MCPController.RegisterTool(new FileRecentTool());
            MCPController.RegisterTool(new FileReloadTool());
            MCPController.RegisterTool(new FileSearchTool());
        }
        MCPController.RegisterTool(new FilePropsTool());
        MCPController.RegisterTool(new FileValidateTool());
        MCPController.RegisterTool(new FileMergeTool());

        // Records operations
        MCPController.RegisterTool(new RecordListTool());
        MCPController.RegisterTool(new RecordInfoTool());
        MCPController.RegisterTool(new RecordDeleteTool());
        MCPController.RegisterTool(new RecordSearchTool());
        MCPController.RegisterTool(new RecordSetRestrictionTool());
        MCPController.RegisterTool(new RecordMergeTool());

        MCPController.RegisterTool(new RecordAddUserRefTool());
        MCPController.RegisterTool(new RecordDeleteUserRefTool());
        MCPController.RegisterTool(new RecordListUserRefsTool());

        MCPController.RegisterTool(new RecordAddSourceCitationTool());
        MCPController.RegisterTool(new RecordDeleteSourceCitationTool());
        MCPController.RegisterTool(new RecordListSourceCitationsTool());

        MCPController.RegisterTool(new RecordAddMultimediaLinkTool());
        MCPController.RegisterTool(new RecordDeleteMultimediaLinkTool());
        MCPController.RegisterTool(new RecordListMultimediaTool());

        MCPController.RegisterTool(new RecordAddNoteTool());
        MCPController.RegisterTool(new RecordDeleteNoteTool());
        MCPController.RegisterTool(new RecordListNotesTool());

        // Events
        MCPController.RegisterTool(new EventTypeListTool());
        MCPController.RegisterTool(new GEDCOMDateSpecTool());
        MCPController.RegisterResource(new GEDCOMDateSpecResource());

        // Individuals operations
        MCPController.RegisterTool(new IndiSearchTool());
        MCPController.RegisterTool(new IndividualUpsertTool());

        MCPController.RegisterTool(new IndiListSpousesTool()); // editing with family tools

        MCPController.RegisterTool(new IndiListAssociationsTool());
        MCPController.RegisterTool(new IndiUpsertAssociationTool());
        MCPController.RegisterTool(new IndiDeleteAssociationTool());

        MCPController.RegisterTool(new IndiListEventsTool());
        MCPController.RegisterTool(new IndiUpsertEventTool());
        MCPController.RegisterTool(new IndiDeleteEventTool());

        MCPController.RegisterTool(new IndiListPersonalNamesTool());
        MCPController.RegisterTool(new IndiUpsertPersonalNameTool());
        MCPController.RegisterTool(new IndiDeletePersonalNameTool());

        // Families operations
        MCPController.RegisterTool(new FamilyUpsertTool());

        MCPController.RegisterTool(new FamAddChildTool());
        MCPController.RegisterTool(new FamDeleteChildTool());
        MCPController.RegisterTool(new FamListChildrenTool());

        MCPController.RegisterTool(new FamListEventsTool());
        MCPController.RegisterTool(new FamUpsertEventTool());
        MCPController.RegisterTool(new FamDeleteEventTool());

        // Notes operations
        MCPController.RegisterTool(new NoteUpsertTool());

        // Multimedia operations
        MCPController.RegisterTool(new MediaUpsertTool());
        MCPController.RegisterTool(new MediaGetTool());

        MCPController.RegisterTool(new MediaListFilesTool());
        MCPController.RegisterTool(new MediaUpsertFileTool());
        MCPController.RegisterTool(new MediaDeleteFileTool());

        // Sources operations
        MCPController.RegisterTool(new SourceUpsertTool());

        MCPController.RegisterTool(new SourceListRepositoriesTool());
        MCPController.RegisterTool(new SourceAddRepositoryTool());
        MCPController.RegisterTool(new SourceDeleteRepositoryTool());

        // Repositories operations
        MCPController.RegisterTool(new RepositoryUpsertTool());

        if (!pureMode) {
            MCPController.RegisterTool(new IndiListGroupsTool()); // editing with group tools

            // Groups operations
            MCPController.RegisterTool(new GroupUpsertTool());

            MCPController.RegisterTool(new GroupListMembersTool());
            MCPController.RegisterTool(new GroupAddMemberTool());
            MCPController.RegisterTool(new GroupDeleteMemberTool());

            // Tasks operations
            MCPController.RegisterTool(new TaskUpsertTool());

            // Researches operations
            MCPController.RegisterTool(new ResearchUpsertTool());

            MCPController.RegisterTool(new ResearchListTasksTool());
            MCPController.RegisterTool(new ResearchAddTaskTool());
            MCPController.RegisterTool(new ResearchDeleteTaskTool());

            MCPController.RegisterTool(new ResearchListCommunicationsTool());
            MCPController.RegisterTool(new ResearchAddCommunicationTool());
            MCPController.RegisterTool(new ResearchDeleteCommunicationTool());

            MCPController.RegisterTool(new ResearchListGroupsTool());
            MCPController.RegisterTool(new ResearchAddGroupTool());
            MCPController.RegisterTool(new ResearchDeleteGroupTool());

            // Communications operations
            MCPController.RegisterTool(new CommunicationUpsertTool());

            // Locations operations
            MCPController.RegisterTool(new LocationUpsertTool());

            MCPController.RegisterTool(new LocationListNamesTool());
            MCPController.RegisterTool(new LocationUpsertNameTool());
            MCPController.RegisterTool(new LocationDeleteNameTool());

            MCPController.RegisterTool(new LocationListTopLinksTool());
            MCPController.RegisterTool(new LocationUpsertTopLinkTool());
            MCPController.RegisterTool(new LocationDeleteTopLinkTool());
        }

        // Pedigree operations
        MCPController.RegisterTool(new PedigreeTraverseTool());

        // Tools
        MCPController.RegisterTool(new TreeCompareTool());
        MCPController.RegisterTool(new TreeSplitTool());
        MCPController.RegisterTool(new FamilyGroupsTool());
        MCPController.RegisterTool(new TreeCheckTool());
        MCPController.RegisterTool(new PatSearchTool());
        MCPController.RegisterTool(new PlacesManagerTool());
    }

    #endregion
}
