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
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Plugins;
using GKMCPPlugin.Features;
using GKMCPPlugin.Utilities;
using ZLMKit.MCP;

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

public sealed class Plugin : WidgetPlugin
{
    private string fDisplayName = "GKMCPPlugin";
    private ILangMan fLangMan;
    private MCPServer fMCPServer;
    private RuntimeContext fRuntimeContext;


    public override string DisplayName { get { return fDisplayName; } }
    public override ILangMan LangMan { get { return fLangMan; } }
    public override IImage Icon { get { return null; } }
    public override PluginCategory Category { get { return PluginCategory.Common; } }


    public Plugin()
    {
    }

    public override void BaseChanged(IBaseWindow baseWin)
    {
        fRuntimeContext.BaseContext = baseWin.Context;
    }

    public override void BaseClosed(IBaseWindow baseWin)
    {
        fRuntimeContext.BaseContext = null;
    }

    public override void Execute()
    {
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
            RuntimeContext.Initialize();

            fMCPServer = new MCPServer();
            fRuntimeContext = new RuntimeContext(fMCPServer);
            fMCPServer.Context = fRuntimeContext;

            InitFeatures(fMCPServer, embedded: true, pureMode: false, tdeMode: true, ragMode: true);

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
            StopAsync();
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

    public static void InitFeatures(MCPServer mcpServer, bool embedded, bool pureMode, bool tdeMode, bool ragMode)
    {
        MCPToolDiscovery.SetQueryEqualizer(new QueryEqualizer());

        mcpServer.InitFeatures(tdeMode, ragMode);

        // Files operations
        if (!embedded) {
            mcpServer.RegisterTool(new FileNewTool());
            mcpServer.RegisterTool(new FileLoadTool());
            mcpServer.RegisterTool(new FileSaveTool());
            mcpServer.RegisterTool(new FileRecentTool());
            mcpServer.RegisterTool(new FileReloadTool());
            mcpServer.RegisterTool(new FileSearchTool());
        }
        mcpServer.RegisterTool(new FilePropsTool());
        mcpServer.RegisterTool(new FileValidateTool());
        mcpServer.RegisterTool(new FileMergeTool());

        // Records operations
        mcpServer.RegisterTool(new RecordListTool());
        mcpServer.RegisterTool(new RecordInfoTool());
        mcpServer.RegisterTool(new RecordDeleteTool());
        mcpServer.RegisterTool(new RecordSearchTool());
        mcpServer.RegisterTool(new RecordSetRestrictionTool());
        mcpServer.RegisterTool(new RecordMergeTool());

        mcpServer.RegisterTool(new RecordAddUserRefTool());
        mcpServer.RegisterTool(new RecordDeleteUserRefTool());
        mcpServer.RegisterTool(new RecordListUserRefsTool());

        mcpServer.RegisterTool(new RecordAddSourceCitationTool());
        mcpServer.RegisterTool(new RecordDeleteSourceCitationTool());
        mcpServer.RegisterTool(new RecordListSourceCitationsTool());

        mcpServer.RegisterTool(new RecordAddMultimediaLinkTool());
        mcpServer.RegisterTool(new RecordDeleteMultimediaLinkTool());
        mcpServer.RegisterTool(new RecordListMultimediaTool());

        mcpServer.RegisterTool(new RecordAddNoteTool());
        mcpServer.RegisterTool(new RecordDeleteNoteTool());
        mcpServer.RegisterTool(new RecordListNotesTool());

        // Events
        mcpServer.RegisterTool(new EventTypeListTool());
        mcpServer.RegisterTool(new GEDCOMDateSpecTool());
        mcpServer.RegisterResource(new GEDCOMDateSpecResource());

        // Individuals operations
        mcpServer.RegisterTool(new IndiSearchTool());
        mcpServer.RegisterTool(new IndividualUpsertTool());

        mcpServer.RegisterTool(new IndiListSpousesTool()); // editing with family tools

        mcpServer.RegisterTool(new IndiListAssociationsTool());
        mcpServer.RegisterTool(new IndiUpsertAssociationTool());
        mcpServer.RegisterTool(new IndiDeleteAssociationTool());

        mcpServer.RegisterTool(new IndiListEventsTool());
        mcpServer.RegisterTool(new IndiUpsertEventTool());
        mcpServer.RegisterTool(new IndiDeleteEventTool());

        mcpServer.RegisterTool(new IndiListPersonalNamesTool());
        mcpServer.RegisterTool(new IndiUpsertPersonalNameTool());
        mcpServer.RegisterTool(new IndiDeletePersonalNameTool());

        // Families operations
        mcpServer.RegisterTool(new FamilyUpsertTool());

        mcpServer.RegisterTool(new FamAddChildTool());
        mcpServer.RegisterTool(new FamDeleteChildTool());
        mcpServer.RegisterTool(new FamListChildrenTool());

        mcpServer.RegisterTool(new FamListEventsTool());
        mcpServer.RegisterTool(new FamUpsertEventTool());
        mcpServer.RegisterTool(new FamDeleteEventTool());

        // Notes operations
        mcpServer.RegisterTool(new NoteUpsertTool());

        // Multimedia operations
        mcpServer.RegisterTool(new MediaUpsertTool());
        mcpServer.RegisterTool(new MediaGetTool());

        mcpServer.RegisterTool(new MediaListFilesTool());
        mcpServer.RegisterTool(new MediaUpsertFileTool());
        mcpServer.RegisterTool(new MediaDeleteFileTool());

        // Sources operations
        mcpServer.RegisterTool(new SourceUpsertTool());

        mcpServer.RegisterTool(new SourceListRepositoriesTool());
        mcpServer.RegisterTool(new SourceAddRepositoryTool());
        mcpServer.RegisterTool(new SourceDeleteRepositoryTool());

        // Repositories operations
        mcpServer.RegisterTool(new RepositoryUpsertTool());

        if (!pureMode) {
            mcpServer.RegisterTool(new IndiListGroupsTool()); // editing with group tools

            // Groups operations
            mcpServer.RegisterTool(new GroupUpsertTool());

            mcpServer.RegisterTool(new GroupListMembersTool());
            mcpServer.RegisterTool(new GroupAddMemberTool());
            mcpServer.RegisterTool(new GroupDeleteMemberTool());

            // Tasks operations
            mcpServer.RegisterTool(new TaskUpsertTool());

            // Researches operations
            mcpServer.RegisterTool(new ResearchUpsertTool());

            mcpServer.RegisterTool(new ResearchListTasksTool());
            mcpServer.RegisterTool(new ResearchAddTaskTool());
            mcpServer.RegisterTool(new ResearchDeleteTaskTool());

            mcpServer.RegisterTool(new ResearchListCommunicationsTool());
            mcpServer.RegisterTool(new ResearchAddCommunicationTool());
            mcpServer.RegisterTool(new ResearchDeleteCommunicationTool());

            mcpServer.RegisterTool(new ResearchListGroupsTool());
            mcpServer.RegisterTool(new ResearchAddGroupTool());
            mcpServer.RegisterTool(new ResearchDeleteGroupTool());

            // Communications operations
            mcpServer.RegisterTool(new CommunicationUpsertTool());

            // Locations operations
            mcpServer.RegisterTool(new LocationUpsertTool());

            mcpServer.RegisterTool(new LocationListNamesTool());
            mcpServer.RegisterTool(new LocationUpsertNameTool());
            mcpServer.RegisterTool(new LocationDeleteNameTool());

            mcpServer.RegisterTool(new LocationListTopLinksTool());
            mcpServer.RegisterTool(new LocationUpsertTopLinkTool());
            mcpServer.RegisterTool(new LocationDeleteTopLinkTool());
        }

        // Pedigree operations
        mcpServer.RegisterTool(new PedigreeTraverseTool());

        // Tools
        mcpServer.RegisterTool(new TreeCompareTool());
        mcpServer.RegisterTool(new TreeSplitTool());
        mcpServer.RegisterTool(new FamilyGroupsTool());
        mcpServer.RegisterTool(new TreeCheckTool());
        mcpServer.RegisterTool(new PatSearchTool());
        mcpServer.RegisterTool(new PlacesManagerTool());
    }

    #endregion
}
