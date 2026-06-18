/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Text.Json;
using GKCore;
using GKCortex.MCP;
using GKCortex.Memory;
using GKCortex.Protocols;
using GKCortex.RAG;

namespace GKCortex.Features;

public class MCPController
{
    private static BaseContext fBaseContext = new BaseContext(null);
    private static readonly Dictionary<string, BaseTool> fTools = new Dictionary<string, BaseTool>();
    private static readonly List<MCPTool> fMCPTools = new List<MCPTool>();
    private static readonly Dictionary<string, BaseResource> fResources = new Dictionary<string, BaseResource>();
    private static bool fTDE = false;

    static MCPController()
    {
    }

    public static void SetContext(BaseContext baseContext)
    {
        if (baseContext != null) {
            fBaseContext = baseContext;
        }
    }

    public static void InitFeatures(bool embedded, bool pureMode, bool tdeMode, bool ragMode)
    {
        fTDE = tdeMode;

        // Files operations
        if (!embedded) {
            RegisterTool(new FileNewTool());
            RegisterTool(new FileLoadTool());
            RegisterTool(new FileSaveTool());
            RegisterTool(new FileRecentTool());
            RegisterTool(new FileReloadTool());
            RegisterTool(new FileSearchTool());
        }
        RegisterTool(new FilePropsTool());
        RegisterTool(new FileValidateTool());
        RegisterTool(new FileMergeTool());

        // Records operations
        RegisterTool(new RecordListTool());
        RegisterTool(new RecordInfoTool());
        RegisterTool(new RecordDeleteTool());
        RegisterTool(new RecordSearchTool());
        RegisterTool(new RecordSetRestrictionTool());
        RegisterTool(new RecordMergeTool());

        RegisterTool(new RecordAddUserRefTool());
        RegisterTool(new RecordDeleteUserRefTool());
        RegisterTool(new RecordListUserRefsTool());

        RegisterTool(new RecordAddSourceCitationTool());
        RegisterTool(new RecordDeleteSourceCitationTool());
        RegisterTool(new RecordListSourceCitationsTool());

        RegisterTool(new RecordAddMultimediaLinkTool());
        RegisterTool(new RecordDeleteMultimediaLinkTool());
        RegisterTool(new RecordListMultimediaTool());

        RegisterTool(new RecordAddNoteTool());
        RegisterTool(new RecordDeleteNoteTool());
        RegisterTool(new RecordListNotesTool());

        // Events
        RegisterTool(new EventTypeListTool());
        RegisterTool(new GEDCOMDateSpecTool());
        RegisterResource(new GEDCOMDateSpecResource());

        // Individuals operations
        RegisterTool(new IndiSearchTool());
        RegisterTool(new IndividualUpsertTool());

        RegisterTool(new IndiListSpousesTool()); // editing with family tools

        RegisterTool(new IndiListAssociationsTool());
        RegisterTool(new IndiUpsertAssociationTool());
        RegisterTool(new IndiDeleteAssociationTool());

        RegisterTool(new IndiListEventsTool());
        RegisterTool(new IndiUpsertEventTool());
        RegisterTool(new IndiDeleteEventTool());

        RegisterTool(new IndiListPersonalNamesTool());
        RegisterTool(new IndiUpsertPersonalNameTool());
        RegisterTool(new IndiDeletePersonalNameTool());

        // Families operations
        RegisterTool(new FamilyUpsertTool());

        RegisterTool(new FamAddChildTool());
        RegisterTool(new FamDeleteChildTool());
        RegisterTool(new FamListChildrenTool());

        RegisterTool(new FamListEventsTool());
        RegisterTool(new FamUpsertEventTool());
        RegisterTool(new FamDeleteEventTool());

        // Notes operations
        RegisterTool(new NoteUpsertTool());

        // Multimedia operations
        RegisterTool(new MediaUpsertTool());
        RegisterTool(new MediaGetTool());

        RegisterTool(new MediaListFilesTool());
        RegisterTool(new MediaUpsertFileTool());
        RegisterTool(new MediaDeleteFileTool());

        // Sources operations
        RegisterTool(new SourceUpsertTool());

        RegisterTool(new SourceListRepositoriesTool());
        RegisterTool(new SourceAddRepositoryTool());
        RegisterTool(new SourceDeleteRepositoryTool());

        // Repositories operations
        RegisterTool(new RepositoryUpsertTool());

        if (!pureMode) {
            RegisterTool(new IndiListGroupsTool()); // editing with group tools

            // Groups operations
            RegisterTool(new GroupUpsertTool());

            RegisterTool(new GroupListMembersTool());
            RegisterTool(new GroupAddMemberTool());
            RegisterTool(new GroupDeleteMemberTool());

            // Tasks operations
            RegisterTool(new TaskUpsertTool());

            // Researches operations
            RegisterTool(new ResearchUpsertTool());

            RegisterTool(new ResearchListTasksTool());
            RegisterTool(new ResearchAddTaskTool());
            RegisterTool(new ResearchDeleteTaskTool());

            RegisterTool(new ResearchListCommunicationsTool());
            RegisterTool(new ResearchAddCommunicationTool());
            RegisterTool(new ResearchDeleteCommunicationTool());

            RegisterTool(new ResearchListGroupsTool());
            RegisterTool(new ResearchAddGroupTool());
            RegisterTool(new ResearchDeleteGroupTool());

            // Communications operations
            RegisterTool(new CommunicationUpsertTool());

            // Locations operations
            RegisterTool(new LocationUpsertTool());

            RegisterTool(new LocationListNamesTool());
            RegisterTool(new LocationUpsertNameTool());
            RegisterTool(new LocationDeleteNameTool());

            RegisterTool(new LocationListTopLinksTool());
            RegisterTool(new LocationUpsertTopLinkTool());
            RegisterTool(new LocationDeleteTopLinkTool());
        }

        // Pedigree operations
        RegisterTool(new PedigreeTraverseTool());

        // Tools
        RegisterTool(new TreeCompareTool());
        RegisterTool(new TreeSplitTool());
        RegisterTool(new FamilyGroupsTool());
        RegisterTool(new TreeCheckTool());
        RegisterTool(new PatSearchTool());
        RegisterTool(new PlacesManagerTool());

        if (tdeMode) {
            RegisterTool(new SearchTool(), true);
            RegisterTool(new UseTool(), true);
        }

        if (ragMode) {
            RegisterTool(new RAGSearchExamplesTool(), true);
            RegisterTool(new RAGWritePatternTool(), true);

            RegisterTool(new StoreFactTool(), true);
            RegisterTool(new SearchMemoryTool(), true);

            RegisterTool(new GetContextSummaryTool(), true);
            RegisterTool(new SaveChatMilestoneTool(), true);

            RegisterTool(new GetKnowledgeSubgraphTool(), true);
            RegisterTool(new AddKnowledgeNodeTool(), true);
            RegisterTool(new ConnectKnowledgeNodesTool(), true);

            /*RegisterTool(new CreateGenealogyTaskTool(), true);
            RegisterTool(new UpdateTaskProgressTool(), true);
            RegisterTool(new ChangeTaskStatusTool(), true);*/

            RegisterTool(new GetUserProfileTool(), true);
            RegisterTool(new UpdateUserProfileTool(), true);
            RegisterTool(new RemoveUserPreferenceTool(), true);
        }
    }

    private static void RegisterTool(BaseTool tool, bool metaTool = false)
    {
        fTools.Add(tool.Sign, tool);

        MCPTool mcpTool = tool.CreateTool();
        if (mcpTool != null) {
            if (fTDE && !metaTool) {
                MCPToolDiscovery.Register(tool.Sign, mcpTool);
            }

            if (!fTDE || metaTool) {
                fMCPTools.Add(mcpTool);
            }
        }
    }

    internal static List<MCPTool> GetTools()
    {
        return fMCPTools;
    }

    public static List<MCPContent> ExecuteTool(string toolName, JsonElement args)
    {
        if (fTools.TryGetValue(toolName, out BaseTool cmd)) {
            return cmd.ExecuteTool(fBaseContext, args);
        } else {
            throw new ArgumentException($"Unknown tool: {toolName}");
        }
    }

    private static void RegisterResource(BaseResource resource)
    {
        fResources.Add(resource.Uri, resource);
    }

    internal static IEnumerable<BaseResource> GetResources()
    {
        return fResources.Values;
    }

    public static List<MCPResourceContents> GetResource(string uri)
    {
        if (fResources.TryGetValue(uri, out BaseResource res)) {
            return res.Get(fBaseContext);
        } else {
            return null;
        }
    }
}
