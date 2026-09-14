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
using GKCortex.LMChat;
using GKCortex.Protocols;
using GKCortex.Tools;

namespace GKCortex.MCP;

public class MCPController
{
    private static BaseContext fBaseContext = new BaseContext(null);
    private static ILMChat fLMChat = null;
    private static readonly Dictionary<string, BaseTool> fTools = new Dictionary<string, BaseTool>();
    private static readonly List<MCPTool> fMCPTools = new List<MCPTool>();
    private static readonly Dictionary<string, BaseResource> fResources = new Dictionary<string, BaseResource>();
    private static bool fTDE = false;

    static MCPController()
    {
    }

    public static void SetLMChat(ILMChat lmChat)
    {
        if (lmChat != null) {
            fLMChat = lmChat;
        }
    }

    public static ILMChat GetLMChat()
    {
        return fLMChat;
    }

    public static void SetContext(BaseContext baseContext)
    {
        if (baseContext != null) {
            fBaseContext = baseContext;
        }
    }

    public static void InitFeatures(bool tdeMode, bool ragMode)
    {
        fTDE = tdeMode;

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

            RegisterTool(new GetActiveTasksTool(), true);
            RegisterTool(new CreateGenealogyTaskTool(), true);
            RegisterTool(new UpdateTaskProgressTool(), true);
            RegisterTool(new ChangeTaskStatusTool(), true);

            RegisterTool(new GetUserProfileTool(), true);
            RegisterTool(new UpdateUserProfileTool(), true);
            RegisterTool(new RemoveUserPreferenceTool(), true);
        }
    }

    public static void RegisterTool(BaseTool tool, bool anytime = false)
    {
        fTools.Add(tool.Sign, tool);

        MCPTool mcpTool = tool.CreateTool();
        if (mcpTool != null) {
            if (fTDE && !anytime) {
                MCPToolDiscovery.Register(tool.Sign, mcpTool);
            }

            if (!fTDE || anytime) {
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

    public static void RegisterResource(BaseResource resource)
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
