/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json;
using GDModel;
using GKcli.MCP;
using GKCore;

namespace GKcli.Commands;


internal class SourceListRepositoriesCommand : BaseCommand
{
    public SourceListRepositoriesCommand() : base("source_list_repositories", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all repository citations of a source by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["source_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the source (e.g., 'S1')" }
                },
                Required = new List<string> { "source_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string sourceXRef = MCPHelper.GetRequiredArgument(args, "source_xref");
        var sourceRec = baseContext.Tree.FindXRef<GDMSourceRecord>(sourceXRef);
        if (sourceRec == null)
            return MCPContent.CreateSimpleContent($"Source not found with XRef: {sourceXRef}");

        if (sourceRec.RepositoryCitations.Count <= 0)
            return MCPContent.CreateSimpleContent($"Source '{sourceXRef}' has no repository citations.");

        var rows = new List<string> {
            $"Repository citations of source '{sourceXRef}' ({sourceRec.RepositoryCitations.Count}):",
            "| Index | Repository XRef | Name |",
            "|---|---|---|"
        };
        for (int i = 0; i < sourceRec.RepositoryCitations.Count; i++) {
            var repoPtr = sourceRec.RepositoryCitations[i];
            var repoRec = baseContext.Tree.GetPtrValue<GDMRepositoryRecord>(repoPtr);

            if (repoRec != null) {
                string repoName = GKUtils.GetRecordName(baseContext.Tree, repoRec, false);
                rows.Add($"|{i}|{repoPtr.XRef}|{repoName}|");
            } else {
                rows.Add($"|{i}|{repoPtr.XRef}|(not found)|");
            }
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class SourceAddRepositoryCommand : BaseCommand
{
    public SourceAddRepositoryCommand() : base("source_add_repository", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add an repository citation to a source by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["source_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the source (e.g., 'S1', 'S2')" },
                    ["repository_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the repository (e.g., 'R1', 'R2')" }
                },
                Required = new List<string> { "source_xref", "repository_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string sourceXRef = MCPHelper.GetRequiredArgument(args, "source_xref");
        var sourceRec = baseContext.Tree.FindXRef<GDMSourceRecord>(sourceXRef);
        if (sourceRec == null)
            return MCPContent.CreateSimpleContent($"Source not found with XRef: {sourceXRef}");

        string repositoryXRef = MCPHelper.GetRequiredArgument(args, "repository_xref");
        var repoRec = baseContext.Tree.FindXRef<GDMRepositoryRecord>(repositoryXRef);
        if (repoRec == null)
            return MCPContent.CreateSimpleContent($"Repository not found with XRef: {repositoryXRef}");

        if (sourceRec.FindRepository(repoRec) != null)
            return MCPContent.CreateSimpleContent($"Source {sourceXRef} already cites repository '{repositoryXRef}'.");

        sourceRec.AddRepository(repoRec);
        baseContext.SetModified();

        string repoName = GKUtils.GetRecordName(baseContext.Tree, repoRec, false);
        return MCPContent.CreateSimpleContent($"Repository citation added to source '{sourceXRef}': {repoName} ({repositoryXRef})");
    }
}


internal class SourceDeleteRepositoryCommand : BaseCommand
{
    public SourceDeleteRepositoryCommand() : base("source_delete_repository", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove an repository citation from a source by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["source_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the source (e.g., 'S1', 'S2')" },
                    ["repository_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the repository (e.g., 'R1', 'R2')" }
                },
                Required = new List<string> { "source_xref", "repository_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string sourceXRef = MCPHelper.GetRequiredArgument(args, "source_xref");
        var sourceRec = baseContext.Tree.FindXRef<GDMSourceRecord>(sourceXRef);
        if (sourceRec == null)
            return MCPContent.CreateSimpleContent($"Source not found with XRef: {sourceXRef}");

        string repositoryXRef = MCPHelper.GetRequiredArgument(args, "repository_xref");
        var repoRec = baseContext.Tree.FindXRef<GDMRepositoryRecord>(repositoryXRef);
        if (repoRec == null)
            return MCPContent.CreateSimpleContent($"Repository not found with XRef: {repositoryXRef}");

        if (sourceRec.FindRepository(repoRec) == null)
            return MCPContent.CreateSimpleContent($"There is no citation for repository '{repositoryXRef}' in source {sourceXRef}.");

        sourceRec.RemoveRepository(repoRec);
        baseContext.SetModified();

        string repoName = GKUtils.GetRecordName(baseContext.Tree, repoRec, false);
        return MCPContent.CreateSimpleContent($"Repository citation removed from source '{sourceXRef}': {repoName} ({repositoryXRef})");
    }
}
