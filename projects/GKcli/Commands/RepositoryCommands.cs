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
using GKCore.Locales;

namespace GKcli.Commands;

internal class RepositoryMenuCommand : BaseCommand
{
    public RepositoryMenuCommand() : base("repositories", LSID.RPRepositories, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Repository, true, "Select a repository operation");
    }
}


internal class RepositoryListCommand : RecordCommand
{
    public RepositoryListCommand() : base("repository_list", LSID.Find, CommandCategory.Repository) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var selected = SelectRecord(baseContext, GDMRecordType.rtRepository, "Select a repository", "Repository: {0}", "No records.");
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all repositories in the database",
            InputSchema = MCPToolInputSchema.Empty
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtRepository);
        if (recList.Count == 0)
            return MCPContent.CreateSimpleContent("No repositories in database.");

        var rows = new List<string> {
            $"Repositories ({recList.Count}):",
            "| XRef | Repository |",
            "|---|---|"
        };
        foreach (var rec in recList) {
            var repoRec = (GDMRepositoryRecord)rec;
            rows.Add($"|{rec.XRef}|{repoRec.RepositoryName}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class RepositoryAddCommand : BaseCommand
{
    public RepositoryAddCommand() : base("repository_add", null, CommandCategory.Repository) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a new repository to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Repository name" }
                },
                Required = new List<string> { "name" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string name = MCPHelper.GetRequiredArgument(args, "name");

        var repoRec = baseContext.Tree.CreateRepository();
        repoRec.RepositoryName = name;
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Repository added: {name} with XRef `{repoRec.XRef}`");
    }
}


internal class RepositoryDeleteCommand : BaseCommand
{
    public RepositoryDeleteCommand() : base("repository_delete", null, CommandCategory.Repository) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete a repository from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the repository (e.g., 'R1')" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var repoRec = baseContext.Tree.FindXRef<GDMRepositoryRecord>(xref);
        if (repoRec == null)
            return MCPContent.CreateSimpleContent($"Repository not found with XRef: {xref}");

        baseContext.DeleteRecord(repoRec);

        return MCPContent.CreateSimpleContent($"Repository deleted: {xref}");
    }
}
