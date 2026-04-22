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
using GKUI.Platform;

namespace GKcli.Commands;

internal class RepositoryMenuCommand : BaseCommand
{
    public RepositoryMenuCommand() : base("repositories", LSID.RPRepositories, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Repository, true, "Select a repository operation");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordListCommand"/>).
/// </summary>
internal class RepositoryListCommand : BaseCommand
{
    public RepositoryListCommand() : base("repository_list", LSID.Find, CommandCategory.Repository) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var selected = PromptHelper.SelectRecord(baseContext, GDMRecordType.rtRepository, "Select a repository", "Repository: {0}", "No records.");
    }
}


internal class RepositoryAddCommand : BaseCommand
{
    public RepositoryAddCommand() : base("repository_add", null, CommandCategory.Repository) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
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
        string name = MCPHelper.GetRequiredStr(args, "name");

        var repoRec = baseContext.Tree.CreateRepository();
        repoRec.RepositoryName = name;
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Repository added: {name} with XRef `{repoRec.XRef}`");
    }
}


internal class RepositoryEditCommand : BaseCommand
{
    public RepositoryEditCommand() : base("repository_edit", null, CommandCategory.Repository) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Edit an existing repository record to the database. Only provided fields will be updated. Use 'xref' to identify the record to modify.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit" },
                    ["name"] = new MCPToolProperty { Type = "string", Description = "New name of the repository item" },
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredStr(args, "xref");
        var repoRec = baseContext.Tree.FindXRef<GDMRepositoryRecord>(xref);
        if (repoRec == null)
            return MCPContent.CreateSimpleContent($"Repository not found with XRef: {xref}");

        string name = MCPHelper.GetOptionalStr(args, "name", null);
        if (name != null) {
            repoRec.RepositoryName = name;
        }

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Repository record updated: {repoRec.XRef} - \"{name}\"");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordDeleteCommand"/>).
/// </summary>
internal class RepositoryDeleteCommand : BaseCommand
{
    public RepositoryDeleteCommand() : base("repository_delete", null, CommandCategory.Repository) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
