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

internal class SourceMenuCommand : BaseCommand
{
    public SourceMenuCommand() : base("sources", LSID.RPSources, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Source, true, "Select a source operation");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordListCommand"/>).
/// </summary>
internal class SourceListCommand : BaseCommand
{
    public SourceListCommand() : base("source_list", LSID.Find, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.SelectRecord(baseContext, GDMRecordType.rtSource, "Select a source", "Source: {0}", "No records.");
    }
}


internal class SourceAddCommand : BaseCommand
{
    public SourceAddCommand() : base("source_add", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a new source to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["title"] = new MCPToolProperty { Type = "string", Description = "Source title" },
                    ["short_title"] = new MCPToolProperty { Type = "string", Description = "Source short title" },
                    ["author"] = new MCPToolProperty { Type = "string", Description = "Source author" },
                },
                Required = new List<string> { "title" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string title = MCPHelper.GetRequiredArgument(args, "title");
        string shortTitle = MCPHelper.GetStringArgument(args, "short_title", string.Empty);
        string author = MCPHelper.GetStringArgument(args, "author", string.Empty);

        var sourceRec = baseContext.Tree.CreateSource();
        sourceRec.Title.Lines.Text = title;

        if (!string.IsNullOrEmpty(shortTitle)) {
            sourceRec.ShortTitle = shortTitle;
        }

        if (!string.IsNullOrEmpty(author)) {
            sourceRec.Originator.Lines.Text = author;
        }

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Source added: {title} with XRef `{sourceRec.XRef}`");
    }
}


internal class SourceEditCommand : BaseCommand
{
    public SourceEditCommand() : base("source_edit", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Edit an existing source record in the database. Only provided fields will be updated. Use 'xref' to identify the record to modify.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit" },
                    ["title"] = new MCPToolProperty { Type = "string", Description = "New title of source record" },
                    ["short_title"] = new MCPToolProperty { Type = "string", Description = "New short title of source record" },
                    ["author"] = new MCPToolProperty { Type = "string", Description = "New author of source record" },
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var sourceRec = baseContext.Tree.FindXRef<GDMSourceRecord>(xref);
        if (sourceRec == null)
            return MCPContent.CreateSimpleContent($"Source record not found: '{xref}'.");

        string title = MCPHelper.GetStringArgument(args, "title", null);
        string shortTitle = MCPHelper.GetStringArgument(args, "short_title", null);
        string author = MCPHelper.GetStringArgument(args, "author", null);

        if (title != null) {
            sourceRec.Title.Lines.Text = title;
        }

        if (shortTitle != null) {
            sourceRec.ShortTitle = shortTitle;
        }

        if (author != null) {
            sourceRec.Originator.Lines.Text = author;
        }

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Source updated: {title} with XRef `{sourceRec.XRef}`");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordDeleteCommand"/>).
/// </summary>
internal class SourceDeleteCommand : BaseCommand
{
    public SourceDeleteCommand() : base("source_delete", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
