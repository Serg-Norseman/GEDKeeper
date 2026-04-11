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

internal class SourceMenuCommand : BaseCommand
{
    public SourceMenuCommand() : base("sources", LSID.RPSources, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Source, true, "Select a source operation");
    }
}


internal class SourceListCommand : RecordCommand
{
    public SourceListCommand() : base("source_list", LSID.Find, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        SelectRecord(baseContext, GDMRecordType.rtSource, "Select a source", "Source: {0}", "No records.");
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all sources in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtSource);
        return MCPHelper.PageableTable("sources", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Short title | Source title |\n|---|---|---|";
            } else {
                var rec = (GDMSourceRecord)recList[index];
                string sourceTitle = rec.Title.Lines.Text;
                return $"|{rec.XRef}|{rec.ShortTitle}|{sourceTitle}|";
            }
        });
    }
}


internal class SourceAddCommand : BaseCommand
{
    public SourceAddCommand() : base("source_add", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a new source to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["title"] = new MCPToolProperty { Type = "string", Description = "Source title" }
                },
                Required = new List<string> { "title" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string title = MCPHelper.GetRequiredArgument(args, "title");

        var sourceRec = baseContext.Tree.CreateSource();
        sourceRec.Title.Lines.Text = title;
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Source added: {title} with XRef `{sourceRec.XRef}`");
    }
}


internal class SourceDeleteCommand : BaseCommand
{
    public SourceDeleteCommand() : base("source_delete", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete a source from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the source (e.g., 'S1')" }
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
            return MCPContent.CreateSimpleContent($"Source not found with XRef: {xref}");

        baseContext.DeleteRecord(sourceRec);

        return MCPContent.CreateSimpleContent($"Source deleted: {xref}");
    }
}
