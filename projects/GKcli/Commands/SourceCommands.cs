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


/// <summary>
/// For console use only (for MCP - see <see cref="SourceUpsertCommand"/>).
/// </summary>
internal class SourceAddCommand : BaseCommand
{
    public SourceAddCommand() : base("source_add", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="SourceUpsertCommand"/>).
/// </summary>
internal class SourceEditCommand : BaseCommand
{
    public SourceEditCommand() : base("source_edit", null, CommandCategory.Source) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
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


/// <summary>
/// For MCP use only.
/// </summary>
internal class SourceUpsertCommand : BaseCommand
{
    public SourceUpsertCommand() : base("source_upsert", null, CommandCategory.None) { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add new source or update existing. Provide 'xref' to edit; omit 'xref' to create. 'title' required for new sources.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of existing source to update (omit for new)" },
                    ["title"] = new MCPToolProperty { Type = "string", Description = "Source title (required when creating)" },
                    ["short_title"] = new MCPToolProperty { Type = "string", Description = "Source short title" },
                    ["author"] = new MCPToolProperty { Type = "string", Description = "Source author" },
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetOptionalStr(args, "xref", null);
        string title = MCPHelper.GetOptionalStr(args, "title", null);
        string shortTitle = MCPHelper.GetOptionalStr(args, "short_title", null);
        string author = MCPHelper.GetOptionalStr(args, "author", null);

        bool isEdit = !string.IsNullOrEmpty(xref);
        if (isEdit) {
            var sourceRec = baseContext.Tree.FindXRef<GDMSourceRecord>(xref);
            if (sourceRec == null)
                return MCPContent.CreateSimpleContent($"❌ Source not found: '{xref}'");

            if (title != null) sourceRec.Title.Lines.Text = title;
            if (shortTitle != null) sourceRec.ShortTitle = shortTitle;
            if (author != null) sourceRec.Originator.Lines.Text = author;

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Source updated: {title} with XRef `{sourceRec.XRef}`");
        } else {
            if (string.IsNullOrEmpty(title))
                return MCPContent.CreateSimpleContent("❌ 'title' required for new source");

            var sourceRec = baseContext.Tree.CreateSource();
            sourceRec.Title.Lines.Text = title;
            if (!string.IsNullOrEmpty(shortTitle)) sourceRec.ShortTitle = shortTitle;
            if (!string.IsNullOrEmpty(author)) sourceRec.Originator.Lines.Text = author;

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Source added: {title} with XRef `{sourceRec.XRef}`");
        }
    }
}
