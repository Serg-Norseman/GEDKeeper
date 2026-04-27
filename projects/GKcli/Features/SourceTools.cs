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

namespace GKcli.Features;

internal class SourceUpsertTool : BaseTool
{
    public SourceUpsertTool() : base("source_upsert") { }

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
