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

internal class NoteUpsertTool : BaseTool
{
    public NoteUpsertTool() : base("note_upsert") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add new note or update existing. Provide 'xref' to edit; omit 'xref' to create. 'text' required for new notes.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit (omit for new)" },
                    ["text"] = new MCPToolProperty { Type = "string", Description = "Note text content" },
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetOptionalStr(args, "xref", null);
        string text = MCPHelper.GetOptionalStr(args, "text", null);

        bool isEdit = !string.IsNullOrEmpty(xref);
        if (isEdit) {
            var noteRec = baseContext.Tree.FindXRef<GDMNoteRecord>(xref);
            if (noteRec == null)
                return MCPContent.CreateSimpleContent($"❌ Note record not found: '{xref}'.");

            if (text != null) noteRec.Lines.Text = text;

            baseContext.SetModified();
            string preview = (text != null && text.Length > 10) ? text.Substring(0, 10) + "..." : text;
            return MCPContent.CreateSimpleContent($"✅ Note updated: {preview} with XRef `{noteRec.XRef}`");
        } else {
            if (string.IsNullOrEmpty(text))
                return MCPContent.CreateSimpleContent("❌ 'text' required for new note");

            var noteRec = baseContext.Tree.CreateNote();
            noteRec.Lines.Text = text;

            baseContext.SetModified();
            string preview = (text != null && text.Length > 10) ? text.Substring(0, 10) + "..." : text;
            return MCPContent.CreateSimpleContent($"✅ Note added: {text} with XRef `{noteRec.XRef}`");
        }
    }
}
