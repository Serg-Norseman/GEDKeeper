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

internal class RepositoryUpsertTool : BaseTool
{
    public RepositoryUpsertTool() : base("repository_upsert") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add new repository or update existing. Provide 'xref' to edit; omit 'xref' to create. 'name' required for new repositories.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of existing repository to update (omit for new)" },
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Repository name" }
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetOptionalStr(args, "xref", null);
        string name = MCPHelper.GetOptionalStr(args, "name", null);

        bool isEdit = !string.IsNullOrEmpty(xref);
        if (isEdit) {
            var repoRec = baseContext.Tree.FindXRef<GDMRepositoryRecord>(xref);
            if (repoRec == null)
                return MCPContent.CreateSimpleContent($"❌ Repository not found with XRef: {xref}");

            if (name != null) {
                repoRec.RepositoryName = name;
            }

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Repository record updated: {repoRec.XRef} - \"{repoRec.RepositoryName}\"");
        } else {
            if (string.IsNullOrEmpty(name))
                return MCPContent.CreateSimpleContent("❌ 'name' required for new repository");

            var repoRec = baseContext.Tree.CreateRepository();
            repoRec.RepositoryName = name;

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Repository added: {name} with XRef `{repoRec.XRef}`");
        }
    }
}
