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

internal class LocationUpsertTool : BaseTool
{
    public LocationUpsertTool() : base("location_upsert") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add new location record or update existing. Provide 'xref' to edit; omit 'xref' to create. 'name' required for new locations.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of existing location to update (omit for new)" },
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Name of the location item" },
                    ["lati"] = new MCPToolProperty { Type = "number", Description = "Latitude" },
                    ["long"] = new MCPToolProperty { Type = "number", Description = "Longitude" },
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetOptionalStr(args, "xref", null);
        string name = MCPHelper.GetOptionalStr(args, "name", null);
        bool hasLati = MCPHelper.HasArg(args, "lati");
        bool hasLong = MCPHelper.HasArg(args, "long");
        double lat = MCPHelper.GetOptionalDbl(args, "lati", 0);
        double lng = MCPHelper.GetOptionalDbl(args, "long", 0);

        bool isEdit = !string.IsNullOrEmpty(xref);
        if (isEdit) {
            var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(xref);
            if (locRec == null)
                return MCPContent.CreateSimpleContent($"❌ Location not found with XRef: {xref}");

            if (name != null) {
                locRec.LocationName = name;
            }

            if (hasLati) {
                locRec.Map.Lati = lat;
            }

            if (hasLong) {
                locRec.Map.Long = lng;
            }

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Location record updated: {locRec.XRef} - \"{locRec.LocationName}\"");
        } else {
            if (string.IsNullOrEmpty(name))
                return MCPContent.CreateSimpleContent("❌ 'name' required for new location");

            var locRec = baseContext.Tree.CreateLocation();
            locRec.LocationName = name;

            if (hasLati) {
                locRec.Map.Lati = lat;
            }

            if (hasLong) {
                locRec.Map.Long = lng;
            }

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Location record added: {locRec.XRef} - \"{name}\"");
        }
    }
}
