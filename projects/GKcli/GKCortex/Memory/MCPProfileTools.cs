/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json;
using GKCore;
using GKCortex.Features;
using GKCortex.MCP;
using GKCortex.Protocols;

namespace GKCortex.Memory;

internal class GetUserProfileTool : BaseTool
{
    public GetUserProfileTool() : base("get_user_profile") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Read the complete user profile with all stored preferences",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty>(),
                Required = new List<string>()
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = true,
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var service = new MemoryService();
        var prefs = service.GetAllPreferencesAsync().GetAwaiter().GetResult();

        if (prefs.Count == 0)
            return MCPContent.CreateSimpleContent("❌ User profile is empty. No specific style triggers or research focus preferences have been set yet.");

        return MCPContent.CreateSimpleContent(JsonSerializer.Serialize(prefs, new JsonSerializerOptions { WriteIndented = true }));
    }
}


internal class UpdateUserProfileTool : BaseTool
{
    public UpdateUserProfileTool() : base("update_user_profile") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Write or update a fact about the user in their profile. Keys are strictly controlled via instructions.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["key"] = new MCPToolProperty { Type = "string", Description = "The preference key (must be from the allowed list)" },
                    ["value"] = new MCPToolProperty { Type = "string", Description = "The value to assign to the preference key" }
                },
                Required = new List<string> { "key", "value" }
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = false,
                DestructiveHint = false,
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string key = MCPHelper.GetRequiredStr(args, "key");
        string value = MCPHelper.GetRequiredStr(args, "value");

        var service = new MemoryService();
        bool success = service.SetPreferenceAsync(key, value, 1.0).GetAwaiter().GetResult();

        if (!success)
            return MCPContent.CreateSimpleContent("❌ Provided key is empty or invalid.");

        return MCPContent.CreateSimpleContent($"✅ User profile successfully updated. Parameter '{key.ToLowerInvariant()}' has been recorded.");
    }
}


internal class RemoveUserPreferenceTool : BaseTool
{
    public RemoveUserPreferenceTool() : base("remove_user_preference") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove an outdated or incorrect preference from the user profile",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["key"] = new MCPToolProperty { Type = "string", Description = "The preference key to delete" }
                },
                Required = new List<string> { "key" }
            },
            Annotations = new MCPToolAnnotations() {
                ReadOnlyHint = false,
                DestructiveHint = true
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string key = MCPHelper.GetRequiredStr(args, "key");

        var service = new MemoryService();
        bool deleted = service.DeletePreferenceAsync(key).GetAwaiter().GetResult();

        if (!deleted)
            return MCPContent.CreateSimpleContent($"❌ Parameter '{key}' not found in user profile.");

        return MCPContent.CreateSimpleContent($"✅ Parameter '{key}' successfully removed from user profile.");
    }
}
