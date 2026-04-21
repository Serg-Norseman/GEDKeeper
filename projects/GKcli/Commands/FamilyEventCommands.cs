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
using GKCore.Events;

namespace GKcli.Commands;

internal class FamListEventTypesCommand : EventCommand
{
    public FamListEventTypesCommand() : base("family_list_event_types", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all available event types for families",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> { },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        return GetEventTypes(EventTarget.etFamily);
    }
}


internal class FamListEventsCommand : EventCommand
{
    public FamListEventsCommand() : base("family_list_events", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all events of a family by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["family_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" }
                },
                Required = new List<string> { "family_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string familyXRef = MCPHelper.GetRequiredArgument(args, "family_xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        return GetEventsList(baseContext, "family", familyRec);
    }
}


internal class FamAddEventCommand : EventCommand
{
    public FamAddEventCommand() : base("family_add_event", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add an event to a family by XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["family_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" },
                    ["tag"] = new MCPToolProperty { Type = "string", Description = "GEDCOM tag of the event type (from family_list_event_types)" },
                    ["type"] = new MCPToolProperty { Type = "string", Description = "Event type classification (optional, from family_list_event_types)" },
                    ["date"] = new MCPToolProperty { Type = "string", Description = "Date string. Follow the GEDCOM date specification (from gedcom_date_spec)" },
                    ["place"] = new MCPToolProperty { Type = "string", Description = "Place as a free-form string" },
                    ["location_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of a location record (alternative to place string)" },
                    ["cause"] = new MCPToolProperty { Type = "string", Description = "Cause of the event" },
                    ["agency"] = new MCPToolProperty { Type = "string", Description = "Agency responsible for the event" },
                    ["value"] = new MCPToolProperty { Type = "string", Description = "Fact value (used when the event is a fact/attribute), cannot contain the place of the event" },
                    ["husband_age"] = new MCPToolProperty { Type = "string", Description = "Age of the husband on the date of the event" },
                    ["wife_age"] = new MCPToolProperty { Type = "string", Description = "Age of the wife on the date of the event" },
                },
                Required = new List<string> { "family_xref", "tag" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string familyXRef = MCPHelper.GetRequiredArgument(args, "family_xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        return AddEvent(baseContext, familyRec, "family", args);
    }
}


internal class FamDeleteEventCommand : BaseCommand
{
    public FamDeleteEventCommand() : base("family_delete_event", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove an event from a family by family XRef and event index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["family_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" },
                    ["event_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the event in the family's event list" }
                },
                Required = new List<string> { "family_xref", "event_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string familyXRef = MCPHelper.GetRequiredArgument(args, "family_xref");
        int eventIndex = MCPHelper.GetIntArgument(args, "event_index", -1);

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        if (!familyRec.HasEvents)
            return MCPContent.CreateSimpleContent($"Family '{familyXRef}' has no events.");

        if (eventIndex < 0 || eventIndex >= familyRec.Events.Count)
            return MCPContent.CreateSimpleContent($"Invalid event index {eventIndex} for family '{familyXRef}' (has {familyRec.Events.Count} events).");

        var evt = familyRec.Events[eventIndex];
        string evtInfo = GKUtils.GetEventName(evt);

        familyRec.Events.RemoveAt(eventIndex);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Event removed from family '{familyXRef}' at index {eventIndex}: {evtInfo}");
    }
}
