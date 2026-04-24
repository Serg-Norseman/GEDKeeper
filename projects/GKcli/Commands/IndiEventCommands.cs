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

internal class IndiListEventTypesCommand : EventCommand
{
    public IndiListEventTypesCommand() : base("individual_list_event_types", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all available event types for individuals",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> { },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        return GetEventTypes(EventTarget.etIndividual);
    }
}


internal class IndiListEventsCommand : EventCommand
{
    public IndiListEventsCommand() : base("individual_list_events", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all events of an individual by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" }
                },
                Required = new List<string> { "individual_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        return GetEventsList(baseContext, "individual", indiRec);
    }
}


internal class IndiAddEventCommand : EventCommand
{
    public IndiAddEventCommand() : base("individual_add_event", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add an event to an individual by XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" },
                    ["tag"] = new MCPToolProperty { Type = "string", Description = "GEDCOM tag of the event type (from individual_list_event_types)" },
                    ["type"] = new MCPToolProperty { Type = "string", Description = "Event type classification (optional, from individual_list_event_types)" },
                    ["date"] = new MCPToolProperty { Type = "string", Description = "Date string, strictly with the GEDCOM Date Spec" },
                    ["place"] = new MCPToolProperty { Type = "string", Description = "Place as a free-form string" },
                    ["location_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of a location record (alternative to place string)" },
                    ["cause"] = new MCPToolProperty { Type = "string", Description = "Cause of the event" },
                    ["agency"] = new MCPToolProperty { Type = "string", Description = "Agency responsible for the event" },
                    ["value"] = new MCPToolProperty { Type = "string", Description = "Fact value (used when the event is a fact/attribute), cannot contain the place of the event" },
                    ["age"] = new MCPToolProperty { Type = "string", Description = "Age of the individual on the date of the event" },
                },
                Required = new List<string> { "individual_xref", "tag" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        return AddEvent(baseContext, indiRec, "individual", args);
    }
}


internal class IndiEditEventCommand : EventCommand
{
    public IndiEditEventCommand() : base("individual_edit_event", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add an event to an individual by XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" },
                    ["event_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the event in the individual's event list" },
                    //["tag"] = new MCPToolProperty { Type = "string", Description = "GEDCOM tag of the event type (from individual_list_event_types)" },
                    //["type"] = new MCPToolProperty { Type = "string", Description = "Event type classification (optional, from individual_list_event_types)" },
                    ["date"] = new MCPToolProperty { Type = "string", Description = "Date string, strictly with the GEDCOM Date Spec" },
                    ["place"] = new MCPToolProperty { Type = "string", Description = "Place as a free-form string" },
                    ["location_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of a location record (alternative to place string)" },
                    ["cause"] = new MCPToolProperty { Type = "string", Description = "Cause of the event" },
                    ["agency"] = new MCPToolProperty { Type = "string", Description = "Agency responsible for the event" },
                    ["value"] = new MCPToolProperty { Type = "string", Description = "Fact value (used when the event is a fact/attribute), cannot contain the place of the event" },
                    ["age"] = new MCPToolProperty { Type = "string", Description = "Age of the individual on the date of the event" },
                },
                Required = new List<string> { "individual_xref", "event_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        return EditEvent(baseContext, indiRec, "individual", args);
    }
}


internal class IndiDeleteEventCommand : BaseCommand
{
    public IndiDeleteEventCommand() : base("individual_delete_event", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove an event from an individual by individual XRef and event index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" },
                    ["event_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the event in the individual's event list" }
                },
                Required = new List<string> { "individual_xref", "event_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");
        int eventIndex = MCPHelper.GetOptionalInt(args, "event_index", -1);

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        if (!indiRec.HasEvents)
            return MCPContent.CreateSimpleContent($"Individual '{individualXRef}' has no events.");

        if (eventIndex < 0 || eventIndex >= indiRec.Events.Count)
            return MCPContent.CreateSimpleContent($"Invalid event index {eventIndex} for individual '{individualXRef}' (has {indiRec.Events.Count} events).");

        var evt = indiRec.Events[eventIndex];
        string evtInfo = GKUtils.GetEventName(evt);

        indiRec.Events.RemoveAt(eventIndex);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Event removed from individual '{individualXRef}' at index {eventIndex}: {evtInfo}");
    }
}
