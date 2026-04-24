/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using GDModel;
using GKcli.MCP;
using GKCore;
using GKCore.Controllers;
using GKCore.Events;
using GKCore.Locales;
using GKUI.Platform;
using Sharprompt;

namespace GKcli.Commands;

internal class EventEditCommand : BaseCommand
{
    public EventEditCommand() : base("edit_event", LSID.Event, CommandCategory.Events)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var evt = CommandController.GetVariable<GDMCustomEvent>("selectedObj");
        if (evt == null) {
            PromptHelper.WriteLine("Error: Expected an event");
            return;
        }

        var editedEvent = EditEvent(baseContext, evt);
        if (editedEvent != null) {
            PromptHelper.WriteLine("Event edited successfully");
        } else {
            PromptHelper.WriteLine("Event editing cancelled or failed");
        }
    }

    private GDMCustomEvent EditEvent(BaseContext baseContext, GDMCustomEvent originalEvent)
    {
        var eventToEdit = originalEvent; //(GDMCustomEvent)originalEvent.Clone();

        EventTarget evtTarget = eventToEdit is GDMFamilyEvent ? EventTarget.etFamily : EventTarget.etIndividual;
        var freqEventTypes = BaseController.GetFrequencyEventTypes(baseContext, evtTarget);

        bool continueEditing = true;
        while (continueEditing) {
            var options = new[]
            {
                "Edit Type",
                "Edit Date",
                "Edit Place",
                "Edit Cause",
                "Edit Agency",
                "Edit Value",
                "Show Current Values",
                "Save Changes",
                "Cancel"
            };

            var selection = Prompt.Select("Select an option to edit", options);

            switch (selection) {
                case "Edit Type":
                    var fet = Prompt.Select("Select event type", freqEventTypes, pageSize: 10, textSelector: def => def.Name);
                    var eventType = fet.Ident;
                    eventToEdit.SetName(eventType.Tag);
                    eventToEdit.Classification = eventType.Type;
                    break;

                case "Edit Date":
                    var dateString = Prompt.Input<string>("Enter date (e.g., 1 JAN 1990)", defaultValue: eventToEdit.Date.StringValue);
                    if (!string.IsNullOrEmpty(dateString)) {
                        eventToEdit.Date.ParseString(dateString);
                    }
                    break;

                case "Edit Place":
                    var placeString = Prompt.Input<string>("Enter place", defaultValue: eventToEdit.Place.StringValue);
                    eventToEdit.Place.StringValue = placeString;
                    break;

                case "Edit Cause":
                    var cause = Prompt.Input<string>("Enter cause", defaultValue: eventToEdit.Cause);
                    eventToEdit.Cause = cause;
                    break;

                case "Edit Agency":
                    var agency = Prompt.Input<string>("Enter agency", defaultValue: eventToEdit.Agency);
                    eventToEdit.Agency = agency;
                    break;

                case "Edit Value":
                    var isAttr = eventToEdit is GDMIndividualAttribute;
                    if (isAttr) {
                        var value = Prompt.Input<string>("Enter value", defaultValue: eventToEdit.StringValue);
                        eventToEdit.StringValue = value;
                    } else {
                        PromptHelper.WriteLine("This event type doesn't have a value field.");
                    }
                    break;

                case "Show Current Values":
                    ShowCurrentValues(eventToEdit);
                    break;

                case "Save Changes":
                    if (ValidateAndAccept(eventToEdit)) {
                        originalEvent.Assign(eventToEdit);
                        return originalEvent;
                    }
                    break;

                case "Cancel":
                    continueEditing = false;
                    break;
            }
        }

        return null;
    }

    private bool ValidateAndAccept(GDMCustomEvent eventToAccept)
    {
        try {
            GDMCustomDate dt = eventToAccept.Date.Value;
            if (dt == null) {
                PromptHelper.WriteLine("Error: Date is required");
                return false;
            }

            var eventDef = AppHost.EventDefinitions.Find(eventToAccept);
            BaseController.ValidateEvent(eventToAccept, eventDef);

            return true;
        } catch (Exception ex) {
            PromptHelper.WriteLine($"Validation error: {ex.Message}");
            return false;
        }
    }

    private void ShowCurrentValues(GDMCustomEvent evt)
    {
        string eventType = GKUtils.GetEventName(evt);

        PromptHelper.WriteLine($"Event Type: [yellow]{eventType}[/]");
        PromptHelper.WriteLine($"Date: [yellow]{evt.Date.StringValue}[/]");
        PromptHelper.WriteLine($"Place: [yellow]{evt.Place.StringValue}[/]");
        PromptHelper.WriteLine($"Cause: [yellow]{evt.Cause}[/]");
        PromptHelper.WriteLine($"Agency: [yellow]{evt.Agency}[/]");
        PromptHelper.WriteLine($"Value: [yellow]{evt.StringValue}[/]");
    }
}


internal abstract class EventCommand : BaseCommand
{
    protected EventCommand(string sign, Enum lsid, CommandCategory category) : base(sign, lsid, category) { }

    protected static List<string> GetEventTypes(EventTarget eventTarget)
    {
        var eventTypes = BaseController.GetEventTypes(eventTarget);
        var rows = new List<string>(eventTypes.Count);
        for (int i = 0; i < eventTypes.Count; i++) {
            var evt = eventTypes[i];
            rows.Add(evt.DisplayName);
        }
        return rows;
    }

    protected static List<MCPContent> GetEventTypesTable(EventTarget eventTarget)
    {
        var eventTypes = BaseController.GetEventTypes(eventTarget);

        var rows = new List<string>(eventTypes.Count + 2) {
            "|DisplayName|Tag|Type|\n|---|---|---|"
        };
        for (int i = 0; i < eventTypes.Count; i++) {
            var evt = eventTypes[i];
            rows.Add($"|{evt.DisplayName}|{evt.Tag}|{evt.Type}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }

    protected static List<MCPContent> GetEventsList(BaseContext baseContext, string recName, GDMRecordWithEvents recordWithEvents)
    {
        if (!recordWithEvents.HasEvents)
            return MCPContent.CreateSimpleContent($"{CLIHelper.ToUpperFirst(recName)} '{recordWithEvents.XRef}' has no events.");

        var rows = new List<string> {
            $"Events for {recName} '{recordWithEvents.XRef}' ({recordWithEvents.Events.Count}):",
            "| Index | Event | Date | Age | Place | Cause | Sources | Notes | Media |",
            "|---|---|---|---|---|---|---|---|---|"
        };
        for (int i = 0; i < recordWithEvents.Events.Count; i++) {
            var evt = recordWithEvents.Events[i];
            string eventName = GKUtils.GetEventName(evt);
            string dateStr = GKUtils.GetDateDisplayString(evt.Date.Value);
            string ageStr = GKUtils.GetAgeDisplayStr(evt);
            string placeStr = GKUtils.GetEventPlaceAndAttributeValues(evt);
            string causeStr = GKUtils.GetEventCause(evt);
            string sourcesStr = evt.HasSourceCitations ? evt.SourceCitations.Count.ToString() : string.Empty;
            string notesStr = evt.HasNotes ? evt.Notes.Count.ToString() : string.Empty;
            string mediaStr = evt.HasMultimediaLinks ? evt.MultimediaLinks.Count.ToString() : string.Empty;
            rows.Add($"|{i + 1}|{eventName}|{dateStr}|{ageStr}|{placeStr}|{causeStr}|{sourcesStr}|{notesStr}|{mediaStr}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }

    protected static List<MCPContent> AddEvent(BaseContext baseContext, GDMRecordWithEvents record, string recName, JsonElement args)
    {
        string argType = MCPHelper.GetRequiredStr(args, "type");
        EventDef matchedDef = AppHost.EventDefinitions.Find(argType);
        if (matchedDef == null)
            return MCPContent.CreateSimpleContent($"Event type '{argType}' not found");

        // Determine if this tag corresponds to a fact
        bool isFact = matchedDef.HasValue();

        bool isIndi = (record is GDMIndividualRecord);
        GDMCustomEvent newEvent;
        if (isIndi) {
            if (isFact) {
                newEvent = new GDMIndividualAttribute();
            } else {
                newEvent = new GDMIndividualEvent();
            }
        } else {
            newEvent = new GDMFamilyEvent();
        }
        newEvent.SetName(matchedDef.Tag);
        newEvent.Classification = matchedDef.Type;

        // Date
        string dateStr = MCPHelper.GetOptionalStr(args, "date", "");
        if (!string.IsNullOrEmpty(dateStr)) {
            // Prompt is configured to transmit only GEDCOM-compatible strings.
            newEvent.Date.ParseString(dateStr);
        }

        // Place (string or location xref)
        string placeStr = MCPHelper.GetOptionalStr(args, "place", "");
        string locationXRef = MCPHelper.GetOptionalStr(args, "location_xref", "");
        if (!string.IsNullOrEmpty(locationXRef)) {
            var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
            if (locRec == null)
                return MCPContent.CreateSimpleContent($"Location not found with XRef: {locationXRef}");
            baseContext.Tree.SetPtrValue(newEvent.Place.Location, locRec);
        } else if (!string.IsNullOrEmpty(placeStr)) {
            newEvent.Place.StringValue = placeStr;
        }

        // Cause
        string cause = MCPHelper.GetOptionalStr(args, "cause", "");
        if (!string.IsNullOrEmpty(cause)) {
            newEvent.Cause = cause;
        }

        // Agency
        string agency = MCPHelper.GetOptionalStr(args, "agency", "");
        if (!string.IsNullOrEmpty(agency)) {
            newEvent.Agency = agency;
        }

        // Value (for facts)
        if (isFact) {
            string value = MCPHelper.GetOptionalStr(args, "value", "");
            newEvent.StringValue = value;
        }

        if (isIndi) {
            string indiAge = MCPHelper.GetOptionalStr(args, "age", null);
            if (!string.IsNullOrEmpty(indiAge)) {
                ((GDMIndividualEventDetail)newEvent).Age.StringValue = AgeEditDlgController.GetAgeStr(indiAge);
            }
        } else {
            string husbAge = MCPHelper.GetOptionalStr(args, "husband_age", null);
            if (!string.IsNullOrEmpty(husbAge)) {
                ((GDMFamilyEvent)newEvent).HusbandAge.StringValue = AgeEditDlgController.GetAgeStr(husbAge);
            }

            string wifeAge = MCPHelper.GetOptionalStr(args, "wife_age", null);
            if (!string.IsNullOrEmpty(wifeAge)) {
                ((GDMFamilyEvent)newEvent).WifeAge.StringValue = AgeEditDlgController.GetAgeStr(wifeAge);
            }
        }

        record.Events.Add(newEvent);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Event '{argType}' added to {recName} '{record.XRef}' at index {record.Events.Count - 1}");
    }

    protected static List<MCPContent> EditEvent(BaseContext baseContext, GDMRecordWithEvents record, string recName, JsonElement args)
    {
        int eventIndex = MCPHelper.GetOptionalInt(args, "event_index", -1);

        if (!record.HasEvents)
            return MCPContent.CreateSimpleContent($"Record '{recName}' has no events.");

        if (eventIndex < 0 || eventIndex >= record.Events.Count)
            return MCPContent.CreateSimpleContent($"Invalid event index {eventIndex} for record '{record.XRef}' (has {record.Events.Count} events).");

        var evt = record.Events[eventIndex];

        //string tag = MCPHelper.GetOptionalStr(args, "tag", null);
        //string argType = MCPHelper.GetOptionalStr(args, "type", null);
        //EventDef matchedDef = AppHost.EventDefinitions.Find(tag, argType);
        EventDef matchedDef = AppHost.EventDefinitions.Find(evt);
        // Determine if this tag corresponds to a fact
        bool isFact = matchedDef != null && matchedDef.HasValue();

        bool isIndi = (record is GDMIndividualRecord);
        //newEvent.SetName(tag);
        //newEvent.Classification = matchedDef != null ? matchedDef.Type : argType;

        // Date, prompt is configured to transmit only GEDCOM-compatible strings.
        string dateStr = MCPHelper.GetOptionalStr(args, "date", null);
        if (dateStr != null) {
            evt.Date.ParseString(dateStr);
        }

        // Place (string or location xref)
        string placeStr = MCPHelper.GetOptionalStr(args, "place", null);
        string locationXRef = MCPHelper.GetOptionalStr(args, "location_xref", null);
        if (locationXRef != null) {
            var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
            if (!string.IsNullOrEmpty(locationXRef) && locRec == null)
                return MCPContent.CreateSimpleContent($"Location not found with XRef: {locationXRef}");
            baseContext.Tree.SetPtrValue(evt.Place.Location, locRec);
        } else if (placeStr != null) {
            evt.Place.StringValue = placeStr;
        }

        // Cause
        string cause = MCPHelper.GetOptionalStr(args, "cause", null);
        if (cause != null) {
            evt.Cause = cause;
        }

        // Agency
        string agency = MCPHelper.GetOptionalStr(args, "agency", null);
        if (agency != null) {
            evt.Agency = agency;
        }

        // Value (for facts)
        if (isFact) {
            string value = MCPHelper.GetOptionalStr(args, "value", null);
            if (value != null) {
                evt.StringValue = value;
            }
        }

        if (isIndi) {
            string indiAge = MCPHelper.GetOptionalStr(args, "age", null);
            if (!string.IsNullOrEmpty(indiAge)) {
                ((GDMIndividualEventDetail)evt).Age.StringValue = AgeEditDlgController.GetAgeStr(indiAge);
            }
        } else {
            string husbAge = MCPHelper.GetOptionalStr(args, "husband_age", null);
            if (!string.IsNullOrEmpty(husbAge)) {
                ((GDMFamilyEvent)evt).HusbandAge.StringValue = AgeEditDlgController.GetAgeStr(husbAge);
            }

            string wifeAge = MCPHelper.GetOptionalStr(args, "wife_age", null);
            if (!string.IsNullOrEmpty(wifeAge)) {
                ((GDMFamilyEvent)evt).WifeAge.StringValue = AgeEditDlgController.GetAgeStr(wifeAge);
            }
        }

        baseContext.SetModified();
        string evtName = GKUtils.GetEventName(evt);
        return MCPContent.CreateSimpleContent($"Event '{evtName}' updated to {recName} '{record.XRef}' at index {eventIndex}");
    }
}


// FIXME: too large, causes context exhaustion, not suitable for use in other tools
internal class EventTypeListCommand : EventCommand
{
    public EventTypeListCommand() : base("event_type_list", null, CommandCategory.None) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var recTypes = RuntimeData.RWETypeMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "List all available event types for individuals or families",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_type"] = new MCPToolProperty { Type = "string", Description = "Record type", Enum = recTypes },
                },
                Required = new List<string> { "record_type" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordTypeStr = MCPHelper.GetRequiredStr(args, "record_type");
        if (!RuntimeData.RWETypeMap.TryGetValue(recordTypeStr, out EventTarget eventTarget)) {
            string availableTypes = string.Join(", ", RuntimeData.RecordTypeMap.Keys);
            return MCPContent.CreateSimpleContent($"Unknown record type: '{recordTypeStr}'. Available types: {availableTypes}");
        }

        return GetEventTypesTable(eventTarget);
    }
}
