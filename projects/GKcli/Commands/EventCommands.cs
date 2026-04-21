/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
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

    protected static List<MCPContent> GetEventTypes(EventTarget eventTarget)
    {
        var eventTypes = BaseController.GetEventTypes(eventTarget);

        var rows = new List<string>(eventTypes.Count + 2) {
            "|DisplayName|Tag|Type|HasValue|"
        };
        for (int i = 0; i < eventTypes.Count; i++) {
            var evt = eventTypes[i];
            rows.Add($"|{evt.DisplayName}|{evt.Tag}|{evt.Type}|{evt.HasValue()}|");
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
        string tag = MCPHelper.GetRequiredArgument(args, "tag");
        string argType = MCPHelper.GetStringArgument(args, "type", "");
        EventDef matchedDef = AppHost.EventDefinitions.Find(tag, argType);

        // Determine if this tag corresponds to a fact
        bool isFact = matchedDef != null && matchedDef.HasValue();

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
        newEvent.SetName(tag);
        newEvent.Classification = matchedDef != null ? matchedDef.Type : argType;

        // Date
        string dateStr = MCPHelper.GetStringArgument(args, "date", "");
        if (!string.IsNullOrEmpty(dateStr)) {
            // Prompt is configured to transmit only GEDCOM-compatible strings.
            newEvent.Date.ParseString(dateStr);
        }

        // TODO: Age!

        // Place (string or location xref)
        string placeStr = MCPHelper.GetStringArgument(args, "place", "");
        string locationXRef = MCPHelper.GetStringArgument(args, "location_xref", "");
        if (!string.IsNullOrEmpty(locationXRef)) {
            var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
            if (locRec == null)
                return MCPContent.CreateSimpleContent($"Location not found with XRef: {locationXRef}");
            baseContext.Tree.SetPtrValue(newEvent.Place.Location, locRec);
        } else if (!string.IsNullOrEmpty(placeStr)) {
            newEvent.Place.StringValue = placeStr;
        }

        // Cause
        string cause = MCPHelper.GetStringArgument(args, "cause", "");
        if (!string.IsNullOrEmpty(cause)) {
            newEvent.Cause = cause;
        }

        // Agency
        string agency = MCPHelper.GetStringArgument(args, "agency", "");
        if (!string.IsNullOrEmpty(agency)) {
            newEvent.Agency = agency;
        }

        // Value (for facts)
        if (isFact) {
            string value = MCPHelper.GetStringArgument(args, "value", "");
            newEvent.StringValue = value;
        }

        if (isIndi) {
            string indiAge = MCPHelper.GetStringArgument(args, "age", null);
            if (!string.IsNullOrEmpty(indiAge)) {
                ((GDMIndividualEventDetail)newEvent).Age.StringValue = AgeEditDlgController.GetAgeStr(indiAge);
            }
        } else {
            string husbAge = MCPHelper.GetStringArgument(args, "husband_age", null);
            if (!string.IsNullOrEmpty(husbAge)) {
                ((GDMFamilyEvent)newEvent).HusbandAge.StringValue = AgeEditDlgController.GetAgeStr(husbAge);
            }

            string wifeAge = MCPHelper.GetStringArgument(args, "wife_age", null);
            if (!string.IsNullOrEmpty(wifeAge)) {
                ((GDMFamilyEvent)newEvent).WifeAge.StringValue = AgeEditDlgController.GetAgeStr(wifeAge);
            }
        }

        record.Events.Add(newEvent);
        baseContext.SetModified();

        string evtName = matchedDef != null ? matchedDef.DisplayName : tag;
        return MCPContent.CreateSimpleContent($"Event '{evtName}' added to {recName} '{record.XRef}' at index {record.Events.Count - 1}");
    }
}
