/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
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
