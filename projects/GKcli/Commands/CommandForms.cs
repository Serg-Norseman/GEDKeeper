/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.ComponentModel.DataAnnotations;
using GDModel;
using Sharprompt;

namespace GKUI.Commands;

internal class CommandForms
{
    /// <summary>
    /// See <see cref="GKCore.Controllers.EventEditDlgController"/>.
    /// </summary>
    private class EventFormModel
    {
        [Display(Name = "Select event type")]
        [Required]
        [InlineItems("Birth", "Death", "Residence")]
        public string EventType { get; set; }

        [Display(Name = "Event place")]
        public string Place { get; set; }

        [Display(Name = "Type date")]
        public string Date { get; set; }

        [Display(Name = "Are you ready?")]
        public bool? Ready { get; set; }
    }

    public static bool InputEvent(GDMCustomEvent evt)
    {
        var result = Prompt.Bind<EventFormModel>();
        return true;
    }
}
