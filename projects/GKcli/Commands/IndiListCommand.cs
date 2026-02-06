/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore;
using GKCore.Locales;
using Sharprompt;

namespace GKUI.Commands;

internal class IndiListCommand : BaseCommand
{
    public IndiListCommand() : base("list_individuals", LangMan.LS(LSID.Find), CommandCategory.Individual)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var selected = CommandController.SelectRecord(baseContext, GDMRecordType.rtIndividual, "Select a individual", "Individual: {0}", "No records.");
        if (selected != null)
            IndiChange(selected as GDMIndividualRecord);
    }

    private static void IndiChange(GDMIndividualRecord iRec)
    {
        // defaultValue
        var continueFlag = Prompt.Input<bool>("Continue editing? [true/false]");
        if (!continueFlag) return;

        var newEvent = new GDMIndividualEvent();
        CommandForms.InputEvent(newEvent);
    }
}
