/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.ComponentModel.DataAnnotations;
using GDModel;
using GKCore;
using GKCore.Locales;
using GKUI.Platform;
using Sharprompt;

namespace GKcli.Commands;

internal class IndiMenuCommand : BaseCommand
{
    public IndiMenuCommand() : base("individuals", LSID.RPIndividuals, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Individual, true, "Select a individual operation");
    }
}


internal class IndiListCommand : BaseCommand
{
    public IndiListCommand() : base("individual_list", LSID.Find, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var selected = PromptHelper.SelectRecord(baseContext, GDMRecordType.rtIndividual, "Select a individual", "Individual: {0}", "No records.");
        if (selected != null) {
            var newEvent = new GDMIndividualEvent();
            CommandController.SetVariable("selectedObj", newEvent);

            CommandController.SelectCommand(CommandCategory.Events, true, "Select an event operation");
        }
    }
}


internal class IndiAddCommand : BaseCommand
{
    public IndiAddCommand() : base("individual_add", LSID.MIRecordAdd, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var name = Prompt.Input<string>("Enter the individual's name [first_name /last_name/]");
        var sex = Prompt.Input<char>("Enter the individual's sex [m/f]", validators: [Validators.Required(), SexVal()]);
        var indiRec = baseContext.Tree.CreateIndividual();
        var persName = indiRec.AddPersonalName(new GDMPersonalName());
        persName.ParseString(name);
        indiRec.Sex = (sex == 'm') ? GDMSex.svMale : GDMSex.svFemale;
        PromptHelper.WriteLine("Individual: {0}", GKUtils.GetNameString(indiRec, false));
    }

    private static Func<object, ValidationResult> SexVal(string errorMessage = null)
    {
        return delegate (object input) {
            if (!(input is char sym)) {
                return ValidationResult.Success;
            }

            sym = char.ToLowerInvariant(sym);
            return (sym == 'm' || sym == 'f') ? ValidationResult.Success : new ValidationResult(errorMessage ?? "");
        };
    }
}


internal class IndiEditCommand : BaseCommand
{
    public IndiEditCommand() : base("individual_edit", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiSearchCommand : BaseCommand
{
    public IndiSearchCommand() : base("individual_search", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiDeleteCommand : BaseCommand
{
    public IndiDeleteCommand() : base("individual_delete", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiListSpousesCommand : BaseCommand
{
    public IndiListSpousesCommand() : base("individual_list_spouses", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiListGroupsCommand : BaseCommand
{
    public IndiListGroupsCommand() : base("individual_list_groups", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
