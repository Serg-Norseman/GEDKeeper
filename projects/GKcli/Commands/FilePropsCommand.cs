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

namespace GKcli.Commands;

internal class FilePropsCommand : BaseCommand
{
    public FilePropsCommand() : base("properties_gedcom", LangMan.LS(LSID.MIFileProperties), CommandCategory.File)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.WriteLine("File properties");

        GDMSubmitterRecord submitter = baseContext.Tree.GetSubmitter();
        CommandController.WriteLine(1, "{0}: [yellow]{1}[/]", LangMan.LS(LSID.Author), submitter.Name);
        CommandController.WriteLine(1, "{0}: [yellow]{1}[/]", LangMan.LS(LSID.Address), submitter.Address.Lines.Text);
        if (submitter.Address.PhoneNumbers.Count > 0) {
            CommandController.WriteLine(1, "{0}: [yellow]{1}[/]", LangMan.LS(LSID.Telephone), submitter.Address.PhoneNumbers[0].StringValue);
        }

        CommandController.WriteLine();
        CommandController.WriteLine(1, LangMan.LS(LSID.MIFileProperties));
        int[] stats = baseContext.Tree.GetRecordStats();
        for (int i = 1; i < stats.Length; i++) {
            CommandController.WriteLine(2, "{0}: [yellow]{1}[/]", LangMan.LS(GKData.RecordTypes[i].Name), stats[i]);
        }
    }
}
