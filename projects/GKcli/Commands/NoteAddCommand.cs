/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Locales;
using Sharprompt;

namespace GKcli.Commands;

internal class NoteAddCommand : BaseCommand
{
    public NoteAddCommand() : base("add_note", LangMan.LS(LSID.MIRecordAdd), CommandCategory.Note)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var text = Prompt.Input<string>("Enter the note's text");
        CommandController.WriteLine("Note: {0}", text);
    }
}
