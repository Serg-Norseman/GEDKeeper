/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Locales;

namespace GKUI.Commands;

internal class NoteMenuCommand : BaseCommand
{
    public NoteMenuCommand() : base("notes", LangMan.LS(LSID.RPNotes), CommandCategory.Application)
    {
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.Instance.SelectCommand(CommandCategory.Note, true, "Select a note operation", baseContext);
    }
}
