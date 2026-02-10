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

namespace GKcli.Commands;

internal class NoteMenuCommand : BaseCommand
{
    public NoteMenuCommand() : base("notes", LangMan.LS(LSID.RPNotes), CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        while (true) {
            var selected = CommandController.Instance.SelectCommand(CommandCategory.Note, true, "Select a note operation");
            if (selected == CommandController.CMD_RETURN) break;
        }
    }
}


internal class NoteListCommand : BaseCommand
{
    public NoteListCommand() : base("list_notes", LangMan.LS(LSID.Find), CommandCategory.Note) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var selected = CommandController.SelectRecord(baseContext, GDMRecordType.rtNote, "Select a note", "Note: {0}", "No records.");
        if (selected != null)
            CommandController.SetVariable("selectedObj", selected);
    }
}


internal class NoteAddCommand : BaseCommand
{
    public NoteAddCommand() : base("add_note", LangMan.LS(LSID.MIRecordAdd), CommandCategory.Note) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var text = Prompt.Input<string>("Enter the note's text");
        var noteRec = baseContext.Tree.CreateNote();
        noteRec.Lines.Text = text;
        baseContext.SetModified();
        CommandController.WriteLine("Note: {0}", text);
    }
}


internal class NoteEditCommand : BaseCommand
{
    public NoteEditCommand() : base("edit_note", LangMan.LS(LSID.MIRecordEdit), CommandCategory.Note) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var noteRec = CommandController.GetVariable<GDMNoteRecord>("selectedObj");
        if (noteRec == null) {
            CommandController.WriteLine("Error: Expected an note record");
            return;
        }

        var text = Prompt.Input<string>("Enter the note's text", defaultValue: noteRec.Lines.Text);
        noteRec.Lines.Text = text;
        baseContext.SetModified();
        CommandController.WriteLine("Note: {0}", text);
    }
}


internal class NoteDeleteCommand : BaseCommand
{
    public NoteDeleteCommand() : base("delete_note", LangMan.LS(LSID.MIRecordDelete), CommandCategory.Note) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var noteRec = CommandController.GetVariable<GDMNoteRecord>("selectedObj");
        if (noteRec == null) {
            CommandController.WriteLine("Error: Expected an note record");
            return;
        }

        baseContext.DeleteRecord(noteRec);
    }
}
