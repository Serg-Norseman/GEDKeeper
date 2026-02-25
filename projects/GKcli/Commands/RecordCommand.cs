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
using GKUI.Platform;
using Sharprompt;

namespace GKcli.Commands;

internal abstract class RecordCommand : BaseCommand
{
    public RecordCommand(string sign, Enum lsid, CommandCategory category) : base(sign, lsid, category) { }

    protected static GDMRecord SelectRecord(BaseContext baseContext, GDMRecordType recordType, string prompt, string yesMsg, string noMsg)
    {
        GDMRecord result = null;

        var recList = baseContext.Tree.GetRecords(recordType);
        if (recList.Count > 0) {
            result = Prompt.Select(prompt, recList,
                pageSize: 10,
                textSelector: (GDMRecord r) => { return GKUtils.GetRecordName(baseContext.Tree, r, false); });

            PromptHelper.WriteLine(string.Format(yesMsg, GKUtils.GetRecordName(baseContext.Tree, result, false)));
        } else {
            PromptHelper.WriteLine(noMsg);
        }

        return result;
    }

    protected static void DeleteRecord<T>(BaseContext baseContext, string expectedMsg) where T : GDMRecord
    {
        var rec = CommandController.GetVariable<T>("selectedObj");
        if (rec == null) {
            PromptHelper.WriteLine(expectedMsg);
            return;
        }

        bool result = CommandController.GetConfirm("Подтвердите удаление");
        if (result) {
            baseContext.DeleteRecord(rec);
            CommandController.SetVariable("selectedObj", null);
        }
    }
}
