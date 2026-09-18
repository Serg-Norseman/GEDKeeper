/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Design.Graphics;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Sync;
using GKCore.Utilities;

namespace GKTreeSyncPlugin;

internal sealed class DiffRecordsModel : SimpleListModel<DiffRecord>
{
    public bool ShowOnlyModified { get; set; }


    public DiffRecordsModel(BaseContext baseContext) :
        base(baseContext, CreateListColumns())
    {
    }

    public static ListColumns CreateListColumns()
    {
        var result = new ListColumns(GKListType.ltNone);
        result.AddColumn("Sync", DataType.dtBool, 40, true);
        result.AddColumn("XRef 1", DataType.dtString, 100, true);
        result.AddColumn("XRef 2", DataType.dtString, 100, true);
        result.AddColumn("Name 1", DataType.dtString, 400, true);
        result.AddColumn("Name 2", DataType.dtString, 400, true);
        return result;
    }

    public override bool CheckFilter()
    {
        bool res = (!ShowOnlyModified || fFetchedRec.Status != DiffStatus.Equal);
        return res;
    }

    // fetched data
    private string diffChar, item1, item2;

    public override void Fetch(DiffRecord aRec)
    {
        base.Fetch(aRec);

        diffChar = DiffUtil.GetStatusChar(fFetchedRec.Status);
        switch (fFetchedRec.Status) {
            case DiffStatus.Equal:
            default:
                item1 = diffChar + " " + fFetchedRec.Obj1.XRef;
                item2 = diffChar + " " + fFetchedRec.Obj2.XRef;
                break;

            case DiffStatus.Deleted:
                item1 = diffChar + " " + fFetchedRec.Obj1.XRef;
                item2 = " ";
                break;

            case DiffStatus.Inserted:
                item1 = " ";
                item2 = diffChar + " " + fFetchedRec.Obj2.XRef;
                break;

            case DiffStatus.Modified:
            case DiffStatus.DeepModified:
                item1 = diffChar + " " + fFetchedRec.Obj1.XRef;
                item2 = diffChar + " " + fFetchedRec.Obj2.XRef;
                break;
        }
    }

    protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
    {
        object result = null;
        switch (colType) {
            case 0:
                result = fFetchedRec.Checked;
                break;
            case 1:
                result = item1;
                break;
            case 2:
                result = item2;
                break;
            case 3:
                result = GKUtils.GetRecordName(fBaseContext.Tree, fFetchedRec.Obj1, false);
                break;
            case 4:
                result = GKUtils.GetRecordName(fBaseContext.Tree, fFetchedRec.Obj2, false);
                break;
        }
        return result;
    }

    public override IColor GetBackgroundColor(int itemIndex, object rowData)
    {
        DiffRecord diffRecord = rowData as DiffRecord;
        return Plugin.GetDiffColor(diffRecord.Status);
    }

    protected override void SetColumnValueEx(DiffRecord item, int colIndex, object value)
    {
        if (item != null && colIndex == 0 && value is bool chk)
            item.Checked = chk;
    }
}
