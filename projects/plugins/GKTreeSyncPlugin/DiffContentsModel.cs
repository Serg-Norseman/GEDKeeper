/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Lists;
using GKCore.Options;
using GKCore.Sync;
using GKCore.Utilities;

namespace GKTreeSyncPlugin;

internal delegate string TagContentHandler(GDMTree tree, GDMTag tag);

internal sealed class DiffContentsModel : SimpleListModel<DiffTag>
{
    private readonly TagContentHandler fTagContentHandler;

    public bool ShowOnlyModified { get; set; }


    public DiffContentsModel(BaseContext baseContext, TagContentHandler tagContentHandler) :
        base(baseContext, CreateListColumns())
    {
        fTagContentHandler = tagContentHandler;
    }

    public static ListColumns CreateListColumns()
    {
        var result = new ListColumns(GKListType.ltNone);
        result.AddColumn("Sync", DataType.dtBool, 40, true);
        result.AddColumn("#", DataType.dtInteger, 40, true);
        result.AddColumn("Content 1", DataType.dtString, 400, true);
        result.AddColumn("Content 2", DataType.dtString, 400, true);
        return result;
    }

    public override bool CheckFilter()
    {
        bool res = (!ShowOnlyModified || fFetchedRec.Status != DiffStatus.Equal);
        return res;
    }

    // fetched data
    private string diffChar, prefix1, prefix2;

    public override void Fetch(DiffTag aRec)
    {
        base.Fetch(aRec);

        diffChar = DiffUtil.GetStatusChar(fFetchedRec.Status);
        switch (fFetchedRec.Status) {
            case DiffStatus.Equal:
            default:
                prefix1 = diffChar + " ";
                prefix2 = diffChar + " ";
                break;

            case DiffStatus.Deleted:
                prefix1 = diffChar + " ";
                prefix2 = " ";
                break;

            case DiffStatus.Inserted:
                prefix1 = " ";
                prefix2 = diffChar + " ";
                break;

            case DiffStatus.Modified:
            case DiffStatus.DeepModified:
                prefix1 = diffChar + " ";
                prefix2 = diffChar + " ";
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
                result = fFetchedRec.Num;
                break;
            case 2:
                result = prefix1 + fTagContentHandler(fBaseContext.Tree, fFetchedRec.Obj1);
                break;
            case 3:
                result = prefix2 + fTagContentHandler(fBaseContext.Tree, fFetchedRec.Obj2);
                break;
        }
        return result;
    }

    public override IColor GetBackgroundColor(int itemIndex, object rowData)
    {
        return Plugin.GetDiffColor(((DiffTag)rowData).Status);
    }

    protected override void SetColumnValueEx(DiffTag item, int colIndex, object value)
    {
        if (item != null && colIndex == 0 && value is bool chk)
            item.Checked = chk;
    }
}
