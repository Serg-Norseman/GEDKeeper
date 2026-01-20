/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Locales;
using GKCore.Options;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class NoteListModel : RecordsListModel<GDMNoteRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctText,
            ctChangeDate
        }


        private GDMLines fFetchedLines;


        public NoteListModel(BaseContext baseContext) :
            base(baseContext, CreateListColumns(), GDMRecordType.rtNote)
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtNote);
            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.Note, DataType.dtString, 400, true);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);
            return result;
        }

        protected override bool CheckQuickFilter()
        {
            return IsMatchesMask(fFetchedLines, QuickFilter.Value);
        }

        public override void Fetch(GDMNoteRecord aRec)
        {
            base.Fetch(aRec);
            fFetchedLines = fFetchedRec.Lines;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fFetchedRec.GetId();
                    break;

                case ColumnType.ctText:
                    string noteText = GKUtils.MergeStrings(fFetchedLines, GKData.NOTE_NAME_DBL_MAX_LENGTH);
                    result = noteText;
                    break;

                case ColumnType.ctChangeDate:
                    result = fFetchedRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
