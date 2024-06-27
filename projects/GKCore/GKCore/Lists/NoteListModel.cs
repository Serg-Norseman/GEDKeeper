/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using GDModel;
using GKCore.Interfaces;
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


        public NoteListModel(IBaseContext baseContext) :
            base(baseContext, CreateListColumns(), GDMRecordType.rtNote)
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtNote);

            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.Note, DataType.dtString, 400, true);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = IsMatchesMask(fFetchedLines, QuickFilter.Value);

            res = res && CheckCommonFilter() && CheckExternalFilter(fFetchedRec);

            return res;
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
