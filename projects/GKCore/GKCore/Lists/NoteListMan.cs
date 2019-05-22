/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKCore.Lists
{
    public enum NoteColumnType
    {
        ctText,
        ctChangeDate
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class NoteListMan : ListManager
    {
        private GDMNoteRecord fRec;


        public NoteListMan(IBaseContext baseContext) :
            base(baseContext, CreateNoteListColumns(), GEDCOMRecordType.rtNote)
        {
        }

        public static ListColumns CreateNoteListColumns()
        {
            var result = new ListColumns();

            result.AddColumn(LSID.LSID_Note, DataType.dtString, 400, true);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = (QuickFilter == "*" || IsMatchesMask(fRec.Note.Text, QuickFilter));

            res = res && CheckCommonFilter() && CheckExternalFilter(fRec);

            return res;
        }

        public override void Fetch(GDMRecord aRec)
        {
            fRec = (aRec as GDMNoteRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((NoteColumnType)colType)
            {
                case NoteColumnType.ctText:
                    string noteText = GKUtils.MergeStrings(fRec.Note);
                    //string noteText = GKUtils.TruncateStrings(fRec.Note, GKData.NOTE_NAME_MAX_LENGTH);
                    result = noteText;
                    break;

                case NoteColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
