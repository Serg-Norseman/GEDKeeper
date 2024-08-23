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
    public sealed class SourceListModel : RecordsListModel<GDMSourceRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctShortTitle,
            ctDate,
            ctAuthor,
            ctTitle,
            //ctPublication,
            ctRepositories,
            ctChangeDate
        }


        public SourceListModel(IBaseContext baseContext) :
            base(baseContext, CreateListColumns(), GDMRecordType.rtSource)
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtSource);

            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.ShortTitle, DataType.dtString, 120, true, true);
            result.AddColumn(LSID.Date, 80, false);
            result.AddColumn(LSID.Author, DataType.dtString, 200, true);
            result.AddColumn(LSID.Title, DataType.dtString, 200, true);
            //result.AddColumn(LSID.Publication, DataType.dtString, 200, true);
            result.AddColumn(LSID.RPRepositories, DataType.dtString, 200, true);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = CheckQuickFilter(fFetchedRec.ShortTitle);

            res = res && CheckCommonFilter() && CheckExternalFilter(fFetchedRec);

            return res;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fFetchedRec.GetId();
                    break;

                case ColumnType.ctShortTitle:
                    result = fFetchedRec.ShortTitle.Trim();
                    break;

                case ColumnType.ctDate:
                    result = new GDMDateItem(fFetchedRec.Date.Value);
                    break;

                case ColumnType.ctAuthor:
                    result = GKUtils.MergeStrings(fFetchedRec.Originator.Lines);
                    break;

                case ColumnType.ctTitle:
                    result = GKUtils.MergeStrings(fFetchedRec.Title.Lines);
                    break;

                //case ColumnType.ctPublication:
                //    result = fFetchedRec.Publication.Lines.Text.Trim();
                //    break;

                case ColumnType.ctRepositories:
                    result = GKUtils.GetSourceRepositories(fBaseContext.Tree, fFetchedRec);
                    break;

                case ColumnType.ctChangeDate:
                    result = fFetchedRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
