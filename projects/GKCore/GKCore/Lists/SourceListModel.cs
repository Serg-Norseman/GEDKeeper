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


        public SourceListModel(BaseContext baseContext) :
            base(baseContext, CreateListColumns(), GDMRecordType.rtSource)
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtSource);
            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.ShortTitle, DataType.dtString, 120, true);
            result.AddColumn(LSID.Date, 80, false);
            result.AddColumn(LSID.Author, DataType.dtString, 200, true);
            result.AddColumn(LSID.Title, DataType.dtString, 200, true);
            //result.AddColumn(LSID.Publication, DataType.dtString, 200, true);
            result.AddColumn(LSID.RPRepositories, DataType.dtString, 200, true);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);
            return result;
        }

        protected override string GetQuickFilterBuffer()
        {
            return fFetchedRec.ShortTitle;
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
