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
    public sealed class CommunicationListModel : RecordsListModel<GDMCommunicationRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctCommName,
            ctCorresponder,
            ctCommType,
            ctDate,
            ctChangeDate
        }


        public CommunicationListModel(BaseContext baseContext) :
            base(baseContext, CreateListColumns(), GDMRecordType.rtCommunication)
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtCommunication);
            // not to change the order of these lines in their changes
            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.Theme, DataType.dtString, 300, true);
            result.AddColumn(LSID.Corresponder, DataType.dtString, 200, true);
            result.AddColumn(LSID.Type, DataType.dtString, 90, true);
            result.AddColumn(LSID.Date, DataType.dtString, 90, true);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);
            return result;
        }

        protected override string GetQuickFilterBuffer()
        {
            return fFetchedRec.CommName;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fFetchedRec.GetId();
                    break;

                case ColumnType.ctCommName:
                    result = fFetchedRec.CommName;
                    break;

                case ColumnType.ctCorresponder:
                    result = GKUtils.GetCorresponderStr(fBaseContext.Tree, fFetchedRec, false);
                    break;

                case ColumnType.ctCommType:
                    result = LangMan.LS(GKData.CommunicationNames[(int)fFetchedRec.CommunicationType]);
                    break;

                case ColumnType.ctDate:
                    result = GetDateValue(fFetchedRec.Date, isVisible);
                    break;

                case ColumnType.ctChangeDate:
                    result = fFetchedRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
