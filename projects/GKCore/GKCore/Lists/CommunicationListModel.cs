﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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


        public CommunicationListModel(IBaseContext baseContext) :
            base(baseContext, CreateCommunicationListColumns(), GDMRecordType.rtCommunication)
        {
        }

        public static ListColumns<GDMCommunicationRecord> CreateCommunicationListColumns()
        {
            var result = new ListColumns<GDMCommunicationRecord>();

            // not to change the order of these lines in their changes
            result.AddColumn(LSID.LSID_NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.LSID_Theme, DataType.dtString, 300, true, true);
            result.AddColumn(LSID.LSID_Corresponder, DataType.dtString, 200, true);
            result.AddColumn(LSID.LSID_Type, DataType.dtString, 90, true);
            result.AddColumn(LSID.LSID_Date, DataType.dtString, 90, true);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = IsMatchesMask(fFetchedRec.CommName, QuickFilter);

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
