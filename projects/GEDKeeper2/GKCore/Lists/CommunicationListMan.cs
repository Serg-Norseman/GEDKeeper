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
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public enum CommunicationColumnType
    {
        ctCommName,
        ctCorresponder,
        ctCommType,
        ctDate,
        ctChangeDate
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class CommunicationListColumns : ListColumns
    {
        protected override void InitColumnStatics()
        {
            // not to change the order of these lines in their changes
            AddColumn(LSID.LSID_Theme, DataType.dtString, 300, true);
            AddColumn(LSID.LSID_Corresponder, DataType.dtString, 200, true);
            AddColumn(LSID.LSID_Type, DataType.dtString, 90, true);
            AddColumn(LSID.LSID_Date, DataType.dtString, 90, true);
            AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class CommunicationListMan : ListManager
    {
        private GEDCOMCommunicationRecord fRec;

        public CommunicationListMan(GEDCOMTree tree) : base(tree, new CommunicationListColumns())
        {
        }

        public override bool CheckFilter(ShieldState shieldState)
        {
            bool res = (QuickFilter == "*" || IsMatchesMask(fRec.CommName, QuickFilter));

            res = res && CheckCommonFilter();

            return res;
        }

        public override void Fetch(GEDCOMRecord aRec)
        {
            fRec = (aRec as GEDCOMCommunicationRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((CommunicationColumnType)colType)
            {
                case CommunicationColumnType.ctCommName:
                    result = fRec.CommName;
                    break;

                case CommunicationColumnType.ctCorresponder:
                    result = GKUtils.GetCorresponderStr(fTree, fRec, false);
                    break;

                case CommunicationColumnType.ctCommType:
                    result = LangMan.LS(GKData.CommunicationNames[(int)fRec.CommunicationType]);
                    break;

                case CommunicationColumnType.ctDate:
                    result = GetDateValue(fRec.Date, isVisible);
                    break;

                case CommunicationColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
