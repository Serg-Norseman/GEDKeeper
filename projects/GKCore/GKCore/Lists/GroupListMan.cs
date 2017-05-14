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
    /// <summary>
    /// 
    /// </summary>
    public enum GroupColumnType
    {
        ctName,
        ctChangeDate
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class GroupListColumns : ListColumns
    {
        protected override void InitColumnStatics()
        {
            AddColumn(LSID.LSID_Group, DataType.dtString, 400, true);
            AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class GroupListMan : ListManager
    {
        private GEDCOMGroupRecord fRec;

        public GroupListMan(IBaseContext baseContext) : 
            base(baseContext, new GroupListColumns(), GEDCOMRecordType.rtGroup)
        {
        }

        public override bool CheckFilter()
        {
            bool res = (QuickFilter == "*" || IsMatchesMask(fRec.GroupName, QuickFilter));

            res = res && CheckCommonFilter();

            return res;
        }

        public override void Fetch(GEDCOMRecord aRec)
        {
            fRec = (aRec as GEDCOMGroupRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((GroupColumnType)colType)
            {
                case GroupColumnType.ctName:
                    result = fRec.GroupName;
                    break;

                case GroupColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
