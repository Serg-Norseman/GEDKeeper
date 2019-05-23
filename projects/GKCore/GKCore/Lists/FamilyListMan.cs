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

using GDModel;
using GKCore.Interfaces;

namespace GKCore.Lists
{
    public enum FamilyColumnType
    {
        ctFamilyStr,
        ctMarriageDate,
        ctChangeDate
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class FamilyListMan : ListManager
    {
        private GDMFamilyRecord fRec;


        public FamilyListMan(IBaseContext baseContext) :
            base(baseContext, CreateFamilyListColumns(), GDMRecordType.rtFamily)
        {
        }

        public static ListColumns CreateFamilyListColumns()
        {
            var result = new ListColumns();

            result.AddColumn(LSID.LSID_Spouses, DataType.dtString, 300, true, true);
            result.AddColumn(LSID.LSID_MarriageDate, DataType.dtString, 100, true);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = (fBaseContext.IsRecordAccess(fRec.Restriction)
                        && (QuickFilter == "*" || IsMatchesMask(GKUtils.GetFamilyString(fRec), QuickFilter)));

            res = res && CheckCommonFilter() && CheckExternalFilter(fRec);

            return res;
        }

        public override void Fetch(GDMRecord aRec)
        {
            fRec = (aRec as GDMFamilyRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((FamilyColumnType)colType)
            {
                case FamilyColumnType.ctFamilyStr:
                    result = GKUtils.GetFamilyString(fRec);
                    break;

                case FamilyColumnType.ctMarriageDate:
                    result = GetDateValue(GKUtils.GetMarriageDate(fRec), isVisible);
                    break;

                case FamilyColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
