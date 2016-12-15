/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
    public enum FamilyColumnType
    {
        fctFamilyStr,
        fctMarriageDate,
        fctChangeDate
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class FamilyListColumns : ListColumns
    {
        protected override void InitColumnStatics()
        {
            this.AddStatic(LSID.LSID_Spouses, DataType.dtString, 300, true);
            this.AddStatic(LSID.LSID_MarriageDate, DataType.dtString, 100, true);
            this.AddStatic(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
        }

        public FamilyListColumns() : base()
        {
            InitData(typeof(FamilyColumnType));
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class FamilyListMan : ListManager
    {
        private GEDCOMFamilyRecord fRec;

        public override bool CheckFilter(ShieldState shieldState)
        {
            bool res = (GKUtils.IsRecordAccess(this.fRec.Restriction, shieldState)
                        && (this.QuickFilter == "*" || IsMatchesMask(GKUtils.GetFamilyString(this.fRec), this.QuickFilter)));

            res = res && base.CheckCommonFilter();

            return res;
        }

        public override void Fetch(GEDCOMRecord aRec)
        {
            this.fRec = (aRec as GEDCOMFamilyRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            switch (colType) {
                case 0:
                    return GKUtils.GetFamilyString(this.fRec);
                case 1:
                    return GetDateValue(GKUtils.GetMarriageDate(this.fRec), isVisible);
                case 2:
                    return this.fRec.ChangeDate.ChangeDateTime;
                default:
                    return null;
            }
        }

        public FamilyListMan(GEDCOMTree tree) : base(tree, new FamilyListColumns())
        {
        }
    }
}
