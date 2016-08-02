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

using System;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public enum SourceColumnType
    {
        sctShortName,
        sctAuthor,
        sctTitle,
        sctChangeDate
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceListColumns : ListColumns
    {
        protected override void InitColumnStatics()
        {
            this.AddStatic(LSID.LSID_ShortTitle, DataType.dtString, 120, true);
            this.AddStatic(LSID.LSID_Author, DataType.dtString, 200, true);
            this.AddStatic(LSID.LSID_Title, DataType.dtString, 200, true);
            this.AddStatic(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
        }

        public SourceListColumns() : base()
        {
            InitData(typeof(SourceColumnType));
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceListMan : ListManager
    {
        private GEDCOMSourceRecord fRec;

        public override bool CheckFilter(ShieldState shieldState)
        {
            bool res = (this.QuickFilter == "*" || IsMatchesMask(this.fRec.FiledByEntry, this.QuickFilter));

            res = res && base.CheckCommonFilter();

            return res;
        }

        public override void Fetch(GEDCOMRecord aRec)
        {
            this.fRec = (aRec as GEDCOMSourceRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            switch (colType) {
                case 0:
                    return this.fRec.FiledByEntry.Trim();
                case 1:
                    return this.fRec.Originator.Text.Trim();
                case 2:
                    return this.fRec.Title.Text.Trim();
                case 3:
                    return this.fRec.ChangeDate.ChangeDateTime;
                default:
                    return null;
            }
        }

        public SourceListMan(GEDCOMTree tree) : base(tree, new SourceListColumns())
        {
        }
    }
}
