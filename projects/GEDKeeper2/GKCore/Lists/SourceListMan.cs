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
    public enum SourceColumnType
    {
        ctShortName,
        ctAuthor,
        ctTitle,
        ctChangeDate
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceListColumns : ListColumns
    {
        protected override void InitColumnStatics()
        {
            AddColumn(LSID.LSID_ShortTitle, DataType.dtString, 120, true);
            AddColumn(LSID.LSID_Author, DataType.dtString, 200, true);
            AddColumn(LSID.LSID_Title, DataType.dtString, 200, true);
            AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class SourceListMan : ListManager
    {
        private GEDCOMSourceRecord fRec;

        public SourceListMan(GEDCOMTree tree) : base(tree, new SourceListColumns())
        {
        }

        public override bool CheckFilter(ShieldState shieldState)
        {
            bool res = (QuickFilter == "*" || IsMatchesMask(fRec.FiledByEntry, QuickFilter));

            res = res && CheckCommonFilter();

            return res;
        }

        public override void Fetch(GEDCOMRecord aRec)
        {
            fRec = (aRec as GEDCOMSourceRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((SourceColumnType)colType)
            {
                case SourceColumnType.ctShortName:
                    result = fRec.FiledByEntry.Trim();
                    break;

                case SourceColumnType.ctAuthor:
                    result = fRec.Originator.Text.Trim();
                    break;

                case SourceColumnType.ctTitle:
                    result = fRec.Title.Text.Trim();
                    break;

                case SourceColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
