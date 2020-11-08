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
    public enum RepositoryColumnType
    {
        ctName,
        ctChangeDate
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class RepositoryListMan : ListManager
    {
        private GDMRepositoryRecord fRec;


        public RepositoryListMan(IBaseContext baseContext) :
            base(baseContext, CreateRepositoryListColumns(), GDMRecordType.rtRepository)
        {
        }

        public static ListColumns CreateRepositoryListColumns()
        {
            var result = new ListColumns();

            result.AddColumn(LSID.LSID_Repository, DataType.dtString, 400, true, true);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            bool res = IsMatchesMask(fRec.RepositoryName, QuickFilter);

            res = res && CheckCommonFilter() && CheckExternalFilter(fRec);

            return res;
        }

        public override void Fetch(GDMRecord aRec)
        {
            fRec = (aRec as GDMRepositoryRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((RepositoryColumnType)colType)
            {
                case RepositoryColumnType.ctName:
                    result = fRec.RepositoryName;
                    break;

                case RepositoryColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
