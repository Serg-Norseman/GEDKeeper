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
    public enum MultimediaColumnType
    {
        ctTitle,
        ctMediaType,
        ctFileRef,
        ctChangeDate
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class MultimediaListColumns : ListColumns
    {
        protected override void InitColumnStatics()
        {
            AddColumn(LSID.LSID_Title, DataType.dtString, 150, true);
            AddColumn(LSID.LSID_Type, DataType.dtString, 85, true);
            AddColumn(LSID.LSID_File, DataType.dtString, 300, true);
            AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public sealed class MultimediaListMan : ListManager
    {
        private GEDCOMMultimediaRecord fRec;

        public MultimediaListMan(GEDCOMTree tree) : base(tree, new MultimediaListColumns())
        {
        }

        public override bool CheckFilter(ShieldState shieldState)
        {
            GEDCOMFileReferenceWithTitle fileRef = fRec.FileReferences[0];

            bool res = (QuickFilter == "*" || IsMatchesMask(fileRef.Title, QuickFilter));

            res = res && CheckCommonFilter();

            return res;
        }

        public override void Fetch(GEDCOMRecord aRec)
        {
            fRec = (aRec as GEDCOMMultimediaRecord);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            GEDCOMFileReferenceWithTitle fileRef = fRec.FileReferences[0];

            object result = null;
            switch ((MultimediaColumnType)colType) {
                case MultimediaColumnType.ctTitle:
                    result = fileRef.Title;
                    break;

                case MultimediaColumnType.ctMediaType:
                    result = LangMan.LS(GKData.MediaTypes[(int)fileRef.MediaType]);
                    break;

                case MultimediaColumnType.ctFileRef:
                    result = fileRef.StringValue;
                    break;

                case MultimediaColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
