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
    public sealed class MultimediaListMan : ListManager
    {
        private GEDCOMMultimediaRecord fRec;


        public MultimediaListMan(IBaseContext baseContext) :
            base(baseContext, CreateMultimediaListColumns(), GEDCOMRecordType.rtMultimedia)
        {
        }

        public static ListColumns CreateMultimediaListColumns()
        {
            var result = new ListColumns();

            result.AddColumn(LSID.LSID_Title, DataType.dtString, 150, true, true);
            result.AddColumn(LSID.LSID_Type, DataType.dtString, 85, true);
            result.AddColumn(LSID.LSID_File, DataType.dtString, 300, true);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            GEDCOMFileReferenceWithTitle fileRef = fRec.FileReferences[0];

            bool res = (QuickFilter == "*" || IsMatchesMask(fileRef.Title, QuickFilter));

            res = res && CheckCommonFilter() && CheckExternalFilter(fRec);

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
