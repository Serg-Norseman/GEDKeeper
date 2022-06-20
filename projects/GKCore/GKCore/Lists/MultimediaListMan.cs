/*
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
    public sealed class MultimediaListMan : ListManager<GDMMultimediaRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctTitle,
            ctMediaType,
            ctFileRef,
            ctChangeDate
        }


        private GDMMultimediaRecord fRec;


        public MultimediaListMan(IBaseContext baseContext) :
            base(baseContext, CreateMultimediaListColumns(), GDMRecordType.rtMultimedia)
        {
        }

        public static ListColumns<GDMMultimediaRecord> CreateMultimediaListColumns()
        {
            var result = new ListColumns<GDMMultimediaRecord>();

            result.AddColumn(LSID.LSID_NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.LSID_Title, DataType.dtString, 150, true, true);
            result.AddColumn(LSID.LSID_Type, DataType.dtString, 85, true);
            result.AddColumn(LSID.LSID_File, DataType.dtString, 300, true);
            result.AddColumn(LSID.LSID_Changed, DataType.dtDateTime, 150, true);

            result.ResetDefaults();
            return result;
        }

        public override bool CheckFilter()
        {
            GDMFileReferenceWithTitle fileRef = fRec.FileReferences[0];

            bool res = IsMatchesMask(fileRef.Title, QuickFilter);

            res = res && CheckCommonFilter() && CheckExternalFilter(fRec);

            return res;
        }

        public override void Fetch(GDMRecord aRec)
        {
            fRec = (GDMMultimediaRecord)aRec;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            GDMFileReferenceWithTitle fileRef = fRec.FileReferences[0];
            if (fileRef == null) {
                return null;
            }

            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fRec.GetId();
                    break;

                case ColumnType.ctTitle:
                    result = fileRef.Title;
                    break;

                case ColumnType.ctMediaType:
                    result = LangMan.LS(GKData.MediaTypes[(int)fileRef.MediaType]);
                    break;

                case ColumnType.ctFileRef:
                    result = fileRef.StringValue;
                    break;

                case ColumnType.ctChangeDate:
                    result = fRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }
    }
}
