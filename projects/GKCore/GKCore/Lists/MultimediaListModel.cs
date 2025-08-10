/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GKCore.Charts;
using GKCore.Design.Graphics;
using GKCore.Locales;
using GKCore.Media;
using GKCore.Options;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class MultimediaListModel : RecordsListModel<GDMMultimediaRecord>
    {
        public enum ColumnType
        {
            ctXRefNum,
            ctTitle,
            ctMediaType,
            ctFileRef,
            ctChangeDate
        }


        private GDMFileReferenceWithTitle fFileRef;


        public MultimediaListModel(IBaseContext baseContext) :
            base(baseContext, CreateListColumns(), GDMRecordType.rtMultimedia)
        {
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.rtMultimedia);
            result.AddColumn(LSID.NumberSym, DataType.dtInteger, 50, true);
            result.AddColumn(LSID.Title, DataType.dtString, 150, true);
            result.AddColumn(LSID.Type, DataType.dtString, 85, true);
            result.AddColumn(LSID.File, DataType.dtString, 300, true);
            result.AddColumn(LSID.Changed, DataType.dtDateTime, 150, true);
            return result;
        }

        public override void Fetch(GDMMultimediaRecord aRec)
        {
            base.Fetch(aRec);
            fFileRef = fFetchedRec.FileReferences[0];
            fQuickFilterBuffer = fFileRef.Title;
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch ((ColumnType)colType) {
                case ColumnType.ctXRefNum:
                    result = fFetchedRec.GetId();
                    break;

                case ColumnType.ctTitle:
                    result = fFileRef.Title;
                    break;

                case ColumnType.ctMediaType:
                    result = LangMan.LS(GKData.MediaTypes[(int)fFileRef.MediaType]);
                    break;

                case ColumnType.ctFileRef:
                    result = fFileRef.StringValue;
                    break;

                case ColumnType.ctChangeDate:
                    result = fFetchedRec.ChangeDate.ChangeDateTime;
                    break;
            }
            return result;
        }

        public override IColor GetBackgroundColor(int itemIndex, object rowData)
        {
            GlobalOptions gOptions = GlobalOptions.Instance;
            if (gOptions.HighlightInaccessibleFiles && fBaseContext.VerifyMediaFile(fFileRef.StringValue, out _) != MediaStoreStatus.mssExists) {
                return ChartRenderer.GetColor(GKData.HighlightInaccessibleFiles);
            } else {
                return base.GetBackgroundColor(itemIndex, rowData);
            }
        }
    }
}
