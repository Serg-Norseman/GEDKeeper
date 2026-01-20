/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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


        public MultimediaListModel(BaseContext baseContext) :
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

        protected override string GetQuickFilterBuffer()
        {
            return fFileRef.Title;
        }

        public override void Fetch(GDMMultimediaRecord aRec)
        {
            base.Fetch(aRec);
            fFileRef = fFetchedRec.FileReferences[0];
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
