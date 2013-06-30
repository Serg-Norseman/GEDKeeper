using System;

using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public enum TMultimediaColumnType : byte
	{
		mctTitle,
		mctMediaType,
		mctFileRef,
		mctChangeDate
	}

	public sealed class TMultimediaListColumns : TListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LangMan.LSList[125], TDataType.dtString, 150, true);
			this.AddStatic(LangMan.LSList[113], TDataType.dtString, 85, true);
			this.AddStatic(LangMan.LSList[147], TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LSList[317], TDataType.dtDateTime, 150, true);
		}

		public TMultimediaListColumns() : base()
		{
			InitData(typeof(TMultimediaColumnType));
		}
	}

	public sealed class TMultimediaListMan : TListManager
	{
		private TGEDCOMMultimediaRecord FRec;

		public override bool CheckFilter(TGenEngine.TShieldState aShieldState)
		{
			TGEDCOMFileReferenceWithTitle file_ref = this.FRec.FileReferences[0];

			bool res = (this.QuickFilter == "*" || IsMatchesMask(file_ref.Title, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMMultimediaRecord);
		}

		protected override object GetColumnValueEx(int col_type, int col_subtype)
		{
			TGEDCOMFileReferenceWithTitle file_ref = this.FRec.FileReferences[0];

			switch (col_type) {
				case 0:
					return file_ref.Title;
				case 1:
					return LangMan.LSList[(int)TGenEngine.MediaTypes[(int)file_ref.MediaType] - 1];
				case 2:
					return file_ref.StringValue;
				case 3:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TMultimediaColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return new TMultimediaListColumns();
		}

		public TMultimediaListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
