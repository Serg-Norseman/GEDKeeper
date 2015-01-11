using System;

using GedCom551;
using GKCore;
using GKCore.Interfaces;

/// <summary>
/// 
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
			this.AddStatic(LangMan.LS(LSID.LSID_Title), TDataType.dtString, 150, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Type), TDataType.dtString, 85, true);
			this.AddStatic(LangMan.LS(LSID.LSID_File), TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
		}

		public TMultimediaListColumns() : base()
		{
			InitData(typeof(TMultimediaColumnType));
		}
	}

	public sealed class TMultimediaListMan : TListManager
	{
		private TGEDCOMMultimediaRecord FRec;

		public override bool CheckFilter(ShieldState aShieldState)
		{
			TGEDCOMFileReferenceWithTitle fileRef = this.FRec.FileReferences[0];

			bool res = (this.QuickFilter == "*" || IsMatchesMask(fileRef.Title, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMMultimediaRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			TGEDCOMFileReferenceWithTitle fileRef = this.FRec.FileReferences[0];

			switch (colType) {
				case 0:
					return fileRef.Title;
				case 1:
					return LangMan.LS(GKData.MediaTypes[(int)fileRef.MediaType]);
				case 2:
					return fileRef.StringValue;
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
