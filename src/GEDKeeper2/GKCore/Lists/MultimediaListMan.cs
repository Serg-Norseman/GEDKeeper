using System;
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
		mctTitle,
		mctMediaType,
		mctFileRef,
		mctChangeDate
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class MultimediaListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LSID.LSID_Title, DataType.dtString, 150, true);
			this.AddStatic(LSID.LSID_Type, DataType.dtString, 85, true);
			this.AddStatic(LSID.LSID_File, DataType.dtString, 300, true);
			this.AddStatic(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
		}

		public MultimediaListColumns() : base()
		{
			InitData(typeof(MultimediaColumnType));
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class MultimediaListMan : ListManager
	{
		private GEDCOMMultimediaRecord fRec;

		public override bool CheckFilter(ShieldState shieldState)
		{
			GEDCOMFileReferenceWithTitle fileRef = this.fRec.FileReferences[0];

			bool res = (this.QuickFilter == "*" || IsMatchesMask(fileRef.Title, this.QuickFilter));

			res = res && base.CheckCommonFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMMultimediaRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
		{
			GEDCOMFileReferenceWithTitle fileRef = this.fRec.FileReferences[0];

			switch (colType) {
				case 0:
					return fileRef.Title;
				case 1:
					return LangMan.LS(GKData.MediaTypes[(int)fileRef.MediaType]);
				case 2:
					return fileRef.StringValue;
				case 3:
					return this.fRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(MultimediaColumnType);
		}

		protected override ListColumns GetDefaultListColumns()
		{
			return new MultimediaListColumns();
		}

		public MultimediaListMan(GEDCOMTree tree) : base(tree)
		{
		}
	}
}
