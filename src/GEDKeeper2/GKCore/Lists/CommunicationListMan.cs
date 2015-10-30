using System;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Lists
{
	/// <summary>
	/// 
	/// </summary>
	public enum CommunicationColumnType
	{
		cctCommName,
		cctCorresponder,
		cctCommType,
		cctDate,		
		cctChangeDate
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class CommunicationListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LSID.LSID_Theme, TDataType.dtString, 300, true);
			this.AddStatic(LSID.LSID_Corresponder, TDataType.dtString, 200, true);
			this.AddStatic(LSID.LSID_Type, TDataType.dtString, 90, true);
			this.AddStatic(LSID.LSID_Date, TDataType.dtString, 90, true);
			this.AddStatic(LSID.LSID_Changed, TDataType.dtDateTime, 150, true);
		}

		public CommunicationListColumns() : base()
		{
			InitData(typeof(CommunicationColumnType));
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class CommunicationListMan : ListManager
	{
		private GEDCOMCommunicationRecord fRec;

		public override bool CheckFilter(ShieldState shieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.fRec.CommName, this.QuickFilter));

			res = res && base.CheckCommonFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMCommunicationRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
		{
			switch (colType) {
				case 0:
					return this.fRec.CommName;
				case 1:
					return GKUtils.GetCorresponderStr(this.fTree, this.fRec, false);
				case 2:
					return LangMan.LS(GKData.CommunicationNames[(int)this.fRec.CommunicationType]);
				case 3:
					return GetDateValue(this.fRec.Date, isVisible);
				case 4:
					return this.fRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(CommunicationColumnType);
		}

		protected override ListColumns GetDefaultListColumns()
		{
			return new CommunicationListColumns();
		}

		public CommunicationListMan(GEDCOMTree tree) : base(tree)
		{
		}
	}
}
