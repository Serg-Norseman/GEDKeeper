using System;
using System.Globalization;

using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public enum TLocationColumnType : byte
	{
		lctName,
		lctLati,
		lctLong,
		lctChangeDate
	}

	public sealed class TLocationListColumns : TListColumns
	{
		protected override void InitColumnStatics()
		{
			NumberFormatInfo nfi = new NumberFormatInfo();
			nfi.NumberDecimalSeparator = ".";

			this.AddStatic(LangMan.LSList[125], TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LSList[171], TDataType.dtFloat, 120, "0.000000", nfi, true);
			this.AddStatic(LangMan.LSList[172], TDataType.dtFloat, 120, "0.000000", nfi, true);
			this.AddStatic(LangMan.LSList[317], TDataType.dtDateTime, 150, true);
		}

		public TLocationListColumns()
		{
			InitData(typeof(TLocationColumnType));
		}
	}

	public sealed class TLocationListMan : TListManager
	{
		private TGEDCOMLocationRecord FRec;

		public override bool CheckFilter(TShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.LocationName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMLocationRecord);
		}

		protected override object GetColumnValueEx(int col_type, int col_subtype)
		{
			switch (col_type) {
				case 0:
					return this.FRec.LocationName;
				case 1:
					return this.FRec.Map.Lati;
				case 2:
					return this.FRec.Map.Long;
				case 3:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TLocationColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return new TLocationListColumns();
		}

		public TLocationListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
