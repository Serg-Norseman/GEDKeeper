using System;
using System.Globalization;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

/// <summary>
/// 
/// </summary>

namespace GKUI.Lists
{
	public enum LocationColumnType : byte
	{
		lctName,
		lctLati,
		lctLong,
		lctChangeDate
	}

	public sealed class LocationListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			NumberFormatInfo nfi = new NumberFormatInfo();
			nfi.NumberDecimalSeparator = ".";

			this.AddStatic(LangMan.LS(LSID.LSID_Title), TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Latitude), TDataType.dtFloat, 120, "0.000000", nfi, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Longitude), TDataType.dtFloat, 120, "0.000000", nfi, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
		}

		public LocationListColumns()
		{
			InitData(typeof(LocationColumnType));
		}
	}

	public sealed class LocationListMan : ListManager
	{
		private GEDCOMLocationRecord fRec;

		public override bool CheckFilter(ShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.fRec.LocationName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMLocationRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			switch (colType) {
				case 0:
					return this.fRec.LocationName;
				case 1:
					return this.fRec.Map.Lati;
				case 2:
					return this.fRec.Map.Long;
				case 3:
					return this.fRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(LocationColumnType);
		}

		public override ListColumns GetDefaultListColumns()
		{
			return new LocationListColumns();
		}

		public LocationListMan(GEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
