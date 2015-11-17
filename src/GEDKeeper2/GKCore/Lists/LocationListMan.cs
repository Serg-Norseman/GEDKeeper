using System;
using System.Globalization;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Lists
{
	/// <summary>
	/// 
	/// </summary>
	public enum LocationColumnType
	{
		lctName,
		lctLati,
		lctLong,
		lctChangeDate
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class LocationListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			NumberFormatInfo nfi = new NumberFormatInfo();
			nfi.NumberDecimalSeparator = ".";

			this.AddStatic(LSID.LSID_Title, DataType.dtString, 300, true);
			this.AddStatic(LSID.LSID_Latitude, DataType.dtFloat, 120, true, "0.000000", nfi);
			this.AddStatic(LSID.LSID_Longitude, DataType.dtFloat, 120, true, "0.000000", nfi);
			this.AddStatic(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
		}

		public LocationListColumns()
		{
			InitData(typeof(LocationColumnType));
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class LocationListMan : ListManager
	{
		private GEDCOMLocationRecord fRec;

		public override bool CheckFilter(ShieldState shieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.fRec.LocationName, this.QuickFilter));

			res = res && base.CheckCommonFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMLocationRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
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

		protected override ListColumns GetDefaultListColumns()
		{
			return new LocationListColumns();
		}

		public LocationListMan(GEDCOMTree tree) : base(tree)
		{
		}
	}
}
