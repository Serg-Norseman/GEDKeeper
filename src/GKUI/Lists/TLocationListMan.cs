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
		protected override void InitDefaultColumns()
		{
			TColumnProps[] array1 = new TColumnProps[4];
			array1[0] = new TColumnProps(TLocationColumnType.lctName, true);
			array1[1] = new TColumnProps(TLocationColumnType.lctLati, true);
			array1[2] = new TColumnProps(TLocationColumnType.lctLong, true);
			array1[3] = new TColumnProps(TLocationColumnType.lctChangeDate, true);
			DefColumns = array1;
		}

		public TLocationListColumns()
		{
			InitData(typeof(TLocationColumnType));
		}
	}

	public sealed class TLocationListMan : TListManager
	{
		private TGEDCOMLocationRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.LocationName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMLocationRecord);
		}

		public override object GetColumnValueEx(int col_index)
		{
			switch (col_index) {
				case 1:
					return this.FRec.LocationName;
				case 2:
					return this.FRec.Map.Lati;
				case 3:
					return this.FRec.Map.Long;
				case 4:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		protected override void InitColumnStatics()
		{
			NumberFormatInfo nfi = new NumberFormatInfo();
			nfi.NumberDecimalSeparator = ".";

			this.ColumnStatics.Clear();
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[125], TDataType.dtString, 300));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[171], TDataType.dtFloat, 120, "0.000000", nfi));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[172], TDataType.dtFloat, 120, "0.000000", nfi));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[317], TDataType.dtDateTime, 150));
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
