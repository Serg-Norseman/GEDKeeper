using System;
using System.Collections.Generic;
using System.Globalization;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public enum TDataType
	{
		dtString,
		dtInteger,
		dtFloat,
		dtDate,
		dtDateTime
	}

	public enum TConditionKind
	{
		ck_NotEq, ck_LT, ck_LET, ck_Eq, ck_GET, ck_GT, ck_Contains
	}

	public struct TFilterCondition
	{
		public int col_index;

		public Enum column;
		public TConditionKind condition;
		public object value;
	}

	public struct TColumnStatic
	{
		//public byte colType;
		public string colName;
		public TDataType dataType;
		public NumberFormatInfo nfi;
		public string format;
		public int width;

		public TColumnStatic(/*Enum colType*/ string colName, TDataType dataType, int width)
		{
			//this.colType = ((IConvertible)colType).ToByte(null);
			this.colName = colName;
			this.dataType = dataType;
			this.width = width;
			this.nfi = null;
			this.format = null;
		}

		public TColumnStatic(/*Enum colType*/ string colName, TDataType dataType, int width, string format, NumberFormatInfo nfi)
		{
			//this.colType = ((IConvertible)colType).ToByte(null);
			this.colName = colName;
			this.dataType = dataType;
			this.width = width;
			this.nfi = nfi;
			this.format = format;
		}
	}

	public abstract class TListManager : IDisposable
	{
		protected TGEDCOMTree FTree;
		protected bool Disposed_;

		protected List<TColumnStatic> ColumnStatics = new List<TColumnStatic>();

		public string QuickFilter = "*";
		public List<TFilterCondition> ColumnsFilter = new List<TFilterCondition>();

		public TListManager(TGEDCOMTree aTree)
		{
			this.FTree = aTree;

			InitColumnStatics();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Disposed_ = true;
			}
		}

		protected virtual void InitColumnStatics()
		{
			// dummy
		}

		public bool IsMatchesMask(string S, string Mask)
		{
			bool result = false;
			if (S != null && Mask != null && S != "" && Mask != "")
			{
				string stx = S.ToLower();
				string[] masks = Mask.ToLower().Split(new char[] { '|' });

				int num = masks.Length - 1;
				for (int i = 0; i <= num; i++)
				{
					result = (result || TGenEngine.MatchesMask(stx, masks[i]));
				}
			}
			return result;
		}

		public void UpdateTitles(GKListView aList, bool isMain)
		{
			aList.BeginUpdate();
			try
			{
				aList.Columns.Clear();
				this.UpdateColumns(aList, isMain);
			}
			finally
			{
				aList.EndUpdate();
			}
		}

		public abstract bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState);
		public abstract void Fetch(TGEDCOMRecord aRec);

		public virtual string GetColumnValue(int col_index, bool isMain)
		{
			// aColIndex - from 1
			TColumnStatic cs = this.ColumnStatics[col_index - 1];

			object val = GetColumnValueEx(col_index);

			string res = this.ConvColValue(val, cs);

			return res;
		}

		public virtual object GetColumnValueEx(int col_index)
		{
			// aColIndex - from 1
			return null;
		}

		public virtual void InitFilter(TPersonsFilter aFilter)
		{
		}

		public virtual void UpdateItem(GKListItem aItem, bool isMain)
		{
			for (int i = 1; i <= this.ColumnStatics.Count; i++) {
				aItem.SubItems.Add(this.GetColumnValue(i, isMain));
			}
		}

		public virtual void UpdateColumns(GKListView aList, bool isMain)
		{
			aList.AddListColumn("¹", 50, false);

			for (int i = 0; i < this.ColumnStatics.Count; i++) {
				TColumnStatic cs = this.ColumnStatics[i];
				aList.AddListColumn(cs.colName, cs.width, false);
			}
		}

		//

		public string GetColumnName(Enum colType)
		{
			int col = ((IConvertible)colType).ToByte(null);
			if (col >= 0 && col < ColumnStatics.Count) {
				return ColumnStatics[col].colName;
			} else {
				return "<?>";
			}
		}

		public TDataType GetColumnDataType(int index)
		{
			int col = index - 1;
			if (col >= 0 && col < ColumnStatics.Count) {
				return ColumnStatics[col].dataType;
			} else {
				return TDataType.dtString;
			}
		}

		public abstract TListColumns GetDefaultListColumns();

		public abstract Type GetColumnsEnum();

		protected string ConvColValue(object val, TColumnStatic cs)
		{
			switch (cs.dataType) {
				case TDataType.dtString:
					return val.ToString();

				case TDataType.dtInteger:
					return val.ToString();

				case TDataType.dtDate:
					return val.ToString();

				case TDataType.dtDateTime:
					DateTime dtx = ((DateTime)val);
					return ((dtx.Ticks == 0) ? "" : dtx.ToString("yyyy.MM.dd HH:mm:ss", null));

				case TDataType.dtFloat:
					return ((double)val).ToString(cs.format, cs.nfi);
			}

			return val.ToString();
		}

		protected object ConvColStr(string val, TDataType type)
		{
			switch (type) {
				case TDataType.dtString:
					return val;
				case TDataType.dtInteger:
					return SysUtils.ParseInt(val, 0);
				case TDataType.dtFloat:
					return SysUtils.ParseFloat(val, 0.0);
				case TDataType.dtDate:
				case TDataType.dtDateTime:
					return DateTime.Parse(val);
			}
			return val;
		}

		public void AddCondition(Enum column, TConditionKind condition, string value)
		{
			int col = ((IConvertible)column).ToByte(null) + 1;

			TFilterCondition flt_col = new TFilterCondition();
			flt_col.column = column;
			flt_col.col_index = col;
			flt_col.condition = condition;
			flt_col.value = ConvColStr(value, this.GetColumnDataType(col));
			this.ColumnsFilter.Add(flt_col);
		}

		private bool CheckCondition(TFilterCondition fcond)
		{
			bool res = true;

			object dataval = this.GetColumnValueEx(fcond.col_index);

			int comp_res = 0;
			if (fcond.condition != TConditionKind.ck_Contains) {
				comp_res = ((IComparable)dataval).CompareTo(fcond.value);
			}

			switch (fcond.condition) {
				case TConditionKind.ck_NotEq:
					res = comp_res != 0;
					break;
				case TConditionKind.ck_LT:
					res = comp_res < 0;
					break;
				case TConditionKind.ck_LET:
					res = comp_res <= 0;
					break;
				case TConditionKind.ck_Eq:
					res = comp_res == 0;
					break;
				case TConditionKind.ck_GET:
					res = comp_res >= 0;
					break;
				case TConditionKind.ck_GT:
					res = comp_res > 0;
					break;
				case TConditionKind.ck_Contains:
					res = TGenEngine.MatchesMask(dataval.ToString(), "*" + fcond.value.ToString() + "*");
					break;
			}
			return res;
		}

		protected bool CheckNewFilter()
		{
			bool res = true;

			if (this.ColumnsFilter.Count > 0) {
				for (int i = 0; i < this.ColumnsFilter.Count; i++) {
					TFilterCondition fcond = this.ColumnsFilter[i];
					res = res && this.CheckCondition(fcond);
				}
			}

			return res;
		}
	}
}
